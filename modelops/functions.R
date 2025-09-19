library(terra)
library(sf)
library(purrr)
library(furrr)
library(dplyr)
library(openmeteo)
library(lubridate)
library(tidyverse)
library(data.table)

# Step 1: Define southern France bounding box (adjust if needed)
bbox <- st_bbox(c(xmin = -1.74, xmax = 9.44, ymin = 41.422, ymax = 45.5), crs = 4326)

# Step 2: Generate 5 km grid
grid <- st_make_grid(
  st_as_sfc(bbox),
  cellsize = 0.05,
  square = TRUE,
  what = "polygons"
)

grid_sf <- st_sf(geometry = grid)

# Step 1: Compute centroids
centroids <- st_centroid(grid_sf)

# intersect with france to retain only relevant points
france <- st_read("/home/ptaconet/stages/2025_ieva_culicoides/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)

centroids <- st_intersection(centroids,france) %>%
  dplyr::select(geometry)

# Step 2: Extract coordinates into a data frame
coords <- st_coordinates(centroids)

coords = round(coords,3)


coords = as.data.frame(coords)

coords$site <- seq(1:nrow(coords))

meteo_prep <- coords %>%
  group_by(row_number() %/% 100) %>%
  group_map(~.x) %>%
  map(.,~group_split(.,site))


######################################################
######### Téléchargement des données météo
######################################################

meteo <- read.csv("data/raw/meteofrance_2025.csv")
meteo$date <- as.Date(meteo$date)

coords <- meteo %>%
  distinct(X, Y, site)

meteo_prep <- coords %>%
  group_by(row_number() %/% 500) %>%
  group_map(~.x) %>%
  map(.,~group_split(.,site))

## download past data

meteo_past <- data.frame()
meteo_future <- data.frame()

for(i in 1:length(meteo_prep)){

    cat("Dealing with data package",i,"over",length(meteo_prep),"\n")

    th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_history(
      location = c(.$Y, .$X),
      daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
      model = "meteofrance_arome_france_hd",
      start = max(meteo$date),
      end = today() - 1))

    th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y,))

    meteo_past <- rbind(meteo_past, th_res)

    system('sleep 60') # to avoid status code 429 :  Minutely API request limit exceeded.

}


for(i in 1:length(meteo_prep)){

    cat("Dealing with data package",i,"over",length(meteo_prep),"\n")

    th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_forecast(
      location = c(.$Y, .$X),
      daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
      start = today(),
      end = today() + 15))

    th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y,))

    meteo_future <- rbind(meteo_future, th_res)

    system('sleep 60') # to avoid status code 429 :  Minutely API request limit exceeded.

}

# meteo_2024 <- list.files(file.path("data","raw","meteofrance_2024"), full.names = T) %>%
#   purrr::map_dfr(.,~read.csv(.))

meteo <- rbind(meteo,meteo_past)
write.csv(meteo,"data/raw/meteofrance_2025.csv")
meteo <- rbind(meteo, meteo_future)



 meteo_2025_1 <- list.files(file.path("data","raw","meteofrance_2025_january_june"), full.names = T) %>%
   purrr::map_dfr(.,~read.csv(.))

 meteo_2025_2 <- list.files(file.path("data","raw","meteofrance_2025_june_september"), full.names = T) %>%
   purrr::map_dfr(.,~read.csv(.))

 meteo <- fread("data/raw/meteofrance_2025.csv")

######################################################
######### Creation des variables indépendantes
######################################################

meteo <- data.table(meteo)


meteo <- meteo %>%
  unique() %>%
  group_by(X,Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords <- unique(meteo[,c("site","X","Y")])

unique_coords_sf <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

france <- st_read("/home/ptaconet/stages/2025_ieva_culicoides/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)

coords_retain <- st_intersection(unique_coords_sf,france)

coords_retain <- cbind(coords_retain, st_coordinates(coords_retain))
coords_retain <- st_drop_geometry(coords_retain)
coords_retain <- coords_retain[,c("site","X","Y")]

meteo <- meteo %>%
  filter(site %in% coords_retain$site) %>%
  mutate(date = as.Date(date)) %>%
  rename(TM = daily_temperature_2m_mean, RR = daily_precipitation_sum, UM = daily_relative_humidity_2m_mean) %>%
  dplyr::select(site ,date,RR,TM,UM)


lag_max <- 84

meteo2 <- meteo %>%
  dplyr::select(site,date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday==1) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(site, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n) %>%
  data.table()


# summarizing to weeks
meteo3 <- meteo2 %>%
  left_join(meteo, by = c("date","site")) %>%
  pivot_longer(!(site:date), names_to = "var", values_to = 'val') %>%
  data.table()



#### Functions


fun_summarize_week <- function(meteo3,var_to_summarize,fun_summarize,new_var_name,n_days_agg){

  if(fun_summarize=="sum"){
    # meteo3_summarize <- meteo3 %>%
    #   filter(var==var_to_summarize) %>%
    #   mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
    #   group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
    #   dplyr::summarise(val=sum(val, na.rm = T),date = max(date)) %>%
    #   group_by(site, th_date) %>%
    #   mutate(lag_n=seq(0,n()-1,1)) %>%
    #   mutate(var = new_var_name) %>%
    #   as_tibble() %>%
    #   dplyr::select(-year)

    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = sum(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]


  } else if (fun_summarize == "mean"){
    # meteo3_summarize <- meteo3 %>%
    #   filter(var==var_to_summarize) %>%
    #   mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
    #   group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
    #   summarise(val=mean(val, na.rm = T),date = max(date)) %>%
    #   group_by(site, th_date) %>%
    #   mutate(lag_n=seq(0,n()-1,1)) %>%
    #   mutate(var = new_var_name) %>%
    #   as_tibble() %>%
    #   dplyr::select(-year)

    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = mean(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]

  }  else if (fun_summarize == "max"){
    # meteo3_summarize <- meteo3 %>%
    #   filter(var==var_to_summarize) %>%
    #   mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
    #   group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
    #   summarise(val=max(val, na.rm = T),date = max(date)) %>%
    #   group_by(site, th_date) %>%
    #   mutate(lag_n=seq(0,n()-1,1)) %>%
    #   mutate(var = new_var_name) %>%
    #   as_tibble() %>%
    #   dplyr::select(-year)

    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = max(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]

  }  else if (fun_summarize == "min"){
    # meteo3_summarize <- meteo3 %>%
    #   filter(var==var_to_summarize) %>%
    #   mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
    #   group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
    #   summarise(val=min(val, na.rm = T),date = max(date)) %>%
    #   group_by(site, th_date) %>%
    #   mutate(lag_n=seq(0,n()-1,1)) %>%
    #   mutate(var = new_var_name) %>%
    #   as_tibble() %>%
    #   dplyr::select(-year)

    meteo3_summarize <- meteo3[var == var_to_summarize][
      , lag_n := floor(lag_n / n_days_agg)][
        , year := year(date)][  # lubridate::year assumed loaded
          , .(val = min(val, na.rm = TRUE), date = max(date)),
          by = .(site, th_date, lag_n, year)][
            , lag_n := seq(0, .N - 1), by = .(site, th_date)][
              , var := new_var_name][
                , year := NULL]
  }

  meteo3_summarize <- data.table(meteo3_summarize)

  return(meteo3_summarize)

}

df_meteo_pieges_summ <- fun_summarize_week(meteo3,"RR","sum","RR",7) %>%
  bind_rows(fun_summarize_week(meteo3,"TM","mean","TM",7)) %>%
  bind_rows(fun_summarize_week(meteo3,"UM","mean","UM",7))

## je ne sais pas pourquoi cela va jusque 8 parfois, mais on enleve pour avoir 7 (0+7, donc 8 en tout) semaines
df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n<12)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){

  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))

  max_col <- ncol(df_timeseries_wide)

  for(i in 3:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="max"){
        df_timeseries_wide[column_name] <- max(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="min"){
        df_timeseries_wide[column_name] <- min(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }

  for(i in 3:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)

}



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RR","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")


df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)


df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site,th_date,TM_0_8,UM_5_11,TM_0_4, UM_0_11,RR_1_5) %>%
  rename(date=th_date) %>%
  na.omit(.)

######################################################
######### Génération des prédictions
######################################################

multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting.rds")


df_meteo_predictions$pred_presence_prob <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions, type = 'prob')$Presence
df_meteo_predictions$pred_presence_absence <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions)

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
  mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,.))))

df_meteo_predictions <- df_meteo_predictions %>%
  left_join(coords_retain)


######################################################
######### Rasterisation
######################################################

# to create a regular grid (evenly spaced)
grid_res <- 0.05

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  )

res_x <- min(diff(sort(unique(df_meteo_predictions$X_snap))))
res_y <- min(diff(sort(unique(df_meteo_predictions$Y_snap))))

# Compute raster extent from centers
xmin <- min(df_meteo_predictions$X_snap) - res_x / 2
xmax <- max(df_meteo_predictions$X_snap) + res_x / 2
ymin <- min(df_meteo_predictions$Y_snap) - res_y / 2
ymax <- max(df_meteo_predictions$Y_snap) + res_y / 2

# Create template raster
r_template <- rast(
  extent = c(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "EPSG:4326"
)

# Make sure date is character for naming
df_meteo_predictions$date <- as.character(df_meteo_predictions$date)

# Convert to SpatVector
v <- vect(df_meteo_predictions, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")

# Unique dates
dates <- unique(df_meteo_predictions$date)

# Rasterize each date separately
rasters <- lapply(dates, function(d) {
  v_d <- v[v$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "pred_abundance", fun = "mean")
  names(r) <- d
  #d = gsub("-","",d)
  #writeRaster(r,paste0("data/raw/results_raster_2024/aedesabundance_",d,".tif"))
  r

})

# Combine into a single multilayer raster
r_stack <- rast(rasters)


### create isobands
library(isoraster)

isobands <- lapply(rasters, function(r) {
  i=isoraster::isoband_terra(r,  c(0, 0.05, 10, 20, 30, 40, 50), c(0.05, 10, 20, 30, 40, 50, 60))
  i <- sf::st_as_sf(i)
  i$date <- as.Date(names(r))
  i
})

isobands <- do.call("rbind", isobands)
isobands <- isobands %>%
  filter(!st_is_empty(.)) %>%
  st_transform(2154) %>%
  mutate(fid = seq(1,nrow(.),1)) %>%
  relocate(fid, 1)

######################################################
######### Aggrégation au departement et à la commune pour les prédictions entomo
######################################################

communes_precis <- st_read("modelops/data/raw/France_albopictus_year_colonization.shp")
departements_precis <- st_read("modelops/data/raw/departements-20180101.shp") %>% dplyr::filter(!(code_insee %in% c("974","976","972","973","971")))

departements <- st_read("modelops/data/raw/departements_simplified.gpkg")
communes <- st_read("modelops/data/raw/communes_simplified.gpkg")

library(exactextractr)

communes_mean_abundance <- exact_extract(r_stack, communes, c('mean')) %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, date, mean_abundance) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_abundance))

communes_sd_abundance <- exact_extract(r_stack, communes, 'stdev') %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("stdev"),names_to = "date",values_to = "sd_abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("stdev-","",date)) %>%
  dplyr::select(codgeo, date, sd_abundance) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(sd_abundance))

communes_abundance <- communes_mean_abundance %>%
  mutate(sd_abundance = communes_sd_abundance$sd_abundance) %>%
  mutate(date = as.character(date))

communes_abundance <- communes %>%
  left_join(communes_abundance, by = "codgeo") %>%
  mutate(date=as.Date(date)) %>%
  filter(!is.na(mean_abundance)) %>%
  mutate(mean_abundance = round(mean_abundance,1), sd_abundance = round(sd_abundance,1))

## TODO : reperer les éventuelles erreurs

departements_mean_abundance <- exact_extract(r_stack, departements, 'mean') %>%
  bind_cols(departements) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(dep, date, mean_abundance) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_abundance))

departements_sd_abundance <- exact_extract(r_stack, departements, 'stdev') %>%
  bind_cols(departements) %>%
  pivot_longer(cols = starts_with("stdev"),names_to = "date",values_to = "sd_abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("stdev-","",date)) %>%
  dplyr::select(dep, date, sd_abundance) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(sd_abundance))


departements_abundance <- departements_mean_abundance %>%
  mutate(sd_abundance = departements_sd_abundance$sd_abundance) %>%
  mutate(date = as.character(date))

departements_abundance <- departements %>%
  left_join(departements_abundance, by = "dep") %>%
  mutate(date=as.Date(date)) %>%
  filter(!is.na(mean_abundance)) %>%
  mutate(mean_abundance = round(mean_abundance,1), sd_abundance = round(sd_abundance,1))

#ggplot(departements_abundance, aes(x = date, y = mean_abundance)) + geom_line() + facet_wrap(.~departement) + theme_bw()

#st_write(departements_abundance,"modelops/data/predictions/pred_albo_departements.gpkg", append = FALSE)
#st_write(communes_abundance,"modelops/data/predictions/pred_albo_communes.gpkg", append = FALSE)




######################################################
######### Aggrégation au departement et à la commune pour les données météo
######################################################

meteo <- df_meteo_pieges_summ %>%
  filter(lag_n == 0) %>%
  dplyr::select(-c("th_date","lag_n")) %>%
  pivot_wider(names_from  = var, values_from = val) %>%
  left_join(coords_retain) %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  ) %>%
  mutate(date = as.character(date))

res_x <- min(diff(sort(unique(meteo$X_snap))))
res_y <- min(diff(sort(unique(meteo$Y_snap))))

# Compute raster extent from centers
xmin <- min(meteo$X_snap) - res_x / 2
xmax <- max(meteo$X_snap) + res_x / 2
ymin <- min(meteo$Y_snap) - res_y / 2
ymax <- max(meteo$Y_snap) + res_y / 2

# Create template raster
r_template <- rast(
  extent = c(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "EPSG:4326"
)

# Convert to SpatVector
v_meteo <- vect(meteo, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")

# Unique dates
dates <- unique(meteo$date)


# Rasterize each date separately
rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "TM", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_tm <- rast(rasters)


rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "UM", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_um <- rast(rasters)


rasters <- lapply(dates, function(d) {
  v_d <- v_meteo[v_meteo$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "RR", fun = "mean")
  names(r) <- d
  r
})

# Combine into a single multilayer raster
r_meteo_rr <- rast(rasters)



library(exactextractr)

communes_mean_temperature <- exact_extract(r_meteo_tm, communes, c('mean')) %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_temperature") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, date, mean_temperature) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_temperature)) %>%
  mutate(mean_temperature=round(mean_temperature,1))


communes_mean_rainfall <- exact_extract(r_meteo_rr, communes, c('mean')) %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_rainfall") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, date, mean_rainfall) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_rainfall))  %>%
  mutate(mean_rainfall=round(mean_rainfall,1))


communes_mean_humidity <- exact_extract(r_meteo_um, communes, c('mean')) %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_humidity") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, date, mean_humidity) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_humidity)) %>%
  mutate(mean_humidity=round(mean_humidity,1))


meteo <- left_join(communes_mean_temperature,communes_mean_rainfall)
meteo <- left_join(meteo,communes_mean_humidity)

######################################################
######### Publication dans postgis (sorties des modèles et données météo)
######################################################

departements_abundance <- departements_abundance %>%
  dplyr::select(dep,date,mean_abundance,sd_abundance) %>%
  st_drop_geometry()

communes_abundance <- communes_abundance %>%
  dplyr::select(codgeo, date, mean_abundance, sd_abundance) %>%
  st_drop_geometry()



library(DBI)

con <- dbConnect(
  RPostgres::Postgres(),
  host = "postgresql-taconet.alwaysdata.net",
  dbname = "taconet_albopictus",
  port = 5432,
  user = "taconet",
  password = "HHKcue51"
)

st_write(departements_abundance, dsn = con, layer = "albopictus_abondance_departements",append = FALSE)
st_write(communes_abundance, dsn = con, layer = "albopictus_abondance_communes",append = FALSE)

# DBI:::dbSendQuery(con,'create view albopictus_abondance_departements_geo AS select b.libgeo, b.dep, a.date, a.mean_abundance, a.sd_abundance , b.reg,  b.geometry from albopictus_abondance_departements a left join departements b ON a.dep = b.dep')
# DBI:::dbSendQuery(con,'create view albopictus_abondance_communes_geo AS select  b.libgeo, a.date, a.mean_abundance, a.sd_abundance, b.geometry from albopictus_abondance_communes a left join communes b ON a.codgeo = b.codgeo')

st_write(isobands, dsn = con, layer = "albopictus_abondance_isobands",append = FALSE)


st_write(meteo, dsn = con, layer = "albopictus_meteo_communes",append = FALSE)
# DBI:::dbSendQuery(con,'create view albopictus_meteo_communes_geo AS select  b.libgeo, a.date, a.mean_temperature, a.mean_rainfall, a.mean_humidity, b.geometry from albopictus_meteo_communes a left join communes b ON a.codgeo = b.codgeo')

######################################################
######### Publication dans geoserver
######################################################

#
# library(geosapi)
#
# setwd(file.path("modelops","data","predictions"))
#
# GSman <- GSManager$new(
#   url = "https://geodata.bac-a-sable.inrae.fr/geoserver",
#   user = "omees", pwd = "HHKcue51!HHKcue51!",
#   logger = 'DEBUG'
# )
#
#
# couches <- list.files()
# couches <- gsub("\\.gpkg","",couches)
#
# for(i in 1:length(couches)){
#
#   ds = GSGeoPackageDataStore$new(name=couches[[i]],
#                                  description = "Aedes albopictus predictions layer",
#                                  enabled = TRUE,
#                                  database = paste0("/mnt/geoserver_geodata/",couches[[i]],".gpkg"))
#
#   created <- GSman$createDataStore("omees", ds)
#
#   uploaded <- GSman$uploadGeoPackage(
#     ws = "omees", ds = couches[[i]],
#     endpoint = "file", configure = "first", update = "overwrite",
#     charset = "UTF-8", filename = paste0(couches[[i]],".gpkg")
#   )
#
# }
#

