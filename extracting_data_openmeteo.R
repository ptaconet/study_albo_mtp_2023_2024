library(terra)
library(sf)
library(purrr)
library(furrr)
library(dplyr)
library(openmeteo)
library(lubridate)
library(tidyverse)

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

# what data have already been downloaded ?

already_dl <- as.numeric(list.files("data/raw/meteofrance_2025_june_september"))




for(i in 1:length(meteo_prep)){

  if(!(i %in% already_dl)){

  cat("Dealing with data package",i,"over",length(meteo_prep),"\n")

    # for year 2023
    # th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_history(
    #   location = c(.$Y, .$X),
    #   daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
    #   start = as.Date("2023-01-01"),
    #   end = as.Date("2023-12-31")))

    # for year 2024 and 2025 :
   th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_history(
     location = c(.$Y, .$X),
     daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
     model = "meteofrance_arome_france_hd",
     start = as.Date("2025-06-06"),
     end = as.Date("2025-09-01")))

  th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y,))

  write.csv(th_res,paste0("data/raw/meteofrance_2025_june_september/",i), row.names = F)

  system('sleep 60') # to avoid status code 429 :  Minutely API request limit exceeded.

  }

}


# forecast data

meteo_prep <- coords %>%
  group_by(row_number() %/% 300) %>%
  group_map(~.x) %>%
  map(.,~group_split(.,site))

already_dl <- as.numeric(list.files("data/raw/meteofrance_2025_forecast"))


for(i in 1:length(meteo_prep)){

  if(!(i %in% already_dl)){

    cat("Dealing with data package",i,"over",length(meteo_prep),"\n")

    th_meteo <- map(meteo_prep[[i]], ~openmeteo::weather_forecast(
      location = c(.$Y, .$X),
      daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
      start = today(),
      end = today() + 15))

    th_res <- map2_dfr(meteo_prep[[i]], th_meteo, ~bind_cols(.x, .y,))

    write.csv(th_res,paste0("data/raw/meteofrance_2025_forecast/",i), row.names = F)

    system('sleep 60') # to avoid status code 429 :  Minutely API request limit exceeded.

  }

}


meteo_past <- list.files(file.path("data","raw","meteofrance_2025_january_june"), full.names = T) %>%
  purrr::map_dfr(.,~read.csv(.))

meteo_future <- list.files(file.path("data","raw","meteofrance_2025_forecast"), full.names = T) %>%
  purrr::map_dfr(.,~read.csv(.)) %>%
  filter(as.Date(date) != today() - 2)

meteo <- rbind(meteo_past,meteo_future)


meteo <- list.files(file.path("data","raw","meteofrance_2024"), full.names = T) %>%
  purrr::map_dfr(.,~read.csv(.))



library(data.table)
meteo <- data.table(meteo)


meteo <- meteo %>%
  unique() %>%
  group_by(X,Y) %>%
  mutate(site = cur_group_id()) %>%
  ungroup() %>%
  relocate(site, 1) %>%
  data.table()

unique_coords <- unique(meteo[,c("site","X","Y")])

unique_coords <- st_as_sf(unique_coords, coords = c("X", "Y"), crs = 4326)

france <- st_read("/home/ptaconet/stages/2025_ieva_culicoides/france_ecoclimatic_zones.gpkg")
france <- st_transform(france,4326)

coords_retain <- st_intersection(unique_coords,france)

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



multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting.rds")


df_meteo_predictions$pred_presence_prob <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions, type = 'prob')$Presence
df_meteo_predictions$pred_presence_absence <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions)

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
  mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,.))))

df_meteo_predictions <- df_meteo_predictions %>%
  left_join(coords_retain)

# communes avec Albo

france_albo <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/shp_alto_colonization/transfer_9935526_files_4caef5ee/France_albopictus_year_colonization.shp")
france_albo <- st_transform(france_albo,4326)
france_albo <- france_albo %>% filter(presence == 1)

### plot non animate - vector
ggplot()+
  #geom_sf(data=france, color="cornsilk4", linewidth=0, alpha=1) +
  geom_tile(data = df_meteo_predictions, aes(x = X, y = Y, fill=pred_abundance)) +
  #geom_sf(data=st_centroid(france_albo), size = 0.005) +
  facet_wrap(~date)+
  scale_fill_gradientn(
    colours = c("blue", "white", "red"),  # 0 = blue, high = red
    values = scales::rescale(c(0, 0.001, max(df_meteo_predictions$pred_abundance, na.rm = TRUE))),
    limits = c(0, max(df_meteo_predictions$pred_abundance, na.rm = TRUE))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  guides(fill=guide_legend(title="Predicted abundance"))



## plot animate - vector
library(gganimate)
p <- ggplot() +
  geom_sf(data = france, color = "cornsilk4", linewidth = 0.3, fill = NA) +
  geom_tile(
    data = df_meteo_predictions,
    aes(x = X, y = Y, fill = pred_abundance)
  ) +
  scale_fill_gradientn(
    colours = c("blue", "white", "red"),  # 0 = blue, high = red
    values = scales::rescale(c(0, 0.001, max(df_meteo_predictions$pred_abundance, na.rm = TRUE))),
    limits = c(0, max(df_meteo_predictions$pred_abundance, na.rm = TRUE)),
    oob = scales::squish
  ) +
  coord_sf(default_crs = NULL) +
  theme_minimal() +
  labs(title = "Predicted presence probability – {closest_state}") +
  transition_states(
    date,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("linear")

animate(p, width = 800, height = 600, fps = 2, duration = 20, renderer = gifski_renderer("prediction_animation.gif"))


## rasterize

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

#plot
plot(r_stack, col = c("orange","steelblue"), legend = TRUE)


# v1 (non-geo)
departements <- st_read("data/admin_data/departements-20180101-shp/departements-20180101.shp")
departements$ID <- seq(1:nrow(departements))

names(r_stack) <- gsub("-","",names(r_stack))
names(r_stack) <- paste0("date_",names(r_stack))
mean <- terra::extract(r_stack,departements, fun = "mean", na.rm = T)
sd <- terra::extract(r_stack,departements, fun = "sd", na.rm = T)

mean <- mean %>% left_join(departements) %>% dplyr::select(date_20250209:nom) %>% dplyr::select(-code_insee) %>% pivot_longer(-nom) %>% dplyr::filter(!is.nan(value)) %>% rename(mean = value, date = name)
sd <- sd %>% left_join(departements) %>% dplyr::select(date_20250209:nom) %>% dplyr::select(-code_insee) %>% pivot_longer(-nom) %>% dplyr::filter(!is.nan(value)) %>% rename(sd = value, date = name)

all <- full_join(mean,sd) %>%
  mutate(date = gsub("date_","",date)) %>%
  mutate(date = as.Date(as.character(date),format="%Y%m%d")) %>%
  rename(departement = nom)

dates_canicule <- data.frame(debut = as.Date(c("2025-06-27","2025-08-08")), fin = as.Date(c("2025-07-05","2025-08-18")))

ggplot(all, aes(x = date, y = mean)) + geom_line() + facet_wrap(.~departement) + theme_bw() +
  geom_rect(
  data = dates_canicule,
  aes(xmin = debut, xmax = fin, ymin = -Inf, ymax = Inf),
  inherit.aes = FALSE,
  alpha = 0.2,
  fill = "red"
) +
  scale_x_date(date_breaks="1 month", date_labels="%B") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab("mean egg abundance")

# v2 (geo)

library(exactextractr)

communes_abundance <- exact_extract(r_stack, france_albo, 'mean')
france_albo <- cbind(france_albo,communes_abundance)
plot(france_albo, lwd = 0.2)


departements <- st_read("data/admin_data/departements-20180101-shp/departements-20180101.shp") %>% dplyr::filter(!(code_insee %in% c("974","976","972","973","971")))
departements_abundance <- exact_extract(r_stack, departements, 'mean')
departements_albo <- cbind(departements,departements_abundance)
plot(departements_albo["mean.2025.06.08"], lwd = 0.3)




ab_communes <- france_albo %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(commune, date, abundance, geometry) %>%
  mutate(date = as.Date(date))

ab_departements_albo <- departements_albo %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(nom, date, abundance, geometry) %>%
  filter(!is.nan(abundance)) %>%
  mutate(date = as.Date(date))


st_write(ab_communes,"aedes_abundance_communes.gpkg")
st_write(ab_departements_albo,"aedes_abundance_departements.gpkg", append = FALSE)


library(RSQLite)
a <- dbConnect(RSQLite::SQLite(),"aedes_abundance_departements.gpkg")
dbSendQuery(a,"VACUUM")
dbDisconnect(a)



## sorties Arbocarto
load("/home/ptaconet/Téléchargements/2025-06-05_arbocartoR_rawresults.rda")
iris <- st_read("/home/ptaconet/Téléchargements/georef-herault-iris.geojson") %>% dplyr::select(code_officiel_iris,nom_officiel_iris)

a <- trajectories[[1]] %>%
  mutate(ID = as.numeric(ID)) %>%
  data.frame() %>%
  left_join(iris, by = c("ID"="code_officiel_iris")) %>%
  st_as_sf() %>%
  mutate(DATE = as.Date(DATE))

  a <- a %>%
    mutate(DATE = gsub("2021","2024",DATE)) %>%
    mutate(DATE = as.Date(DATE)) %>%
    filter(DATE %in% unique(b$date))

st_write(a,"aedes_abundance_iris.gpkg", append = F)
a <- dbConnect(RSQLite::SQLite(),"aedes_abundance_iris.gpkg")
dbSendQuery(a,"VACUUM")
dbDisconnect(a)





#### LIME analysis
library(lime)

coords_sf<- st_as_sf(coords,coords = c("X", "Y"), crs = 4326 )
coords_departement <- st_intersection(coords_sf,departements)

meteo <- df_meteo_pieges_summ_wide_meteofrance %>%
  left_join(coords_departement) %>%
  rename(date=th_date) %>%
  st_drop_geometry() %>%
  dplyr::select(site,date,nom,TM_0_8,UM_5_11,TM_0_4, UM_0_11,RR_1_5) %>%
  group_by(nom,date) %>%
  summarise(TM_0_8 = mean(TM_0_8, na.rm = T),UM_5_11 = mean(UM_5_11, na.rm = T), TM_0_4 = mean(TM_0_4, na.rm = T), UM_0_11 = mean(UM_0_11, na.rm = T), RR_1_5 = mean(RR_1_5, na.rm = T)) %>%
  ungroup()

ggplot(meteo, aes(x=date, y = TM_0_4)) + geom_line() + facet_wrap(.~nom)

df_mod_presence_nowcasting <- multiv_model_presence_nowcasting[[3]] #### data frame which was used to build the model
df_mod_abundance_nowcasting <- multiv_model_abundance_nowcasting[[3]] #### data frame which was used to build the model

df_mod_presence_nowcasting_lime <- df_mod_presence_nowcasting %>% dplyr::select(TM_0_8,UM_5_11)
explainer_presence <- lime(df_mod_presence_nowcasting_lime, multiv_model_presence_nowcasting[[1]], n_bins = 6)

df_mod_abundance_nowcasting_lime <- df_mod_abundance_nowcasting %>% dplyr::select(TM_0_4, UM_0_11, RR_1_5)
explainer_abundance <- lime(df_mod_abundance_nowcasting_lime, multiv_model_abundance_nowcasting[[1]], n_bins = 6)

explanation_presence <- explain(
  x = meteo %>% dplyr::select(TM_0_8,UM_5_11),
  explainer = explainer_presence,
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = NULL,
  n_features = 10,
  feature_select = "highest_weights",
  labels = "Presence"
)

x_presence <- meteo %>%
  slice(rep(1:n(), each = 2)) %>%
  dplyr::select(nom, date)

explanation_presence <- explanation_presence %>%
  bind_cols(x_presence) %>%
  mutate(model = "presence")


explanation_abundance <- explain(
  x = meteo  %>% dplyr::select(TM_0_4, UM_0_11, RR_1_5),
  explainer = explainer_abundance,
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = NULL,
  n_features = 10,
  feature_select = "highest_weights")

x_abundance <- meteo %>%
  slice(rep(1:n(), each = 3)) %>%
  dplyr::select(nom, date)

explanation_abundance <- explanation_abundance %>%
  bind_cols(x_abundance) %>%
  mutate(model = "abundance")


# explanation_presence <- explanation_presence %>% filter(explanation_presence$label_prob<0.5)
#
# explanation_abundance <- explanation_abundance %>%
#   filter(!(date %in% unique(explanation_presence$date)))

explanation_tot <- rbind(explanation_presence[,c("nom","date","model","feature","feature_value","feature_weight","feature_desc")],
                         explanation_abundance[,c("nom","date","model","feature","feature_value","feature_weight","feature_desc")])


explanation <- explanation_tot %>%
  rename(site = nom) %>%
  mutate(feature_char = case_when(grepl("UM", feature) ~ "Humidity",
                                  grepl("TM", feature) ~ "Temperature",
                                  grepl("RR", feature) ~ "Rainfall")) %>%
  #mutate(feature_char = fct_relevel(feature_char, rev(c("Temperature","Rainfall","Humidity","Wind","site")))) %>%
  mutate(feature_char = fct_relevel(feature_char, c("Temperature","Rainfall","Humidity"))) %>%
  filter(feature_char!="site") %>%
  mutate(feature_weight = ifelse(feature_weight < -2.5, -2.5 ,feature_weight))

explanation$feature_desc <- factor(explanation$feature_desc,levels = unique(explanation$feature_desc[order(as.numeric(explanation$feature_value))]))



p1 <- ggplot(explanation %>% filter(site == "Alpes-Maritimes"), aes_(~date, ~feature_desc)) +
  geom_tile(aes_(fill = ~feature_weight)) +
  scale_y_discrete("Feature",expand = c(0, 0)) +
  #scale_fill_gradient2("Feature weight",low = "firebrick", mid = "#f7f7f7", high = "steelblue",  limit = c(-2,2.5), n.breaks = 5, labels = c("Important - reduce","","no weight","","Important - raises")) +
  scale_fill_gradientn(
    name = "Feature contribution",
    colours = c("firebrick", "#f7f7f7", "steelblue"),
    values = scales::rescale(c(-3.5, -0.75, 0, 0.75, 3.5)),  # non-linear steps
    limits = c(-3.5, 3.5),
    breaks = c(-2, -0.4, 0, 0.4, 2),
    labels = c("Important - negative", "", "no contribution", "", "Important - positive")
  ) +
  theme_light() +
  facet_grid(rows = vars(feature_char), space="free", scales = "free_y") +
  theme(panel.border = element_rect(fill = NA,colour = "grey60", size = 0.5),
        #panel.grid = element_blank(),
        legend.position = "right",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        #axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text=element_text(size=9),
        text = element_text(size=10),
        plot.margin = margin(5, 5, 5, 5),
        strip.text = element_text(size = rel(1.2))
  ) +
  scale_x_date(limits = c(as.Date("2025-01-01"),as.Date("2025-08-31")),
               breaks = seq(as.Date("2025-01-01"), as.Date("2025-09-01"), by = "1 month"),
               minor_breaks = seq(as.Date("2025-01-01"), as.Date("2025-01-01"), by = "1 month"),
               date_labels = "%Y-%m")


## Via API meteo france  https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=131&id_rubrique=51

library(httr2)

apikey_mf = "eyJ4NXQiOiJZV0kxTTJZNE1qWTNOemsyTkRZeU5XTTRPV014TXpjek1UVmhNbU14T1RSa09ETXlOVEE0Tnc9PSIsImtpZCI6ImdhdGV3YXlfY2VydGlmaWNhdGVfYWxpYXMiLCJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJwdGFjb25ldEBjYXJib24uc3VwZXIiLCJhcHBsaWNhdGlvbiI6eyJvd25lciI6InB0YWNvbmV0IiwidGllclF1b3RhVHlwZSI6bnVsbCwidGllciI6IlVubGltaXRlZCIsIm5hbWUiOiJEZWZhdWx0QXBwbGljYXRpb24iLCJpZCI6Mjc3MTksInV1aWQiOiIzZjZlZWQyOC01MjZjLTQ4NjgtYWE0OC00OWI2ZDU2ZmE0M2EifSwiaXNzIjoiaHR0cHM6XC9cL3BvcnRhaWwtYXBpLm1ldGVvZnJhbmNlLmZyOjQ0M1wvb2F1dGgyXC90b2tlbiIsInRpZXJJbmZvIjp7IjUwUGVyTWluIjp7InRpZXJRdW90YVR5cGUiOiJyZXF1ZXN0Q291bnQiLCJncmFwaFFMTWF4Q29tcGxleGl0eSI6MCwiZ3JhcGhRTE1heERlcHRoIjowLCJzdG9wT25RdW90YVJlYWNoIjp0cnVlLCJzcGlrZUFycmVzdExpbWl0IjowLCJzcGlrZUFycmVzdFVuaXQiOiJzZWMifX0sImtleXR5cGUiOiJQUk9EVUNUSU9OIiwic3Vic2NyaWJlZEFQSXMiOlt7InN1YnNjcmliZXJUZW5hbnREb21haW4iOiJjYXJib24uc3VwZXIiLCJuYW1lIjoiQVJPTUUiLCJjb250ZXh0IjoiXC9wdWJsaWNcL2Fyb21lXC8xLjAiLCJwdWJsaXNoZXIiOiJhZG1pbl9tZiIsInZlcnNpb24iOiIxLjAiLCJzdWJzY3JpcHRpb25UaWVyIjoiNTBQZXJNaW4ifV0sImV4cCI6MTc0ODI3MjIzNSwidG9rZW5fdHlwZSI6ImFwaUtleSIsImlhdCI6MTc0ODI3MTIzNSwianRpIjoiZmEyNzczYjMtNDgwZi00NjdmLTkyMmItYThkODQ2ZjYyYjQ2In0=.hiBw-TF_SnHh9YuTdqjcm13xFtgb_qtrtdTmj35TxDrqogMqNeS8GB1kduasLup7DfM6vt3E6xZujEiKQsNrA99ZKS6gRupg906nDtsz9jNFesYdjeltTKAc52UKu7xcEvUy2azF6EiKChIs67vb1WT2FG1JC6yV4tqeb9dR9IzPdnqRFqYGQXzGnz01Sscc-tazXoG9BNxt7s_iYQd4NAIit3MWwcezGIl3h__g8HOvXyvhKUQQ8ck_C1MlH3dOJcckuDtIjbdgItMBhxQNtxI04Zwovr0rhuwuU3Y2HjXoIh2paw491Yts9J1rNksjawRrzNla64kHrcCNz3lc7w=="

dates <- c("2025-05-22T00:00:00","2025-05-22T03:00:00","2025-05-22T06:00:00","2025-05-22T09:00:00","2025-05-22T12:00:00","2025-05-22T15:00:00","2025-05-22T18:00:00","2025-05-22T21:00:00")

var = "TEMPERATURE__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND" # TOTAL_WATER_PRECIPITATION__GROUND_OR_WATER_SURFACE #RELATIVE_HUMIDITY__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND

for(i in 1:length(dates)){

 url <- paste0("https://public-api.meteofrance.fr/public/arome/1.0/wcs/MF-NWP-HIGHRES-AROME-0025-FRANCE-WCS/GetCoverage?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=TEMPERATURE__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND___",dates[i],"Z&subset=time(",dates[i],"Z)&subset=lat(41,51)&subset=long(-5.3,9.6)&subset=height(2)")
 #url <- paste0("https://public-api.meteofrance.fr/public/arome/1.0/wcs/MF-NWP-HIGHRES-AROME-001-FRANCE-WCS/GetCoverage?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=TOTAL_WATER_PRECIPITATION__GROUND_OR_WATER_SURFACE___",dates[i],"Z&subset=time(",dates[i],"Z)&subset=lat(41,51)&subset=long(-5.3,9.6)")
 #url <- paste0("https://public-api.meteofrance.fr/public/arome/1.0/wcs/MF-NWP-HIGHRES-AROME-001-FRANCE-WCS/GetCoverage?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&coverageId=RELATIVE_HUMIDITY__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND___",dates[i],"Z&subset=time(",dates[i],"Z)&subset=lat(41,51)&subset=long(-5.3,9.6)&subset=height(2)")

response <- request(url) %>%
  req_url_query(apikey = apikey_mf) %>%
  req_perform()

# Save binary response directly
writeBin(resp_body_raw(response), paste0("temperature_025",dates[i],".tif"))

}


#### comparison with year 2025
pieges_data <- read.csv(file.path("data","processed","pieges_data_2021_2025.csv")) %>%
  mutate(date=as.Date(date_releve_jour, format = "%d/%m/%y")) %>%
  mutate(effectif_jour = gsub(",",".",effectif_jour)) %>%
  mutate(effectif_jour=as.numeric(effectif_jour)) %>%
  filter(date>"2025-02-09") %>%
  dplyr::select(nom_commune, date, Semaine_releve, effectif_jour ) %>%
  rename(commune = nom_commune, observed = effectif_jour) %>%
  group_by(commune, Semaine_releve) %>%
  summarise(observed=mean(observed)) %>%
  ungroup()

 read.csv(file.path("data","processed","pieges_data_2021_2025.csv")) %>%
  mutate(date=as.Date(date_releve_jour, format = "%d/%m/%y")) %>%
  mutate(effectif_jour = gsub(",",".",effectif_jour)) %>%
  mutate(effectif_jour=as.numeric(effectif_jour)) %>%
  dplyr::select(nom_commune, date, Semaine_releve, effectif_jour,  annee) %>%
  rename(commune = nom_commune, observed = effectif_jour) %>%
   mutate(annee = as.factor(annee)) %>%
  group_by(commune, Semaine_releve, annee) %>%
  summarise(observed=mean(observed, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x=Semaine_releve, y = observed, group = annee, color = annee)) +
  geom_line() +
  facet_wrap(.~commune)



ab_communes = ab_communes %>%
  st_drop_geometry() %>%
  filter(commune %in% c("Pérols","Saint-médard-en-jalles","Murviel-lès-Montpellier","Bayonne")) %>%
  mutate(commune = case_when(commune == "Pérols"~"PEROLS",
                             commune == "Saint-médard-en-jalles"~"SAINT-MEDARD-EN-JALLES",
                             commune == "Murviel-lès-Montpellier"~"MURVIEL-LES-MONTPELLIER",
                             commune == "Bayonne"~"BAYONNE")) %>%
  mutate(Semaine_releve = lubridate::week(date)) %>%
  rename(predicted = abundance) %>%
  filter(Semaine_releve<=23)


 ab_communes %>%
  left_join(pieges_data, by = c("commune","Semaine_releve")) %>%
  pivot_longer(c("predicted","observed")) %>%
  filter(Semaine_releve<=23) %>%
  ggplot(aes(x=Semaine_releve, y = value, group = name, color = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~commune)
