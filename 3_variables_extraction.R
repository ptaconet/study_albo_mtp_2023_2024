library(tidyverse)
library(furrr)


pieges_data <- read.csv( file.path("data","processed","df_pieges.csv")) %>%
  filter(!is.na(date_releve)) %>%
  mutate(num_releve = seq(1,nrow(.),1)) %>%
  mutate(idpointdecapture = paste0(num_piege,"_",num_releve))

sites = unique(pieges_data$site)

pieges_location <- pieges_data %>%
  group_by(num_piege, site) %>%
  summarise(Latitude=mean(Latitude, na.rm = T), Longitude = mean(Longitude, na.rm = T))


### ### ### ### ### ### ### ###
### Variables météorologiques
### ### ### ### ### ### ### ###

meteo <- read.csv(file.path("data","processed","data_meteofrance_2022_2024.csv")) %>%
  rename(site = nom_commune) %>%
  mutate(date = as.Date(date)) %>%
  mutate(TN_GDD = ifelse(TN>11, TN, 11),TX_GDD = case_when(TX<11~11, TX==11~11, TX==30~30, 11<TX & TX<30~TX, TX>30~30), GDDjour=(TX_GDD+TN_GDD)/2-11 ) %>%
  dplyr::select(-TN_GDD) %>%
  dplyr::select(-TX_GDD)

meteo2 <- meteo %>% filter(site=="PEROLS") %>% mutate(site="MONTPELLIER") # pour les données météo sur montpellier, on prend les données de PEROLS

meteo = bind_rows(meteo,meteo2)

lag_max <- 55 # (2 mois avant la capture)



meteo$GDDacc<-NA
meteo$GDDbound<-NA
i<-1
## calcul GDD accumulé et limité à 1350 depuis 1e janvuer
while (i<=nrow(meteo)){
  date<-meteo$date[i]
  if (date<"2023-01-01"){
    meteo$GDDacc[i]<-NA
    meteo$GDDbound[i]<-NA
  }
  else{
    gdd<-subset(meteo, meteo$date>="2023-01-01"& meteo$date<=meteo$date[i])
    gdd_acc<-sum(gdd$GDDjour)
    meteo$GDDacc[i]<-gdd_acc
    if (gdd_acc<=1350){
      meteo$GDDbound[i]<-gdd_acc}
    else{
      meteo$GDDbound[i]<-meteo$GDDbound[i-1]-meteo$GDDjour[i]
    }
  }
  i<-i+1
}



#### Functions

fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize,fun_summarize,new_var_name,n_days_agg){

  if(fun_summarize=="sum"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture, lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      dplyr::summarise(val=sum(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  } else if (fun_summarize == "mean"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=mean(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "max"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=max(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "min"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=min(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }
  return(df_meteo_pieges2_summarize)

}


meteo <- meteo %>%
  mutate(date = as.character(date))

pieges_data2 <- pieges_data %>%
  dplyr::select(idpointdecapture, num_releve, num_piege, date_releve, site) %>%
  mutate(date_releve=as.Date(date_releve))


# Fonction pour créer les lignes avec les différentes valeurs de lag
create_rows <- function(i) {
  map_df(0:lag_max, ~data.frame(
    idpointdecapture = pieges_data2$idpointdecapture[i],
    num_releve = pieges_data2$num_releve[i],
    num_piege = pieges_data2$num_piege[i],
    site = pieges_data2$site[i],
    date = as.character(as.Date(pieges_data2$date_releve[i] - .x)),
    lag_n = .x,
    stringsAsFactors = FALSE
  ))
}

# Appliquer la fonction à toutes les lignes de pieges_data2
df_meteo_pieges <- furrr::future_map_dfr(1:nrow(pieges_data2), create_rows)



# summarizing to weeks
df_meteo_pieges2 <- df_meteo_pieges %>%
  left_join(meteo, by = c("date","site")) %>%
  pivot_longer(!(idpointdecapture:lag_n), names_to = "var", values_to = 'val')

df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RR","sum","RR",7) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RR","max","RRMAX",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"DRR","sum","DRR",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TN","min","TN",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TX","max","TX",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TM","mean","TM",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TAMPLI","mean","TAMPLI",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"FFM","mean","FFM",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"FXY","mean","FXY",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"UM","mean","UM",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDjour","sum","GDDjour",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDacc","mean","GDDacc",7)) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDbound","mean","GDDbound",7))

## je ne sais pas pourquoi cela va jusque 8 parfois, mais on enleve pour avoir 7 (0+7, donc 8 en tout) semaines
df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n<8)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){

  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))

  max_col <- ncol(df_timeseries_wide)

  for(i in 2:(max_col-1)){
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

  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)

}



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RR","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"RRMAX","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"DRR","sum")
df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TN","mean")
df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"TX","mean")
df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide7 <- fun_ccm_df(df_meteo_pieges_summ,"TAMPLI","mean")
df_meteo_pieges_summ_wide8 <- fun_ccm_df(df_meteo_pieges_summ,"FFM","mean")
df_meteo_pieges_summ_wide9 <- fun_ccm_df(df_meteo_pieges_summ,"FXY","mean")
df_meteo_pieges_summ_wide10 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")
df_meteo_pieges_summ_wide11 <- fun_ccm_df(df_meteo_pieges_summ,"GDDjour","sum")
df_meteo_pieges_summ_wide12 <- fun_ccm_df(df_meteo_pieges_summ,"GDDacc","mean")
df_meteo_pieges_summ_wide13 <- fun_ccm_df(df_meteo_pieges_summ,"GDDbound","mean")




df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3) %>%
  left_join(df_meteo_pieges_summ_wide4) %>%
  left_join(df_meteo_pieges_summ_wide5) %>%
  left_join(df_meteo_pieges_summ_wide6) %>%
  left_join(df_meteo_pieges_summ_wide7) %>%
  left_join(df_meteo_pieges_summ_wide8) %>%
  left_join(df_meteo_pieges_summ_wide9) %>%
  left_join(df_meteo_pieges_summ_wide10) %>%
  left_join(df_meteo_pieges_summ_wide11) %>%
  left_join(df_meteo_pieges_summ_wide12) %>%
  left_join(df_meteo_pieges_summ_wide13)



### ### ### ### ### ### ### ###
### Variables paysagères, issues d'OSM
### ### ### ### ### ### ### ###

# OSM data with package osmdata
# see tuto at : https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library(osmdata)
library(terra)
library(sf)

# available_features()
# keys = c("landuse","leisure","highway","building")
keys = c("leisure")

fun_get_osm_data <- function(nom_site,name_key){

  bb <- c(left = min(pieges_data$Longitude[which(pieges_data$site == nom_site)])-0.002, bottom = min(pieges_data$Latitude[which(pieges_data$site == nom_site)])-0.002, right = max(pieges_data$Longitude[which(pieges_data$site == nom_site)])+0.002, top = max(pieges_data$Latitude[which(pieges_data$site == nom_site)])+0.002)
  #bb <- getbb(nom_site)
  q <- opq(bbox = bb)

  q1 <- add_osm_feature(q, key = name_key)
  osm_data <- osmdata_sf(q1)

  return(osm_data)
}


fun_distance_to_nearest_feat <- function(sf_pieges,sf_data){

  if(nrow(sf_data)>0){

  sf_pieges <- st_transform(sf_pieges,terra::crs(sf_data))

  # sf_osm = sf_osm[!is.na(as.data.frame(sf_osm)[,col_filter]),]
  # sf_osm = sf_osm[as.data.frame(sf_osm)[,col_filter] %in% feature_in_osm,]

  nearest <- st_nearest_feature(sf_pieges,sf_data)
  dist <- st_distance(sf_pieges, sf_data[nearest,], by_element=TRUE) %>% as.numeric()
  dist <- data.frame(sf_pieges$num_piege,dist)
  } else {
    dist <- data.frame(sf_pieges$num_piege,rep(NA,nrow(sf_pieges)))
  }

  colnames(dist) <- c("num_piege","dist_to_nearest")

  return(dist)


}

fun_surface_in_buffer <- function(sf_pieges,sf_data,buffer_size){

  sf_pieges <- st_transform(sf_pieges,2154) # pour calculer des distances en mètres
  sf_data <- st_transform(sf_data,2154)

  df_surf = sf::st_intersection(st_buffer(sf_pieges,buffer_size), sf_data) %>%
    mutate(aire = as.numeric(st_area(geometry))) %>%
     st_drop_geometry() %>%
     group_by(num_piege) %>%
     summarise(aire = sum(aire))

  return(df_surf)
}

fun_length_in_buffer <- function(sf_pieges,sf_data,buffer_size){

  sf_pieges <- st_transform(sf_pieges,2154) # pour calculer des distances en mètres
  sf_data <- st_transform(sf_data,2154)

  df_len = sf::st_intersection(st_buffer(sf_pieges,buffer_size), sf_data) %>%
    mutate(long = as.numeric(st_length(geometry))) %>%
    st_drop_geometry() %>%
    group_by(num_piege) %>%
    summarise(long = sum(long))

  return(df_len)
}

fun_intersect <- function(sf_pieges,sf_data,features_in_osm){  # features_in_osm = columns in osm data that we want to keep

  sf_pieges <- st_transform(sf_pieges,terra::crs(sf_data))

  df_intersect <- sf::st_intersection(sf_pieges,sf_data) %>%
    dplyr::select(num_piege,!!features_in_osm) %>%
    st_drop_geometry()

  return(df_intersect)

}


osm_data <- expand.grid(keys,sites) %>%
  rename(key = Var1, site = Var2) %>%
  mutate(osm_data = purrr::map2(site,key, ~fun_get_osm_data(.x,.y)))

pieges_location_nest <- pieges_location %>%
  group_by(site) %>%
  tidyr::nest(coords=c(Latitude,Longitude,num_piege)) %>%
  mutate(sf_points=map(coords,~sf::st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326))) %>%
  dplyr::select(-coords)

pieges_location_osm <- pieges_location_nest %>%
  left_join(osm_data, by = "site")



## leisure
var_leisure <- pieges_location_osm %>%
  filter(key=="leisure") %>%
  mutate(swimming_pool = map2(sf_points,osm_data, ~fun_distance_to_nearest_feat(.x,.y$osm_polygons %>% filter(leisure == "swimming_pool")))) %>%
  mutate(park_garden = map2(sf_points,osm_data, ~fun_distance_to_nearest_feat(.x,.y$osm_polygons %>% filter(leisure %in% c("park","garden")))))

dist_to_swimming_pool <- list_rbind(var_leisure$swimming_pool)
dist_to_park_garden <-  list_rbind(var_leisure$park_garden)


#ggplot() + geom_sf(data = batiments$osm_polygons) + geom_sf(data = highway$osm_lines) + geom_sf(data = leisure$osm_polygons, fill = "lightblue")  + geom_sf(data = df_sf %>% filter(Site=="PEROLS")) + theme_bw()




## BDTOPO

library(ows4R) # interface for OGC webservices
library(httr) # generic webservice package

wfs_ign <- "https://data.geopf.fr/wfs"
ign_client <- WFSClient$new(wfs_ign, serviceVersion = "2.0.0")

key <- c("BDTOPO_V3:batiment","BDTOPO_V3:cours_d_eau","BDTOPO_V3:troncon_de_route","BDTOPO_V3:zone_de_vegetation","BDTOPO_V3:cimetiere")

fun_get_bdtopo_data <- function(nom_site,key){

  bb <- c(bottom = min(pieges_data$Latitude[which(pieges_data$site == nom_site)])-0.002, left = min(pieges_data$Longitude[which(pieges_data$site == nom_site)])-0.002,top = max(pieges_data$Latitude[which(pieges_data$site == nom_site)])+0.002, right = max(pieges_data$Longitude[which(pieges_data$site == nom_site)])+0.002)
  bb <- paste(bb, collapse = ",")

  url <- parse_url(wfs_ign)
  url$query <- list(service = "wfs",
                    #version = "2.0.0", # optional
                    request = "GetFeature",
                    typename = key,
                    srsName = "EPSG:4326",
                    bbox = bb)  ## ordre : bottom, left, top, right

  request <- build_url(url)
  bdtopo_data <- read_sf(request)


  return(bdtopo_data)
}

bdtopo_data <- expand.grid(key,sites) %>%
  rename(key = Var1, site = Var2) %>%
  mutate(bdtopo_data = furrr::future_map2(site,key, ~fun_get_bdtopo_data(.x,.y)))

pieges_location_bdtopo <- pieges_location_nest %>%
  left_join(bdtopo_data, by = "site")

## distance au cours d'eau le plus proche
var_cours_d_eau <- pieges_location_bdtopo %>%
  filter(key=="BDTOPO_V3:cours_d_eau") %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_cast(.,"MULTILINESTRING"))) %>%
  mutate(cours_d_eau = map2(sf_points,bdtopo_data, ~fun_distance_to_nearest_feat(.x,.y)))

dist_cours_eau <- list_rbind(var_cours_d_eau$cours_d_eau)
colnames(dist_cours_eau) <- c("num_piege","dist_to_cours_eau")

## distance au cimetiere le plus proche
var_cimetiere <- pieges_location_bdtopo %>%
  filter(key=="BDTOPO_V3:cimetiere") %>%
  filter(!(site %in% c("SAINT-MEDARD-EN-JALLES","BAYONNE"))) %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_cast(.,"GEOMETRYCOLLECTION"))) %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_collection_extract(.,"POLYGON"))) %>%
  mutate(cimetiere = map2(sf_points,bdtopo_data, ~fun_distance_to_nearest_feat(.x,.y)))

dist_cimetiere <- list_rbind(var_cimetiere$cimetiere)
colnames(dist_cimetiere) <- c("num_piege","dist_to_cimetiere")

## distance à la zone de végétation la plus proche et surface de vegetation dans buffer 50 m
var_dist_vege <- pieges_location_bdtopo %>%
  filter(key=="BDTOPO_V3:zone_de_vegetation") %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_cast(.,"GEOMETRYCOLLECTION"))) %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_collection_extract(.,"POLYGON"))) %>%
  mutate(zone_de_vegetation = map2(sf_points,bdtopo_data, ~fun_distance_to_nearest_feat(.x,.y))) %>%
  mutate(surface_vege = map2(sf_points,bdtopo_data, ~fun_surface_in_buffer(.x,.y,50)))

dist_vege <- list_rbind(var_dist_vege$zone_de_vegetation)
colnames(dist_vege) <- c("num_piege","dist_to_vegetation")

surf_vege <- list_rbind(var_dist_vege$surface_vege)
colnames(surf_vege) <- c("num_piege","surf_vegetation")


## buildings
var_buildings <-  pieges_location_bdtopo %>%
  filter(key=="BDTOPO_V3:batiment") %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_cast(.,"GEOMETRYCOLLECTION"))) %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_collection_extract(.,"POLYGON"))) %>%
  mutate(bati = map2(sf_points,bdtopo_data, ~fun_surface_in_buffer(.x,.y,50)))

surf_buildings <- list_rbind(var_buildings$bati)

## longueur des routes
var_routes <- pieges_location_bdtopo %>%
  filter(key=="BDTOPO_V3:troncon_de_route") %>%
  mutate(bdtopo_data = map(bdtopo_data,~sf::st_cast(.,"GEOMETRYCOLLECTION"))) %>%
  mutate(routes = map2(sf_points,bdtopo_data, ~fun_length_in_buffer(.x,.y,50)))

dist_routes <- list_rbind(var_routes$routes)
colnames(dist_routes) <- c("num_piege","longueur_routes")


pieges_location <- pieges_location %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


## distance à la frontiere "naturelle" des quartiers
sf_quartiers <- st_read("data/processed/contours_villes.gpkg")

sf_quartiers <- st_cast(sf_quartiers,"LINESTRING")

dist_to_border_quartier <- fun_distance_to_nearest_feat(st_transform(pieges_location,2154),st_transform(sf_quartiers,2154))
colnames(dist_to_border_quartier) <- c("num_piege","dist_to_border_quartier")


## Filosofi

filosofi <-  st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/Filosofi2017_carreaux_200m_gpkg/Filosofi2017_carreaux_200m_met.gpkg")
df_filosofi <- fun_intersect(pieges_location,filosofi,c("Ind", "Men", "Men_pauv", "Ind_snv", "Log_av45", "Log_45_70", "Log_70_90", "Log_ap90", "Log_soc"))


## landuse (local climate zone)

landuse_mmm_lcz <-  st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MM_MMM_LCZ/MOS_CLIMAT_M3M_LCZ.shp")
landuse <- fun_intersect(pieges_location,landuse_mmm,"lib19_ni_1")

## fine vegetation
sf_vegetation <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MMM_MMM_VegFine/MMM_MMM_VegFine.shp")

pieges_proj <- st_transform(pieges_location,terra::crs(sf_vegetation))

var_dist_vege <- fun_distance_to_nearest_feat(pieges_proj,sf_vegetation)
var_surf_vege_tot <- fun_surface_in_buffer(pieges_proj,sf_vegetation,50)


## fine population

sf_pop <- st_read("/home/ptaconet/contributions_diverses_projets_mivegec/modeling_vector_mtp/data/MMM_MMM_PopFine/MMM_MMM_PopFine.shp")

pieges_proj <- st_transform(pieges_location,terra::crs(sf_pop))

POP <- sf::st_intersection(st_buffer(pieges_proj,50), sf_pop) %>%
  st_drop_geometry() %>%
  group_by(num_piege) %>%
  summarise(POP = sum(pop_2016)) %>%
  complete(num_piege,  fill = list(POP = 0, sd = 0)) %>%
  as_tibble()





df_model <- pieges_data %>%
  dplyr::select(idpointdecapture , site, num_piege, Latitude, Longitude , date_releve, effectif_jour_PP ) %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance, by = "idpointdecapture")

write.csv(df_model, "df_model.csv", row.names = F)
