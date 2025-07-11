# See tutorial here : https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/

library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices

df <- read.csv( file.path("data","processed","df_pieges.csv")) %>%
  distinct(num_piege,Latitude ,Longitude ,   site)

df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)

wfs_ign <- "https://data.geopf.fr/wfs"

ign_client <- WFSClient$new(wfs_ign, serviceVersion = "2.0.0")

# liste des couches disponibles :
ign_client$getFeatureTypes(pretty = TRUE)  # voir aussi ici : https://geoservices.ign.fr/services-geoplateforme-diffusion#nomstechniques          https://geoservices.ign.fr/services-web-experts-economie

bbox_perols <- c( bottom = min(df$Latitude[which(df$site == "PEROLS")])-0.002,left = min(df$Longitude[which(df$site == "PEROLS")])-0.002,  top = max(df$Latitude[which(df$site == "PEROLS")])+0.002,right = max(df$Longitude[which(df$site == "PEROLS")])+0.002)
bbox_murviel <- c( bottom = min(df$Latitude[which(df$site == "MURVIEL-LES-MONTPELLIER")])-0.002, left = min(df$Longitude[which(df$site == "MURVIEL-LES-MONTPELLIER")])-0.002,  top = max(df$Latitude[which(df$site == "MURVIEL-LES-MONTPELLIER")])+0.002, right = max(df$Longitude[which(df$site == "MURVIEL-LES-MONTPELLIER")])+0.002)
#bbox_montpellier <- c(bottom = min(df$Latitude[which(df$site == "MONTPELLIER")])-0.002,left = min(df$Longitude[which(df$site == "MONTPELLIER")])-0.002, top = max(df$Latitude[which(df$site == "MONTPELLIER")])+0.002, right = max(df$Longitude[which(df$site == "MONTPELLIER")])+0.002)

bbox_perols <- paste(bbox_perols, collapse = ",")
bbox_murviel <- paste(bbox_murviel, collapse = ",")
#bbox_montpellier <- paste(bbox_montpellier, collapse = ",")

couches_interet <- c("BDTOPO_V3:batiment","BDTOPO_V3:cours_d_eau","BDTOPO_V3:plan_d_eau","BDTOPO_V3:surface_hydrographique","BDTOPO_V3:troncon_de_route","BDTOPO_V3:zone_de_vegetation")




url <- parse_url(wfs_ign)
url$query <- list(service = "wfs",
                  version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = couches_interet[1],
                  srsName = "EPSG:4326",
                  bbox = bbox_murviel)  ## ordre : bottom, left, top, right
request <- build_url(url)
bati <- read_sf(request)

url <- parse_url(wfs_ign)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = couches_interet[5],
                  srsName = "EPSG:4326",
                  bbox = bbox_murviel)  ## ordre : bottom, left, top, right
request <- build_url(url)
route <- read_sf(request)

url <- parse_url(wfs_ign)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetFeature",
                  typename = couches_interet[6],
                  srsName = "EPSG:4326",
                  bbox = bbox_murviel)  ## ordre : bottom, left, top, right
request <- build_url(url)
vegetation <- read_sf(request)


ggplot() + geom_sf(data = bati) + geom_sf(data = route) + geom_sf(data = vegetation, fill = "lightgreen")  + geom_sf(data = df_sf %>% filter(site=="MURVIEL-LES-MONTPELLIER")) + theme_bw()






# OSM data with package osmdata
# see tuto at : https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library(osmdata)

bb <- c(left = min(df$Longitude[which(df$site == "PEROLS")])-0.002, bottom = min(df$Latitude[which(df$site == "PEROLS")])-0.002, right = max(df$Longitude[which(df$site == "PEROLS")])+0.002, top = max(df$Latitude[which(df$site == "PEROLS")])+0.002)
#bb <- getbb('Perols')
q <- opq(bbox = bb)

bbox_matrix <- matrix(c(bb["left"], bb["right"], bb["bottom"], bb["top"]),
                      nrow = 2, byrow = FALSE,
                      dimnames = list(c("x", "y"), c("min", "max")))

# available_features()

keys = c("landuse","leisure","highway","building")

q1 <- add_osm_feature(q,key = 'landuse')
landuse <- osmdata_sf(q1) %>% trim_osmdata(bbox_matrix)

q1 <- add_osm_feature(q,key = 'leisure')
leisure <- osmdata_sf(q1) %>% trim_osmdata(bbox_matrix)

q1 <- add_osm_feature(q,key = 'highway')
highway <- osmdata_sf(q1) %>% trim_osmdata(bbox_matrix)

q1 <- add_osm_feature(q,key = 'building')
batiments <- osmdata_sf(q1) %>% trim_osmdata(bbox_matrix)

ggplot() + geom_sf(data = batiments$osm_polygons) + geom_sf(data = highway$osm_lines) + geom_sf(data = leisure$osm_polygons, fill = "lightblue")  + geom_sf(data = df_sf %>% filter(site=="PEROLS")) + theme_bw()





### ### ### ### ### ### ### ###
### Variables paysagères, issues d'OSM
### ### ### ### ### ### ### ###

# OSM data with package osmdata
# see tuto at : https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library(osmdata)
library(terra)
library(sf)

# available_features()
keys = c("landuse","leisure","highway","building")

fun_get_osm_data <- function(nom_site,name_key){

  bb <- c(left = min(pieges_data$Longitude[which(pieges_data$site == nom_site)])-0.002, bottom = min(pieges_data$Latitude[which(pieges_data$site == nom_site)])-0.002, right = max(pieges_data$Longitude[which(pieges_data$site == nom_site)])+0.002, top = max(pieges_data$Latitude[which(pieges_data$site == nom_site)])+0.002)
  #bb <- getbb(nom_site)
  q <- opq(bbox = bb)

  q1 <- add_osm_feature(q, key = name_key)
  osm_data <- osmdata_sf(q1)

  return(osm_data)
}


fun_distance_to_nearest_feat <- function(sf_pieges,sf_osm,col_filter,feature_in_osm){

  sf_pieges <- st_transform(sf_pieges,terra::crs(sf_osm))

  sf_osm[as.data.frame(sf_osm)[,col_filter] == feature_in_osm,]

  nearest <- st_nearest_feature(sf_pieges,sf_osm)
  dist <- st_distance(sf_pieges, sf_osm[nearest,], by_element=TRUE) %>% as.numeric()
  dist <- data.frame(sf_pieges$num_piege,dist)
  colnames(dist) <- c("num_piege",feature_in_osm)
  return(dist)


}

fun_surface_in_buffer <- function(sf_pieges,sf_osm,feature_in_osm,buffer){


}

fun_intersect <- function(sf_pieges,sf_osm,feature_in_osm){  # feature_in_osm = column in osm data that we want to keep

  sf_pieges <- st_transform(sf_pieges,terra::crs(sf_osm))

  df_intersect <- sf::st_intersection(sf_pieges,sf_osm) %>%
    dplyr::select(num_piege,!!feature_in_osm) %>%
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


## landuse
var_landuse <- pieges_location_osm %>%
  filter(key=="landuse") %>%
  mutate(landuse = map2(sf_points,osm_data, ~fun_intersect(.x,.y$osm_polygons,"landuse")))

var_landuse <- list_rbind(var_landuse$landuse) %>%
  filter(!is.na(landuse)) %>%
  group_by(num_piege) %>%
  filter(row_number()==1)

## leisure
var_leisure <- pieges_location_osm %>%
  filter(key=="leisure") %>%
  mutate(landuse = map2(sf_points,osm_data, ~fun_distance_to_nearest_feat(.x,.y$osm_polygons,"leisure","swimming_pool")))


#ggplot() + geom_sf(data = batiments$osm_polygons) + geom_sf(data = highway$osm_lines) + geom_sf(data = leisure$osm_polygons, fill = "lightblue")  + geom_sf(data = df_sf %>% filter(site=="PEROLS")) + theme_bw()




df_model <- pieges_data %>%
  dplyr::select(idpointdecapture , site, num_piege, Latitude, Longitude , date_releve, effectif_jour_PP ) %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance, by = "idpointdecapture")

write.csv(df_model, "df_model.csv", row.names = F)
