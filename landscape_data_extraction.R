# See tutorial here : https://inbo.github.io/tutorials/tutorials/spatial_wfs_services/

library(sf) # simple features packages for handling vector GIS data
library(httr) # generic webservice package
library(tidyverse) # a suite of packages for data wrangling, transformation, plotting, ...
library(ows4R) # interface for OGC webservices


wfs_ign <- "https://data.geopf.fr/wfs"

ign_client <- WFSClient$new(wfs_ign,
                            serviceVersion = "2.0.0")

# liste des couches disponibles :
ign_client$getFeatureTypes(pretty = TRUE)  # voir aussi ici : https://geoservices.ign.fr/services-geoplateforme-diffusion#nomstechniques

bbox_perols <- c( bottom = min(df$Latitude[which(df$Site == "PEROLS")])-0.002,left = min(df$Longitude[which(df$Site == "PEROLS")])-0.002,  top = max(df$Latitude[which(df$Site == "PEROLS")])+0.002,right = max(df$Longitude[which(df$Site == "PEROLS")])+0.002)
bbox_murviel <- c( bottom = min(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, left = min(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002,  top = max(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002, right = max(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002)
bbox_montpellier <- c(bottom = min(df$Latitude[which(df$Site == "MONTPELLIER")])-0.002,left = min(df$Longitude[which(df$Site == "MONTPELLIER")])-0.002, top = max(df$Latitude[which(df$Site == "MONTPELLIER")])+0.002, right = max(df$Longitude[which(df$Site == "MONTPELLIER")])+0.002)

bbox_perols <- paste(bbox_perols, collapse = ",")
bbox_murviel <- paste(bbox_murviel, collapse = ",")
bbox_montpellier <- paste(bbox_montpellier, collapse = ",")

couches_interet <- c("BDTOPO_V3:batiment","BDTOPO_V3:cours_d_eau","BDTOPO_V3:plan_d_eau","BDTOPO_V3:surface_hydrographique","BDTOPO_V3:troncon_de_route","BDTOPO_V3:zone_de_vegetation")




url <- parse_url(wfs_ign)
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
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


ggplot() + geom_sf(data = bati) + geom_sf(data = route) + geom_sf(data = vegetation, fill = "lightgreen")  + geom_sf(data = df_sf %>% filter(Site=="MURVIEL-LES-MONTPELLIER")) + theme_bw()






# OSM data with package osmdata
# see tuto at : https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

library(osmdata)

bb <- c(left = min(df$Longitude[which(df$Site == "PEROLS")])-0.002, bottom = min(df$Latitude[which(df$Site == "PEROLS")])-0.002, right = max(df$Longitude[which(df$Site == "PEROLS")])+0.002, top = max(df$Latitude[which(df$Site == "PEROLS")])+0.002)
#bb <- getbb('Perols')
q <- opq(bbox = bb)

# available_features()

keys = c("landuse","leisure","highway","building")

q1 <- add_osm_feature(q,key = 'landuse')
landuse <- osmdata_sf(q1) %>% trim_osmdata(bb)

q1 <- add_osm_feature(q,key = 'leisure')
leisure <- osmdata_sf(q1) %>% trim_osmdata(bb)

q1 <- add_osm_feature(q,key = 'highway')
highway <- osmdata_sf(q1) %>% trim_osmdata(bb)

q1 <- add_osm_feature(q,key = 'building')
batiments <- osmdata_sf(q1) %>% trim_osmdata(bb)

ggplot() + geom_sf(data = batiments$osm_polygons) + geom_sf(data = highway$osm_lines) + geom_sf(data = leisure$osm_polygons, fill = "lightblue")  + geom_sf(data = df_sf %>% filter(Site=="PEROLS")) + theme_bw()
