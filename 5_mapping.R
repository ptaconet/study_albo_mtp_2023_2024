## Load packages

library(readxl)
library(tidyverse)
library(lubridate)
library(ggmap)
library(patchwork)
library(ggdensity)
library(tidyr)
#library(ggsn)
library(terra)
library(sp)
library(patchwork)
library(sf)
library(stars)
library(gstat)
library(sfhotspot)
library(purrr)
#library(rmapshaper)

# ## Import and prepare dataset of mosquito collection

# Data altopictus
df <- read_excel("piege_data.xlsx") %>%
  filter(!is.na(date_releve_jour), !is.na(effectif_jour_PP), statut == "RAS") %>%
  #mutate(daterec = parse_date_time(date_releve_jour,"d/m/y")) %>%
  rename(daterec = date_releve_jour, Latitude = y, Longitude = x, Site = nom_commune, NumPP = num_piege) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(effectif_jour_PP = as.numeric(effectif_jour_PP)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour_PP)

# Data Colombine
loc_pieges_2023 <- read_sf("MTP_P02_TRAPS_LOCATION_2023.gpkg") %>% mutate(ANNEE = 2023) %>% dplyr::select(ID_PIEGE,LATITUDE,LONGITUDE, ANNEE) %>% st_drop_geometry() %>% filter(!is.na(LATITUDE)) %>% mutate(ID_PIEGE = gsub("_","",ID_PIEGE)) %>% rename(ID_PP = ID_PIEGE) %>% mutate(LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE))
loc_pieges_2024 <- read_sf("MTP_P04_TRAPS_LOCATION_2024.gpkg") %>% mutate(ANNEE = 2024) %>% dplyr::select(ID_PIEGE,latitude,longitude, ANNEE) %>% st_drop_geometry() %>% filter(!is.na(ID_PIEGE)) %>% rename(LONGITUDE = longitude, LATITUDE = latitude, ID_PP = ID_PIEGE) %>% mutate(ID_PP = paste0("PP",ID_PP))
loc_pieges <- bind_rows(loc_pieges_2023,loc_pieges_2024) %>% mutate(ID_PP = case_when(ID_PP == "PP1" ~ "PP01",
                                                                                      ID_PP == "PP2" ~ "PP02",
                                                                                      ID_PP == "PP3" ~ "PP03",
                                                                                      ID_PP == "PP4" ~ "PP04",
                                                                                      ID_PP == "PP5" ~ "PP05",
                                                                                      ID_PP == "PP6" ~ "PP06",
                                                                                      TRUE ~ ID_PP))

df_colombine <- read.csv("MTP_EGGS_ABUNDANCE_2023_2024.csv") %>%
  left_join(loc_pieges) %>%
  mutate(Site = "MONTPELLIER") %>%
  mutate(daterec = lubridate::parse_date_time(paste(ANNEE, SEMAINE, 1, sep="/"),'Y/W/w')) %>%
  rename(week = SEMAINE, Year = ANNEE, Latitude = LATITUDE, Longitude = LONGITUDE, NumPP = ID_PP, Mois_numeric = MOIS, effectif_jour_PP = NB_OEUFS/7) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour_PP)

# Bind data altopictus and Montpellier
df <- df %>%
  bind_rows(df_colombine) %>%
  mutate(Mois = as.character(lubridate::month(daterec, label = TRUE))) %>%
  mutate(Mois =  fct_relevel(Mois, c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc"))) %>%
  mutate(Year = factor(Year, levels = c("2023", "2024"))) %>%
  mutate(saison = ifelse(Mois %in% c("mai","juin","juil","août","sept"), 'Summer','Winter')) %>%
  mutate(saison = fct_relevel(saison, c("Winter","Summer"))) %>%
  mutate(date_year = as.Date(paste0(Year,"-01-01"))) %>%
  filter(!is.na(Latitude))


### Mapping

# # Import cartographic data
# bbox_bayonne <- c(left = min(df$Longitude[which(df$Site == "BAYONNE")])-0.001, bottom = min(df$Latitude[which(df$Site == "BAYONNE")])-0.001, right = max(df$Longitude[which(df$Site == "BAYONNE")])+0.001, top = max(df$Latitude[which(df$Site == "BAYONNE")])+0.001)
# bbox_perols <- c(left = min(df$Longitude[which(df$Site == "PEROLS")])-0.002, bottom = min(df$Latitude[which(df$Site == "PEROLS")])-0.002, right = max(df$Longitude[which(df$Site == "PEROLS")])+0.002, top = max(df$Latitude[which(df$Site == "PEROLS")])+0.002)
# bbox_stmedard <- c(left = min(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, bottom = min(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, right = max(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002, top = max(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002)
# bbox_murviel <- c(left = min(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002)
# bbox_montpellier <- c(left = min(df$Longitude[which(df$Site == "MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MONTPELLIER")])+0.002)
# bbox_allsites <- c(left = min(df$Longitude)-1, bottom = min(df$Latitude)-1, right = max(df$Longitude)+1, top = max(df$Latitude)+1)
#
# ggmap::register_stadiamaps("MY API KEY", write = FALSE)
#
# map_bayonne <- get_stadiamap(bbox_bayonne, maptype = "stamen_terrain", zoom = 16)
# map_perols <- get_stadiamap(bbox_perols, maptype = "stamen_terrain", zoom = 16)
# map_stmedard <- get_stadiamap(bbox_stmedard, maptype = "stamen_terrain", zoom = 16)
# map_murviel <- get_stadiamap(bbox_murviel, maptype = "stamen_terrain", zoom = 16)
# map_montpellier <- get_stadiamap(bbox_montpellier, maptype = "stamen_terrain", zoom = 14)
# map_allsites <- get_stadiamap(bbox_allsites, maptype = "stamen_terrain", zoom = 8)
#


## Plot the location of the traps

df_sf <- df %>%
  st_as_sf( crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))

# function calculates angle with respect to polygon centroid.
# we need this to order the polygon correctly
calc_angle <- function(lon,lat) {
  cent_lon <- mean(lon)
  cent_lat <- mean(lat)
  ang <- atan2(lat - cent_lat, lon - cent_lon)

  return(ang)
}

bbox <-df %>%
  group_by(Site) %>%
  summarise(xmin = min(Longitude),ymin = min(Latitude), xmax=max(Longitude),  ymax = max(Latitude)) %>%
  gather(x,Longitude,c('xmin','xmax')) %>%
  gather(y,Latitude,c('ymin','ymax')) %>%
  st_as_sf(coords=c('Longitude','Latitude'),crs=4326,remove=F) %>%
  group_by(Site) %>%
  rename(lon = Longitude, lat = Latitude) %>%
  mutate(angle = calc_angle(lon,lat)) %>%
  arrange(angle) %>%
  summarise(do_union=FALSE) %>%
  st_cast('POLYGON')

#bbox = bbox[dim(bbox)[1]:1,]

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

traps_map <- ggmap(map_allsites) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = bbox, aes(col=Site), inherit.aes = FALSE, alpha = 0.7) +
  geom_sf(data = df_sf, aes(col=Site), size = 0.3, inherit.aes = FALSE)  +
  theme_bw() +
  scale_color_manual(values = cbp1) +
  ggtitle("Locations of the areas and traps") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_blank(),
         axis.title.y = element_blank())
  #+
  # ggsn::scalebar(dist = 250, dist_unit = "m",
  #                transform = TRUE,
  #                height = 0.02, st.size	=2.5,st.dist = 0.02,
  #                x.min = min(df$Longitude), x.max = max(df$Longitude) , y.min = min(df$Latitude)-0.001,y.max = max(df$Latitude),
  #                border.size = 0.3
  # )


#ggsave("plots/traps_map.png",traps_map, width = 7.74, height = 5.28, units = 'in')



## visualize the data in standard plots
ylim1 = boxplot.stats(df$effectif_jour_PP)$stats[c(1, 5)]
p0 =  ggplot(df , aes(x = as.factor(Year), y = effectif_jour_PP, fill = Site)) +
  geom_boxplot(outlier.size = 0.1) +
  scale_fill_brewer(palette = "PuOr") +
  theme_bw() +
  coord_cartesian(ylim = ylim1*2.5)

df_global_stats <- df %>%
  group_by(Site,NumPP) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), latitude = mean(Latitude), longitude = mean(Longitude))

df_mensual_stats <- df %>%
  mutate(daterec = round_date(daterec,"month")) %>%
  group_by(Site,NumPP,daterec) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), latitude = mean(Latitude), longitude = mean(Longitude))

df_mensual_stats2 <- df %>%
  mutate(daterec = round_date(daterec,"month")) %>%
  group_by(Site,daterec) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T))


ggplot(df_mensual_stats2, aes(x=daterec,y=effectif_jour_PP_mn, group = Site, colour = Site)) +
  geom_line() +
  geom_point() +
  #geom_ribbon(aes(ymin=effectif_jour_PP_mn-effectif_jour_PP_sd, ymax=effectif_jour_PP_mn+effectif_jour_PP_sd), linetype=2, alpha=0.1) +
  theme_bw()


ggplot(df_mensual_stats2, aes(x=daterec,y=effectif_jour_PP_mn)) +
  geom_line() +
  geom_point() +
  facet_wrap(.~Site) +
  geom_ribbon(aes(ymin=effectif_jour_PP_mn-effectif_jour_PP_sd, ymax=effectif_jour_PP_mn+effectif_jour_PP_sd), linetype=2, alpha=0.1) +
  theme_bw()


### other plots


cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## Create some plots to visualize the data

# number of eggs by year - month - season : boxplots

ylim1 = boxplot.stats(df$effectif_jour_PP)$stats[c(1, 5)]


p_annee <- ggplot(data = df , aes(x = as.factor(Year), y = effectif_jour_PP, fill = Site)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_fill_manual(values = cbp1) +
  #facet_wrap(.~environment) +
  theme_bw() +
  #ggtitle("Number of mosquito eggs by trap, site and year") +
  coord_cartesian(ylim = ylim1*2.5) +
  xlab("Year") +
  ylab("Number of eggs/ovitrap") +
  theme(legend.position="bottom")

p_annee


df_yearly_stats <- df %>%
  group_by(Year,Site) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), latitude = mean(Latitude, na.rm=T), longitude = mean(Longitude,na.rm =T))


p_annee2 <- ggplot(df_yearly_stats , aes(x = as.factor(Year), y = effectif_jour_PP_mn, group = Site, colour = Site)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = cbp1) +
  #facet_wrap(.~environment) +
  #geom_ribbon(aes(ymin=nb_oeufs_mn-nb_oeufs_sd, ymax=nb_oeufs_mn+nb_oeufs_sd, group = Site, colour = Site), alpha=0.1) +
  theme_bw() +
  #ggtitle("Average") +
  xlab("Year") +
  ylab("Average number of eggs/ovitrap") +
  theme(legend.position="bottom") +
  ylim(c(0,400))

p_annee2
# ggsave('plots/p_annee2.png', p_annee2)

p_annee+p_annee2 + plot_annotation(tag_level = "A")  #title = "Number of mosquito eggs collected by trap, site and year (left : boxplot, right : average)")

p_mois <- ggplot(df , aes(x = Mois, y = effectif_jour_PP, fill = Site)) +
  geom_boxplot(size = 0.1, outlier.shape = NA) +
  scale_fill_manual(values = cbp1) +
  #facet_wrap(.~environment) +
  theme_bw() +
  #ggtitle("Boxplots du nombre d'oeufs par piège, site et mois") +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=1)) +
  coord_cartesian(ylim = ylim1*1.5) +
  xlab("Month") +
  ylab("Number of eggs/ovitrap") +
  theme(legend.position="bottom")

p_mois
#ggsave('plots/p_mois.png', p_mois)

df_mensual_stats <- df %>%
  group_by(Site,Mois) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), latitude = mean(Latitude, na.rm=T), longitude = mean(Longitude, na.rm=T))

p_mois2 <- ggplot(df_mensual_stats, aes(x=Mois,y=effectif_jour_PP_mn, group = Site, colour = Site)) +
  geom_line() +
  #geom_rect(aes(xmin=0, xmax=4, ymin=-Inf, ymax=Inf), alpha=0.005,  linewidth = 0, fill = "yellow") +
  geom_rect(aes(xmin=4.5, xmax=10.5, ymin=-Inf, ymax=Inf), alpha=0.005, linewidth = 0, fill = "yellow") +
  #geom_rect(aes(xmin=11, xmax=12, ymin=-Inf, ymax=Inf), alpha=0.005, linewidth = 0, fill = "yellow") +
  geom_point() +
  scale_color_manual(values = cbp1) +
  #facet_wrap(.~environment) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,  vjust = 0.5, hjust=1)) +
  #scale_fill_manual("Season", values=c("yellow","blue"), labels=c("Winter (hot, dry)","Summer (hot, rainy)")) +
  #ggtitle("Nombre moyen d'oeufs par site et mois") +
  xlab("Month") +
  ylab("Average number of eggs/ovitrap") +
  theme(legend.position="bottom")

p_mois2
#ggsave('plots/p_mois2.png', p_mois2)

p_mois+p_mois2  + plot_annotation(tag_level = "A") #+ plot_annotation(title = "Number of mosquito eggs collected by trap, site and month (left : boxplot, right : average)")

p_saison <- df %>%
  ggplot(aes(x = saison, y = effectif_jour_PP, fill = Site)) +
  scale_fill_manual(values = cbp1) +
  geom_boxplot(outlier.shape = NA, size = 0.3) +
  #facet_wrap(.~environment) +
  theme_bw() +
  coord_cartesian(ylim = ylim1*1.5)  +
  #ggtitle("Number of mosquito eggs collected by trap, site and season") +
  xlab("Season") +
  ylab("Number of eggs/ovitrap") +
  theme(legend.position="bottom")

p_saison

#ggsave('plots/p_saison.png', p_saison)


df_mensual_stats2 <- df %>%
  mutate(Positivité=ifelse(effectif_jour_PP>0,1,0)) %>%
  group_by(Site,Year,Mois) %>%
  summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), positivite = sum(Positivité)/n()*100) %>%
  mutate(date=as.factor(paste0(Mois,"-",Year))) %>%
  mutate(date = fct_relevel(date,c(
    "janv-2023"    ,   "févr-2023"     , "mars-2023"     , "avril-2023"  ,  "mai-2023", "juin-2023" ,  "juil-2023"  ,"août-2023"  ,"sept-2023"  ,
    "oct-2023",  "nov-2023"   ,  "déc-2023" ,"janv-2024"    ,   "févr-2024"     , "mars-2024"     , "avril-2024"  ,  "mai-2024", "juin-2024" ,  "juil-2024"  ,"août-2024"  ,"sept-2024"  ,
    "oct-2024",  "nov-2024"   ,  "déc-2024"  ))) %>%
  mutate(positivite = ifelse(positivite==0,NA,positivite))


ggplot(df_mensual_stats2) +
  geom_bar(aes(fill=Site, y=effectif_jour_PP_mn, x=date), position=position_dodge2(padding=0,preserve = "single"), stat="identity",width = 0.7) +
  geom_line(aes(group=Site, color = Site, y=effectif_jour_PP_mn, x=date), linetype = "longdash", linewidth=0.3) +
  geom_line(aes(group=Site, color = Site, y=positivite*2, x=date), linetype = "dashed") +
  geom_point(aes(group=Site, color = Site, y=positivite*2, x=date), size = 1.5) +
  geom_rect(data = data.frame(x = 0, y = 0), aes(xmin=5.5, xmax=10.5, ymin=-Inf, ymax=Inf), alpha=0.2,  linewidth = 0, fill = "yellow") +
  geom_rect(data = data.frame(x = 0, y = 0), aes(xmin=15.5, xmax=18.5, ymin=-Inf, ymax=Inf), alpha=0.2,  linewidth = 0, fill = "yellow") +
  xlab("") +
  scale_fill_manual(values = cbp1) +
  scale_color_manual(values = cbp1) +
  theme_bw() +
  scale_y_continuous(name = "Number of eggs/ovitrap (mean)", sec.axis = sec_axis( trans=~./2, name="Positive traps (%)", breaks = seq(0,100, 25))) +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))









## Visualization in maps
# ressources :
# https://r-spatial.org/book/12-Interpolation.html
# https://www.molecularecologist.com/2016/03/03/geographical-heat-maps-in-r/
# https://cran.r-project.org/web/packages/ggmap/readme/README.html

bbox_bayonne <- c(left = min(df$Longitude[which(df$Site == "BAYONNE")])-0.001, bottom = min(df$Latitude[which(df$Site == "BAYONNE")])-0.001, right = max(df$Longitude[which(df$Site == "BAYONNE")])+0.001, top = max(df$Latitude[which(df$Site == "BAYONNE")])+0.001)
bbox_perols <- c(left = min(df$Longitude[which(df$Site == "PEROLS")])-0.002, bottom = min(df$Latitude[which(df$Site == "PEROLS")])-0.002, right = max(df$Longitude[which(df$Site == "PEROLS")])+0.002, top = max(df$Latitude[which(df$Site == "PEROLS")])+0.002)
bbox_stmedard <- c(left = min(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, bottom = min(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])-0.002, right = max(df$Longitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002, top = max(df$Latitude[which(df$Site == "SAINT-MEDARD-EN-JALLES")])+0.002)
bbox_murviel <- c(left = min(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MURVIEL-LES-MONTPELLIER")])+0.002)
bbox_montpellier <- c(left = min(df$Longitude[which(df$Site == "MONTPELLIER")])-0.002, bottom = min(df$Latitude[which(df$Site == "MONTPELLIER")])-0.002, right = max(df$Longitude[which(df$Site == "MONTPELLIER")])+0.002, top = max(df$Latitude[which(df$Site == "MONTPELLIER")])+0.002)
bbox_allsites <- c(left = min(df$Longitude)-1, bottom = min(df$Latitude)-1, right = max(df$Longitude)+1, top = max(df$Latitude)+1)

# ggmap::register_stadiamaps("MY API KEY", write = FALSE)

map_bayonne <- get_stadiamap(bbox_bayonne, maptype = "stamen_terrain", zoom = 16)
map_perols <- get_stadiamap(bbox_perols, maptype = "stamen_terrain", zoom = 16)
map_stmedard <- get_stadiamap(bbox_stmedard, maptype = "stamen_terrain", zoom = 16)
map_murviel <- get_stadiamap(bbox_murviel, maptype = "stamen_terrain", zoom = 16)
map_montpellier <- get_stadiamap(bbox_montpellier, maptype = "stamen_terrain", zoom = 16)
map_allsites <- get_stadiamap(bbox_allsites, maptype = "stamen_terrain", zoom = 8)

# autres jolis fonds :
# - stamen_toner_lite
# - stamen_toner_background
# - stamen_terrain_lines à utiliser avec ggmap(map_Duparc) + theme_void()

fun_get_map <- function(df, site, temporal_grouping_column, map_type, spat_res = 1/2220){

  # Define a function to fix the bbox to be in EPSG:3857
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")),
                         c("ymin", "xmin", "ymax", "xmax"))

    # Coonvert the bbox to an sf polygon, transform it to 3857,
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

    # Overwrite the bbox of the ggmap object with the transformed coordinates
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }

  # define theme
  my_theme <- function() {
    theme(
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "transparent"), # necessary to avoid drawing panel outline
      panel.border = element_rect(colour = "grey", fill=NA, linewidth=1),
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank()
    )
  }


  if(site == "BAYONNE"){

    map = map_bayonne
    df <- df %>% filter(Site == "BAYONNE")

  } else if (site == "PEROLS"){

    map = map_perols
    df <- df %>% filter(Site == "PEROLS")

  } else if (site == "SAINT-MEDARD-EN-JALLES"){

    map = map_stmedard
    df <- df %>% filter(Site == "SAINT-MEDARD-EN-JALLES")

  } else if (site == "MURVIEL-LES-MONTPELLIER"){

    map = map_murviel
    df <- df %>% filter(Site == "MURVIEL-LES-MONTPELLIER")

  } else if (site == "MONTPELLIER"){

    map = map_montpellier
    df <- df %>% filter(Site == "MONTPELLIER")

  } else if (site == "all"){
    map = map_allsites
  }


  if(map_type=="heatmap"){

    # df2 <- tidyr::uncount(df, effectif_jour_PP)

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude)) %>%
      tidyr::uncount(effectif_jour_PP_mn)


    final_map <- ggmap(map) +
      geom_hdr(aes(Longitude, Latitude, fill = after_stat(probs)), data = df2, alpha = .5, method = "kde") +
      geom_hdr_lines(aes(Longitude, Latitude), data = df2, method = "kde", linewidth = 0.2, show.legend=F) +
      scale_fill_brewer(name="Egg density", palette = "YlOrRd", labels=c("Lowest","","","Highest")) +
      {if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      my_theme() +
      theme(legend.position="none") +
      ggtitle(paste0(site," - by ", temporal_grouping_column))

  }

  if(map_type=="idw"){

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))

    colnames(df2)[1] <- temporal_grouping_column

    unique_times <- unique(df[,temporal_grouping_column])[[1]]

    df_sf <- st_as_sf(df2, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))

    grd <- st_bbox(df_sf) %>%
      st_as_stars(dx = spat_res)

    grd2 <- st_simplify(st_buffer(st_convex_hull(st_union(st_geometry(df_sf))), dist = 15000), dTolerance = 5000)

    maps <- list()

    for(i in 1:length(unique_times)){

      df_sf2 <- df_sf %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      j <- idw(effectif_jour_PP_mn~1, df_sf2,grd)

      rext <- st_bbox(j)

      r <- raster(t(j[[1]]), xmn = rext[1], xmx = rext[3],
                  ymn = rext[2], ymx=rext[4],
                  crs = st_crs(i)$proj4string)

      r = mask(r, as(grd2, "Spatial"))

      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))

      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="lightyellow", high="red", limits = c(0,max(df2$effectif_jour_PP_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02) +
        ggtitle(unique_times[i])

      maps[[i]] <- th_map
    }

    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")

  }


  if(map_type=="rasterized"){

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))

    colnames(df2)[1] <- temporal_grouping_column

    unique_times <- unique(df[,temporal_grouping_column])[[1]]

    for(i in 1:length(unique_times)){

      df_th_year <- df2 %>% filter(!!sym(temporal_grouping_column) == unique_times[i])
      df_th_year_sp <- SpatialPointsDataFrame(df_th_year[,c("Longitude","Latitude")], df_th_year,proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

      r <- raster(ext = extent(df_th_year_sp), crs = crs(df_th_year_sp), res = spat_res)
      r <- extend(r, c(1,1))
      r <- raster::rasterize(df_th_year_sp, r, "effectif_jour_PP_mn", fun = mean)

      r <-  as.data.frame(r, xy=TRUE) %>%
        filter(!is.na(layer))

      th_map<- ggmap(map) +
        geom_tile(data = r,  aes(x = x, y = y, fill = layer),  alpha = 0.8, size = 0.02) +
        scale_fill_gradient(low="lightyellow", high="red", limits = c(0,max(df2$effectif_jour_PP_mn))) +
        geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02) +
        ggtitle(unique_times[i])


      maps[[i]] <- th_map

    }

    final_map <- wrap_plots(maps) +  plot_layout(nrow = 1, guides = "collect")

  }

  if(map_type == "points"){

    df2 <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))

    final_map <- ggmap(map) +
      #{if(temporal_grouping_column=="Year")geom_point(data = df2,aes(x = Longitude, y = Latitude), color = "red", size = 0.02)}+
      geom_point(aes(x = Longitude, y = Latitude), data = df2, size = 0.5, color = "black") +
      geom_point(aes(x = Longitude, y = Latitude, size = effectif_jour_PP_mn), data = df2 %>% filter(effectif_jour_PP_mn>0), colour = "darkred", alpha = 0.7) +
      scale_size_continuous(breaks = c(10,20,50,100,200), limits = c(0,200), range = c(1,10), name="Mean egg count / trap / day", labels=c("1-10","10-20","20-50","50-100",">100")) +
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",1,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column)) +
      my_theme()
      #+ theme(legend.position="none")


  }

  if(map_type=="hexagrid"){

    # Use the function:
    map <- ggmap_bbox(map)

    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(3857)

    g = st_make_grid(df_sf , square=FALSE, cellsize = 150)
    nc2 = st_sf(geom=g)
    nc2$ID=1:length(g)

    a= nc2 %>%
      st_join(df_sf, join = st_intersects,left = TRUE) %>%
      filter(!is.na(Year)) %>%
      group_by(ID,!!sym(temporal_grouping_column)) %>%
      summarise(effectif_jour_PP_mn = mean(effectif_jour_PP, na.rm = T))


    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = a, aes(fill = effectif_jour_PP_mn), inherit.aes = FALSE,alpha = .7,) +
      scale_fill_gradient(low="lightyellow", high="red", trans = "sqrt") +
      {if(temporal_grouping_column=="Year")geom_sf(data = df_sf %>% st_transform(3857), color = "red", size = 0.01, inherit.aes = FALSE)}+
      facet_wrap(temporal_grouping_column, nrow = ifelse(temporal_grouping_column=="Mois",2,1)) +
      ggtitle(paste0(site," - by ", temporal_grouping_column," - ", map_type)) +
      my_theme()


  }

  if (map_type=="hotspots_eachyear"){

    map <- ggmap_bbox(map)

    unique_times <- unique(df[,temporal_grouping_column])[[1]]

    if(temporal_grouping_column=="Mois"){
      unique_times <- c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc")
    }
    if(temporal_grouping_column=="saison"){
      unique_times <- c("Winter","Summer")
    }

    df_sf <- st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude"))

    maps <- list()
    hotspots <- list()
    dfs_sf <- list()

    for(i in 1:length(unique_times)){

      df_th_year <- df %>%
        filter(!!sym(temporal_grouping_column) == unique_times[i]) %>%
        group_by(NumPP) %>%
        summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude))


      th_df_sf <- st_as_sf(df_th_year, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
        st_transform(32740)

      th_hotspot <- hotspot_gistar(th_df_sf, weights = effectif_jour_PP_mn, cell_size = 150, grid_type = "hex")

      th_hotspot <- th_hotspot %>%
        filter(gistar > 0)

      #th_hotspot <- ms_simplify(th_hotspot, keep = 0.1,keep_shapes = FALSE)


      hotspots[[i]] <- th_hotspot
      dfs_sf[[i]] <- th_df_sf

    }

    max_kde <- do.call("rbind", hotspots)
    max_kde <- max(max_kde$kde)

    for(i in 1:length(unique_times)){

      th_map <- ggmap(map) +
        coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
        geom_sf(data = hotspots[[i]] %>% st_transform(3857), mapping = aes(fill = kde), inherit.aes = FALSE, lwd = 0,alpha = .8) +
        scale_fill_gradient(low="lightyellow", high="red",limits = c(0,max_kde), name = "Egg density",  breaks=seq(0, max_kde, length.out = 4), labels = c("Lowest","","","Higest")) +
        {if(temporal_grouping_column=="Year")geom_sf(data = dfs_sf[[i]] %>% st_transform(3857), color = "red", size = 0.05, inherit.aes = FALSE)}+
        my_theme() +
        labs(subtitle = unique_times[i])


      maps[[i]] <- th_map
    }

    final_map <- wrap_plots(maps) +
      plot_layout(nrow = ifelse(temporal_grouping_column=="Mois",2,1), guides = "collect") +
      plot_annotation(title = paste0(site," - hotspots - ",temporal_grouping_column))


  }

  if(map_type == "hotspots_change"){

    map <- ggmap_bbox(map)

    # df_sf <- df %>%
    #   tidyr::uncount(effectif_jour_PP) %>%
    #   st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
    #   st_transform(32740)

    df_sf <- df %>%
      group_by(!!sym(temporal_grouping_column), NumPP) %>%
      summarise(nb_rec = n(), effectif_jour_PP_mn = trunc(mean(effectif_jour_PP, na.rm = T)), effectif_jour_PP_sd = sd(effectif_jour_PP, na.rm = T), Latitude = mean(Latitude), Longitude = mean(Longitude), daterec = mean(daterec)) %>%
      mutate(daterec = round(daterec,units="year")) %>%
      tidyr::uncount(effectif_jour_PP_mn) %>%
      st_as_sf(crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>%
      st_transform(32740)


    cl = hotspot_classify(
      df_sf,
      cell_size = 150,
      time = "daterec",
      period = "1 year",
      grid_type = "rect"
    )

    cl <- cl %>%
      filter(hotspot_category!="no pattern") %>%
      st_transform(3857)

    final_map <- ggmap(map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      geom_sf(data = cl, aes(fill = hotspot_category), inherit.aes = FALSE) +
      my_theme() +
      geom_sf(data = st_as_sf(df, crs = "OGC:CRS84", coords = c("Longitude", "Latitude")) %>% st_transform(3857), color = "red", size = 0.1, inherit.aes = FALSE) +
      ggtitle(paste0(site, " - Evolution des hotspots entre ", min(df$Year), " et ",max(df$Year)))

  }

  return(final_map)
}


Sites <- data.frame(site = c("BAYONNE","MURVIEL-LES-MONTPELLIER","PEROLS","SAINT-MEDARD-EN-JALLES","MONTPELLIER"))

# points - par année et par mois
pl1 <- Sites %>%
  mutate(pl_year_points = purrr::map(.$site,~fun_get_map(df,.,"Year","points"))) %>%
  mutate(pl_month_points = purrr::map(.$site,~fun_get_map(df,.,"Mois","points"))) %>%
  mutate(pl_saison_points = purrr::map(.$site,~fun_get_map(df,.,"saison","points")))

purrr::map2(pl1$site, pl1$pl_year_points, ~ggsave(paste0("plots/points_year_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl1$site, pl1$pl_month_points, ~ggsave(paste0("plots/points_month_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl1$site, pl1$pl_saison_points, ~ggsave(paste0("plots/points_saison_",.x,".png"),.y, width = 20, height = 5, units = 'in'))

# hexagrid - par année et par mois
pl2 <- Sites %>%
  mutate(pl_year_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"Year","hexagrid"))) %>%
  mutate(pl_month_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"Mois","hexagrid"))) %>%
  mutate(pl_saison_hexagrid = purrr::map(.$site,~fun_get_map(df,.,"saison","hexagrid")))


purrr::map2(pl2$site, pl2$pl_year_hexagrid, ~ggsave(paste0("plots/hexagrid_year_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl2$site, pl2$pl_month_hexagrid, ~ggsave(paste0("plots/hexagrid_month_",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl2$site, pl2$pl_saison_hexagrid, ~ggsave(paste0("plots/hexagrid_saison_",.x,".png"),.y, width = 20, height = 5, units = 'in'))

# hotspots - par année et par mois
# pl3 <- Sites %>%
#   mutate(pl_year_hotspots = purrr::map(.$site,~fun_get_map(df,.,"Year","hotspots_eachyear"))) %>%
#   mutate(pl_month_hotspots = purrr::map(.$site,~fun_get_map(df,.,"Mois","hotspots_eachyear"))) %>%
#   mutate(pl_saison_hotspots = purrr::map(.$site,~fun_get_map(df,.,"saison","hotspots_eachyear")))
#
# purrr::map2(pl3$site, pl3$pl_year_hotspots, ~ggsave(paste0("plots/hotspots_year_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
# purrr::map2(pl3$site, pl3$pl_month_hotspots, ~ggsave(paste0("plots/hotspots_month_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
# purrr::map2(pl3$site, pl3$pl_saison_hotspots, ~ggsave(paste0("plots/hotspots_saison_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
#

# hotspot change - par année

# pl4 <- Sites %>%
#   mutate(pl_year_hotspotchange = purrr::map(.$site,~fun_get_map(df,.,"Year","hotspots_change")))

# purrr::map2(pl4$site, pl4$pl_year_hotspotchange, ~ggsave(paste0("plots/hotspotchange_year_",.x,".png"),.y, width = 20, height = 5, units = 'in'))


# heatmap - par année et par mois
pl5 <- Sites %>%
  mutate(pl_year_heatmap = purrr::map(.$site,~fun_get_map(df,.,"Year","heatmap"))) %>%
  mutate(pl_month_heatmap = purrr::map(.$site,~fun_get_map(df,.,"Mois","heatmap"))) %>%
  mutate(pl_saison_heatmap = purrr::map(.$site,~fun_get_map(df,.,"saison","heatmap")))

purrr::map2(pl5$site, pl5$pl_year_heatmap, ~ggsave(paste0("plots/heatmap_year_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl5$site, pl5$pl_month_heatmap, ~ggsave(paste0("plots/heatmap_month_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))
purrr::map2(pl5$site, pl5$pl_saison_heatmap, ~ggsave(paste0("plots/heatmap_saison_col2",.x,".png"),.y, width = 20, height = 5, units = 'in'))


# Compositions de plots

p=pl1$pl_year_points[[1]]/pl5$pl_year_heatmap[[1]] + plot_annotation(title = "Duparc area (residential)", subtitle = "Spatio-temporal distribution of mosquito egg by year")
p2=pl1$pl_year_points[[3]]/pl5$pl_year_heatmap[[3]] + plot_annotation(title = "Bois Rouge area (residential)", subtitle = "Spatio-temporal distribution of mosquito egg by year")
p3=pl1$pl_year_points[[2]]/pl5$pl_year_heatmap[[2]] + plot_annotation(title = "Ravine area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by year")
p4=pl1$pl_year_points[[4]]/pl5$pl_year_heatmap[[4]] + plot_annotation(title = "ZT BR area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by year")

pl1$pl_month_points[[1]]/pl5$pl_month_heatmap[[1]] + plot_annotation(title = "Duparc area (residential settings)", subtitle = "Spatio-temporal distribution of mosquito egg by month")
pl1$pl_month_points[[3]]/pl5$pl_month_heatmap[[3]] + plot_annotation(title = "Bois Rouge area (residential settings)", subtitle = "Spatio-temporal distribution of mosquito egg by month")
pl1$pl_month_points[[2]]/pl5$pl_month_heatmap[[2]] + plot_annotation(title = "Ravine area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by month")
pl1$pl_month_points[[4]]/pl5$pl_month_heatmap[[4]] + plot_annotation(title = "ZT BR area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by month")

pl1$pl_saison_points[[1]]/pl5$pl_saison_heatmap[[1]] + plot_annotation(title = "Duparc area (residential settings)", subtitle = "Spatio-temporal distribution of mosquito egg by season of the year")
pl1$pl_saison_points[[3]]/pl5$pl_saison_heatmap[[3]] + plot_annotation(title = "Bois Rouge area (residential settings)", subtitle = "Spatio-temporal distribution of mosquito egg by season of the year")
pl1$pl_saison_points[[2]]/pl5$pl_saison_heatmap[[2]] + plot_annotation(title = "Ravine area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by season of the year")
pl1$pl_saison_points[[4]]/pl5$pl_saison_heatmap[[4]] + plot_annotation(title = "ZT BR area (buffer zone)", subtitle = "Spatio-temporal distribution of mosquito egg by season of the year")




ggsave("plots/heatmap2_year_Duparc.png",pl1$pl_year_points[[1]]/pl5$pl_year_heatmap[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_year_Duparc.png",pl1$pl_year_points[[1]]/pl2$pl_year_hexagrid[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_year_Duparc.png",pl1$pl_year_points[[1]]/pl3$pl_year_hotspots[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_year_Ravine.png",pl1$pl_year_points[[2]]/pl5$pl_year_heatmap[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_year_Ravine.png",pl1$pl_year_points[[2]]/pl2$pl_year_hexagrid[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_year_Ravine.png",pl1$pl_year_points[[2]]/pl3$pl_year_hotspots[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_year_BoisRouge.png",pl1$pl_year_points[[3]]/pl5$pl_year_heatmap[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_year_BoisRouge.png",pl1$pl_year_points[[3]]/pl2$pl_year_hexagrid[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_year_BoisRouge.png",pl1$pl_year_points[[3]]/pl3$pl_year_hotspots[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_year_ZTBR.png",pl1$pl_year_points[[4]]/pl5$pl_year_heatmap[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_year_ZTBR.png",pl1$pl_year_points[[4]]/pl2$pl_year_hexagrid[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_year_ZTBR.png",pl1$pl_year_points[[4]]/pl3$pl_year_hotspots[[4]],width = 29.7, height = 21, units = 'cm')


ggsave("plots/heatmap2_month_Duparc.png",pl1$pl_month_points[[1]]/pl5$pl_month_heatmap[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_month_Duparc.png",pl1$pl_month_points[[1]]/pl2$pl_month_hexagrid[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_month_Duparc.png",pl1$pl_month_points[[1]]/pl3$pl_month_hotspots[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_month_Ravine.png",pl1$pl_month_points[[2]]/pl5$pl_month_heatmap[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_month_Ravine.png",pl1$pl_month_points[[2]]/pl2$pl_month_hexagrid[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_month_Ravine.png",pl1$pl_month_points[[2]]/pl3$pl_month_hotspots[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_month_BoisRouge.png",pl1$pl_month_points[[3]]/pl5$pl_month_heatmap[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_month_BoisRouge.png",pl1$pl_month_points[[3]]/pl2$pl_month_hexagrid[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_month_BoisRouge.png",pl1$pl_month_points[[3]]/pl3$pl_month_hotspots[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_month_ZTBR.png",pl1$pl_month_points[[4]]/pl5$pl_month_heatmap[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_month_ZTBR.png",pl1$pl_month_points[[4]]/pl2$pl_month_hexagrid[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_month_ZTBR.png",pl1$pl_month_points[[4]]/pl3$pl_month_hotspots[[4]],width = 29.7, height = 21, units = 'cm')


ggsave("plots/heatmap2_saison_Duparc.png",pl1$pl_saison_points[[1]]/pl5$pl_saison_heatmap[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_saison_Duparc.png",pl1$pl_saison_points[[1]]/pl2$pl_saison_hexagrid[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_saison_Duparc.png",pl1$pl_saison_points[[1]]/pl3$pl_saison_hotspots[[1]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_saison_Ravine.png",pl1$pl_saison_points[[2]]/pl5$pl_saison_heatmap[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_saison_Ravine.png",pl1$pl_saison_points[[2]]/pl2$pl_saison_hexagrid[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_saison_Ravine.png",pl1$pl_saison_points[[2]]/pl3$pl_saison_hotspots[[2]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_saison_BoisRouge.png",pl1$pl_saison_points[[3]]/pl5$pl_saison_heatmap[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_saison_BoisRouge.png",pl1$pl_saison_points[[3]]/pl2$pl_saison_hexagrid[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_saison_BoisRouge.png",pl1$pl_saison_points[[3]]/pl3$pl_saison_hotspots[[3]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/heatmap2_saison_ZTBR.png",pl1$pl_saison_points[[4]]/pl5$pl_saison_heatmap[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hexagrid2_saison_ZTBR.png",pl1$pl_saison_points[[4]]/pl2$pl_saison_hexagrid[[4]],width = 29.7, height = 21, units = 'cm')
ggsave("plots/hotspots2_saison_ZTBR.png",pl1$pl_saison_points[[4]]/pl3$pl_saison_hotspots[[4]],width = 29.7, height = 21, units = 'cm')


files2zip <- dir('plots', full.names = TRUE)
zip(zipfile = 'plots', files = files2zip)


fun_get_map(df,"Duparc","Year","heatmap")

# points_map <- ggmap(map_Duparcnord) +
#   geom_point(data = df,aes(x = Longitude, y = Latitude), color = "red")



## autre style :
# ggmap(mm) +
#   geom_density2d(data = tidyr::uncount(df, effectif_jour_PP), aes(x = Longitude, y = Latitude), size = 0.3) +
#   stat_density2d(data = tidyr::uncount(df, effectif_jour_PP),
#                  aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01,
#                  bins = 7, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") +
#   scale_alpha(range = c(0, 0.3), guide = FALSE)

