library(tidyverse)
library(readxl)
library(sf)

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
df_pieges <- df %>%
  bind_rows(df_colombine) %>%
  rename(num_piege = NumPP, date_releve = daterec, site = Site) %>%
  mutate(Mois = as.character(lubridate::month(date_releve, label = TRUE))) %>%
  mutate(Mois =  fct_relevel(Mois, c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc"))) %>%
  mutate(Year = factor(Year, levels = c("2023", "2024"))) %>%
  mutate(saison = ifelse(Mois %in% c("mai","juin","juil","août","sept"), 'Summer','Winter')) %>%
  mutate(saison = fct_relevel(saison, c("Winter","Summer"))) %>%
  mutate(date_year = as.Date(paste0(Year,"-01-01"))) %>%
  filter(!is.na(Latitude))


write.csv(df_pieges, file.path("data","processed","df_pieges.csv"), row.names = F)
