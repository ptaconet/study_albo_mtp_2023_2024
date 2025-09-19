library(tidyverse)
library(patchwork)
library(sf)
library(DBI)

pieges_data <- read.csv( file.path("data","processed","df_pieges_2022_2025.csv")) %>%
  filter(!is.na(date_releve)) %>%
  mutate( week = week(date_releve), month = month(date_releve), year = year(date_releve)) %>%
  mutate(effectif_jour = as.numeric(effectif_jour)) %>%
  mutate(year = factor(year, levels = c("2023", "2024","2025")))


df_meteofrance_2023_2025 <- read.csv(file.path("data","processed","data_meteofrance_2022_2025.csv")) %>%
  mutate( week = week(date), month = month(date), year = year(date)) %>%
  group_by(nom_commune, week, year) %>%
  summarise(RFD = sum(RR, na.rm = T), TMN = mean(TM, na.rm = T), TN = mean(TMN, na.rm = T), TX = mean(TX, na.rm = T), UM = mean(UM, na.rm = T)) %>%
  arrange(nom_commune, year, week) %>%
  group_by(nom_commune, year) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = as.character(year)) %>%
  ungroup() %>%
  rename(site = nom_commune)


pieges_data <- pieges_data %>%
  group_by(week, year, site) %>%
  summarise(effectif_jour=mean(effectif_jour, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  mutate(year=year(date))

df <- df_meteofrance_2023_2025 %>%
  mutate(year = as.numeric(year)) %>%
  left_join(pieges_data, by = c("year","week","site")) %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  dplyr::select(site, date, effectif_jour, RFD, TMN ) %>%
  rename(rainfall = RFD, temperature = TMN, egg_effectif_jour = effectif_jour)




con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "postgresql-taconet.alwaysdata.net",
  dbname = "taconet_albopictus",
  port = 5432,
  user = "******",
  password = "******"
)

sf::st_write(df, dsn = con, layer = "albopictus_field_surveillance",append = FALSE)


##########################################
### create table of coordinates of surveillance
##########################################

pieges_data <- read.csv( file.path("data","processed","df_pieges_2022_2025.csv")) %>%
  mutate(date_releve = as.Date(date_releve)) %>%
  group_by( site) %>%
  summarise(Latitude = mean(Latitude, na.rm = T), Longitude = mean(Longitude, na.rm = T), start_surveillance = min(date_releve), end_surveillance = max(date_releve)) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = 4326)

st_write(pieges_data, dsn = con, layer = "albopictus_field_surveillance_metadata_traps",append = FALSE)


DBI:::dbSendQuery(con,'create view albopictus_field_surveillance_traps_geo AS select b.site, a.date, a.egg_effectif_jour, a.temperature, a.rainfall ,  b.geometry from albopictus_field_surveillance a left join albopictus_field_surveillance_metadata_traps b ON a.site = b.site')

