library(tidyverse)
library(readxl)
library(sf)

# ## Import and prepare dataset of mosquito collection

# Data altopictus
# df <- read.csv("piege_data.csv") %>%
#   filter(!is.na(date_releve_jour),!is.na(effectif_jour), statut == "RAS") %>%
#   mutate(daterec = as.Date(parse_date_time(date_releve_jour,"d/m/y"))) %>%
#   rename(Latitude = y, Longitude = x, Site = nom_commune, NumPP = num_piege) %>%
#   mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
#   mutate(Latitude = gsub(",",".",Latitude), Longitude = gsub(",",".",Longitude), effectif_jour = gsub(",",".",effectif_jour)) %>%
#   mutate(effectif_jour = as.numeric(effectif_jour), Latitude = as.numeric(Latitude),  Longitude = as.numeric(Longitude)) %>%
#   filter(!is.na(effectif_jour)) %>%
#   filter(Year %in% c(2023,2024)) %>%
#   dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour)

df <- readxl::read_xlsx("PP reseaux Altopictus Occitanie 2025-12-09.xlsx") %>%
  filter(!is.na(date_releve_jour),!is.na(effectif_jour), statut == "RAS") %>%
  #mutate(daterec = as.Date(parse_date_time(date_releve_jour,"d/m/y"))) %>%
  mutate(daterec = date_releve_jour) %>%
  rename(Latitude = y, Longitude = x, Site = nom_commune, NumPP = num_piege) %>%
  mutate(week = week(daterec), Mois_numeric = month(daterec), Year = year(daterec)) %>%
  mutate(Latitude = gsub(",",".",Latitude), Longitude = gsub(",",".",Longitude), effectif_jour = gsub(",",".",effectif_jour)) %>%
  mutate(effectif_jour = as.numeric(effectif_jour), Latitude = as.numeric(Latitude),  Longitude = as.numeric(Longitude)) %>%
  filter(!is.na(effectif_jour)) %>%
  filter(Year %in% c(2023,2024,2025)) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour)

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
  mutate(effectif_jour = NB_OEUFS/7) %>%
  rename(week = SEMAINE, Year = ANNEE, Latitude = LATITUDE, Longitude = LONGITUDE, NumPP = ID_PP, Mois_numeric = MOIS) %>%
  dplyr::select(daterec, week, Year, Mois_numeric, NumPP, Latitude, Longitude, Site, effectif_jour)





# Bind data altopictus and Montpellier
df_pieges <- df %>%
  bind_rows(df_colombine) %>%
  rename(num_piege = NumPP, date_releve = daterec, site = Site) %>%
  mutate(Mois = as.character(lubridate::month(date_releve, label = TRUE))) %>%
  mutate(Mois =  fct_relevel(Mois, c("janv","févr","mars","avril","mai","juin","juil","août","sept","oct","nov","déc"))) %>%
  mutate(Year = factor(Year, levels = c("2023", "2024","2025"))) %>%
  mutate(saison = ifelse(Mois %in% c("mai","juin","juil","août","sept"), 'Summer','Winter')) %>%
  mutate(saison = fct_relevel(saison, c("Winter","Summer"))) %>%
  mutate(date_year = as.Date(paste0(Year,"-01-01"))) %>%
  filter(!is.na(Latitude))




## Data EID NICE
load("EID_Nice_2008_2023_all.RData")

dms_to_dd <- function(dms) {
  # Replace symbols with spaces and split
  dms_clean <- gsub("[°'\"]", " ", dms)  # replace ° ' " by spaces
  dms_clean <- gsub(",", ".", dms_clean) # replace , by .
  parts <- unlist(strsplit(dms_clean, "\\s+"))
  parts <- parts[parts != ""]            # remove empty parts

  deg <- as.numeric(parts[1])
  min <- as.numeric(parts[2])
  sec <- as.numeric(parts[3])
  hemi <- parts[4]

  dd <- deg + min/60 + sec/3600
  if (hemi %in% c("S", "W")) dd <- -dd

  return(dd)
}

data_all <- data_all %>%
  dplyr::filter(!is.na(eggs_per_day)) %>%
  rename(date_releve = date_detection, site = commune, num_piege = id_piege , effectif_jour = eggs_per_day) %>%
  mutate(Latitude = sapply(lat,dms_to_dd), Longitude = sapply(lon, dms_to_dd))



# for this analysis we remove Montpellier
df_pieges <- df_pieges %>% filter(!(site == "MONTPELLIER"))

write.csv(df_pieges, file.path("data","processed","df_pieges_2022_2025.csv"), row.names = F)
