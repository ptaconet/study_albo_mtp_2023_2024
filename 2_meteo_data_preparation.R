library(tidyverse)


df_meteofrance_RR_T_Vent <- list.files(file.path("data","raw","meteofrance"), full.names = T, pattern = "RR-T-Vent.csv.gz") %>%
  purrr::map_dfr(.,~read_delim(., delim = ";", show_col_types = FALSE, na = "")) %>%
  filter(NOM_USUEL %in% c("MONTPELLIER-AEROPORT","MONTARNAUD","BORDEAUX-MERIGNAC","BIARRITZ-PAYS-BASQUE","BEDARIEUX"))

df_meteofrance_autres_parametres <- list.files(file.path("data","raw","meteofrance"), full.names = T, pattern = "autres_parametres.csv.gz") %>%
  purrr::map_dfr(.,~read_delim(., delim = ";", show_col_types = FALSE, na = "")) %>%
  filter(NOM_USUEL %in% c("MONTPELLIER-AEROPORT","MONTARNAUD","BORDEAUX-MERIGNAC","BIARRITZ-PAYS-BASQUE","BEDARIEUX"))

df_meteofrance <- df_meteofrance_RR_T_Vent %>%
  left_join(df_meteofrance_autres_parametres) %>%
  mutate(date = parse_date_time(AAAAMMJJ,"ymd")) %>%
  dplyr::select( NOM_USUEL , date, RR, DRR, TM, TN, TX, TAMPLI, FFM, FXY, UM) %>%
  mutate(DRR = DRR/60) %>% # passer les minutes en heures
  rename(nom_commune = NOM_USUEL) %>%
  mutate(nom_commune = case_when(nom_commune=="MONTPELLIER-AEROPORT" ~ "PEROLS",
                                 nom_commune=="MONTARNAUD" ~ "MURVIEL-LES-MONTPELLIER",
                                 nom_commune=="BORDEAUX-MERIGNAC" ~ "SAINT-MEDARD-EN-JALLES",
                                 nom_commune=="BIARRITZ-PAYS-BASQUE" ~ "BAYONNE",
                                 nom_commune=="BEDARIEUX" ~ "BEDARIEUX"))

# on comble les trous pour MURVIEL (DRR, FFM, FMX, UM) avec les données de BEDARIEUX (station la plus proche) puis on enleve bedarieux
df_meteofrance_bedarieux <- df_meteofrance %>%
  filter(nom_commune=="BEDARIEUX") %>%
  dplyr::select( nom_commune  ,    date,  DRR,FFM ,  FXY ,   UM) %>%
  rename(DRR2=DRR,FFM2=FFM ,  FXY2=FXY ,   UM2=UM) %>%
  mutate(nom_commune = "MURVIEL-LES-MONTPELLIER")

df_meteofrance <- df_meteofrance %>%
  left_join(df_meteofrance_bedarieux) %>%
  mutate(DRR = ifelse(nom_commune == "MURVIEL-LES-MONTPELLIER" & is.na(DRR), DRR2, DRR)) %>%
  mutate(FFM = ifelse(nom_commune == "MURVIEL-LES-MONTPELLIER" & is.na(FFM), FFM2, FFM)) %>%
  mutate(FXY = ifelse(nom_commune == "MURVIEL-LES-MONTPELLIER" & is.na(FXY), FXY2, FXY)) %>%
  mutate(UM = ifelse(nom_commune == "MURVIEL-LES-MONTPELLIER" & is.na(UM), UM2, UM)) %>%
  dplyr::select(-c("DRR2","FFM2","FXY2","UM2")) %>%
  dplyr::filter(nom_commune!="BEDARIEUX")


df_meteofrance_2022_2024 <- df_meteofrance %>% filter(date>="2022-01-01")
write.csv(df_meteofrance_2022_2024,file.path("data","processed","data_meteofrance_2022_2024.csv"), row.names = F)

df_meteofrance_historique <- df_meteofrance %>% filter(date<"2022-01-01")
write.csv(df_meteofrance_historique,file.path("data","processed","data_meteofrance_historique.csv"), row.names = F)



## préparation des données synop:

# df_meteofrance_2023_2024 <- list.files("data_meteofrance", full.names = T, pattern = "synop") %>%
#   future_map_dfr(.,~read_delim(., delim = ";", na = "mq",show_col_types = FALSE)) %>%
#   filter(numer_sta == "07643") %>%
#   mutate(date = parse_date_time(date,"ymdHMS")) %>%
#   mutate(jour = as_date(date)) %>%
#   mutate(temp = t - 273.15) %>%
#   group_by(numer_sta,jour) %>%
#   summarise(RFD = sum(rr3, na.rm = T),
#             TMIN = min(temp, na.rm = T),
#             TMAX = max(temp, na.rm = T),
#             TMN = mean(temp, na.rm = T),
#             TAMP = max(temp, na.rm = T) - min(temp, na.rm = T),
#             TSD = sd(temp, na.rm = T),
#             RHMIN = min(u, na.rm = T),
#             RHMAX = max(u, na.rm = T),
#             RHMN = mean(u, na.rm = T),
#             RHAMP = max(u, na.rm = T) - min(u, na.rm = T),
#             RHSD = sd(u, na.rm = T),
#             WINDMIN = min(ff, na.rm = T),
#             WINDMAX = max(ff, na.rm = T),
#             WINDMN = mean(ff, na.rm = T)) %>%
#   mutate(RFD = ifelse(RFD<0,0,RFD)) %>%
#   rename(date=jour) %>%
#   mutate(dpt = case_when(numer_sta == "07643" ~ "HERAULT"))
#
# write.csv(df_meteofrance,"data_meteofrance_2022_2024.csv", row.names = F)
#
#




#
#
# # number of consecutive days without rain (last N days)
# df_meteofrance_2023_2024_RFNO <-  df_meteofrance_2023_2024_RR_T_Vent %>%
#   left_join(df_meteofrance_2023_2024_autres_parametres) %>%
#   mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
#   group_by(NOM_USUEL,date,year,month,week) %>%
#   mutate(sequence = data.table::rleid(RR == 0),) %>%
#   filter(RR == 0) %>%
#   group_by(NOM_USUEL,date,year,month,week, sequence) %>%
#   summarise(RFNO = n())
#
#
# summarise(RFD = sum(RR, na.rm = T),
#           RFDUREE = sum(DRR, na.rm = T)/60,
#           RFMAX = max(RR, na.rm = T),
#           TMN = mean(TM, na.rm = T),
#           TMIN = min(TN, na.rm = T),
#           TMAX = max(TX, na.rm = T),
#           WINDMN = mean(FFM, na.rm = T),
#           WINDMAX = mean(FXY, na.rm = T),
#           UMN = mean(UM, na.rm = T),
#           UMIN = min(UM, na.rm = T),
#           UMAX = max(UM, na.rm = T)) %>%
