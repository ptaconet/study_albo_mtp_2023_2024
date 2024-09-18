# library(tidyverse)
# library(furrr)
#
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

df_meteofrance_2023_2024 <- read_delim("data_meteofrance/dpt_34_2023_2024.csv.gz", delim = ";", na = "", show_col_types = FALSE) %>%
  filter(NOM_USUEL %in% c("MONTPELLIER-AEROPORT","MONTARNAUD")) %>%
  mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
  group_by(NOM_USUEL,date,year,month,week) %>%
  summarise(RFD = sum(RR, na.rm = T),
            TMN = mean(TM, na.rm = T),
            TMIN = mean(TN, na.rm = T),
            TMAX = mean(TX, na.rm = T),
            WINDMN = mean(FFM, na.rm = T)) %>%
  rename(nom_commune = NOM_USUEL) %>%
  mutate(nom_commune = case_when(nom_commune=="MONTPELLIER-AEROPORT" ~ "PEROLS",
                                 nom_commune=="MONTARNAUD" ~ "MURVIEL-LES-MONTPELLIER"))


write.csv(df_meteofrance_2023_2024,"data_meteofrance/data_meteofrance_2023_2024.csv", row.names = F)


df_meteofrance_historique <- read_delim("data_meteofrance/dpt_34_historique.csv.gz", delim = ";", na = "", show_col_types = FALSE) %>%
  filter(NOM_USUEL == "MONTPELLIER-AEROPORT") %>%
  mutate(date = parse_date_time(AAAAMMJJ,"ymd"), year = year(date), month = month(date), week = week(date)) %>%
  rename(RFD = RR, TMN = TM, TMIN = TN , TMAX = TX) %>%
  dplyr::select(date, year, month, week, RFD, TMN, TMIN, TMAX)

write.csv(df_meteofrance_historique,"data_meteofrance/data_meteofrance_historique.csv", row.names = F)
