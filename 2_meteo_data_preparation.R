library(tidyverse)

df_meteofrance <- list.files("data_meteofrance", full.names = T) %>%
  purrr::map_dfr(.,~read_delim(., delim = ";", na = "mq")) %>%
  filter(numer_sta %in% c("07643","07510")) %>%
  mutate(date = parse_date_time(date,"ymdHMS")) %>%
  mutate(jour = as_date(date)) %>%
  mutate(temp = t - 273.15) %>%
  group_by(numer_sta,jour) %>%
  summarise(RFD = sum(rr3, na.rm = T),
            TMIN = min(temp, na.rm = T),
            TMAX = max(temp, na.rm = T),
            TMN = mean(temp, na.rm = T),
            TAMP = max(temp, na.rm = T) - min(temp, na.rm = T),
            TSD = sd(temp, na.rm = T),
            RHMIN = min(u, na.rm = T),
            RHMAX = max(u, na.rm = T),
            RHMN = mean(u, na.rm = T),
            RHAMP = max(u, na.rm = T) - min(u, na.rm = T),
            RHSD = sd(u, na.rm = T),
            WINDMIN = min(ff, na.rm = T),
            WINDMAX = max(ff, na.rm = T),
            WINDMN = mean(ff, na.rm = T)) %>%
  mutate(RFD = ifelse(RFD<0,0,RFD)) %>%
  rename(date=jour) %>%
  mutate(dpt = case_when(numer_sta == "07643" ~ "HERAULT",
                         #numer_sta == "07643" ~ "PYRENEES-ATLANTIQUES",
                         numer_sta == "07510" ~ "GIRONDE" ))

write.csv(df_meteofrance,"data_meteofrance/data_meteofrance.csv",row.names = F)
