library(tidyverse)
library(patchwork)

pieges_data <- read.csv("piege_data.csv") %>%
  filter(!is.na(date_releve_jour), statut == "RAS") %>%
  mutate(date_releve_jour = parse_date_time(date_releve_jour,"d/m/y"), week = week(date_releve_jour), month = month(date_releve_jour), year = year(date_releve_jour)) %>%
  filter(dpt == "HERAULT") %>%
  mutate(effectif_jour_PP = as.numeric(effectif_jour_PP)) %>%
  mutate(year = factor(year, levels = c("2023", "2024")))


df_meteofrance_historique <- read.csv("data_meteofrance/data_meteofrance_historique.csv") %>%
  group_by(year, week) %>%
  summarise(RFD = sum(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
  group_by(week) %>%
  summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
            #RFD_sd = sd(RFD, na.rm = T), TMN_sd = sd(TMN, na.rm = T), TMIN_sd = sd(TMIN, na.rm = T), TMAX_sd = sd(TMAX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = "moy. 1950-2022")


df_meteofrance_2023_2024 <-  read.csv("data_meteofrance/data_meteofrance_2023_2024.csv") %>%
  mutate(date = as.Date(date)) %>%
  group_by(year, week) %>%
  summarise(RFD = sum(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = as.character(year))


df_meteofrance_proj <-  read.delim("data_meteofrance/tasmintasmaxtasprtothusssfcwind_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20230101-21001231.txt", skip = 64, sep = ",", col.names = c("Date", "Latitude", "Longitude",  "tasminAdjust", "tasmaxAdjust" ,"tasAdjust", "prtotAdjust" ,"hussAdjust", "sfcWindAdjust"),  na.strings = "-999.99") %>%
  mutate(date = parse_date_time(Date,"ymd"), week = week(date), year = year(date)) %>%
  group_by(year, week) %>%
  summarise(RFD = sum(prtotAdjust, na.rm = T)*86400, TMN = mean(tasAdjust, na.rm = T)-273.15, TMIN = mean(tasminAdjust, na.rm = T)-273.15, TMAX = mean(tasmaxAdjust, na.rm = T)-273.15) %>%
  group_by(week) %>%
  summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = "proj. 2023-2100 (scenario rcp4.5)")


df_meteofrance <- rbind(df_meteofrance_historique,df_meteofrance_2023_2024,df_meteofrance_proj) %>%
  mutate(year = factor(year, levels = c("2023", "2024", "moy. 1950-2022","proj. 2023-2100 (scenario rcp4.5)")))


cbp1 <-c("#FD9B63", "#E7D37F","#81A263","#B60071")

# précipitations
scaleFactor1 <- max(df_meteofrance$RFDcum, na.rm = T) / max(pieges_data$effectif_jour_PP, na.rm = T)

p1 <- ggplot() +
  geom_line(aes(x = as.factor(df_meteofrance$week), y = df_meteofrance$RFDcum, colour = as.factor(df_meteofrance$year), group =  as.factor(df_meteofrance$year)), size = 0.5) +
  geom_boxplot(aes(x = as.factor(pieges_data$week), y = pieges_data$effectif_jour_PP * scaleFactor1, fill = as.factor(pieges_data$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "précipitation cumulées (mm)", sec.axis = sec_axis(~./scaleFactor1, name = "effectif jour PP")) +
  scale_fill_manual(values = cbp1, name = "Collectes larves") +
  scale_color_manual(values = cbp1, name = "Facteur météorologique") +
  labs(title="Larves albo et précipitation cumulées", x ="Semaine") +
  theme_bw()


# températures
scaleFactor2 <- max(df_meteofrance$TMN, na.rm = T) / max(pieges_data$effectif_jour_PP, na.rm = T)

p2 <- ggplot() +
  geom_line(aes(x = as.factor(df_meteofrance$week), y = df_meteofrance$TMN, colour = as.factor(df_meteofrance$year), group =  as.factor(df_meteofrance$year)), size = 0.5) +
  geom_boxplot(aes(x = as.factor(pieges_data$week), y = pieges_data$effectif_jour_PP * scaleFactor2, fill = as.factor(pieges_data$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "températures (°C)", sec.axis = sec_axis(~./scaleFactor2, name = "effectif jour PP")) +
  scale_fill_manual(values = cbp1, name = "Collectes larves") +
  scale_color_manual(values = cbp1, name = "Facteur météorologique") +
  labs(title="Larves albo et températures", x ="Semaine") +
  theme_bw()

p2/p1 + plot_layout(guides = "collect")

## par site - pas forcément très pertinent...
#
# pieges_data <- read.csv("piege_data.csv") %>%
#   filter(!is.na(date_releve_jour)) %>%
#   mutate(date_releve_jour = parse_date_time(date_releve_jour,"d/m/y"), week = week(date_releve_jour), month = month(date_releve_jour), year = year(date_releve_jour)) %>%
#   filter(dpt == "HERAULT") %>%
#   mutate(effectif_jour_PP = as.numeric(effectif_jour_PP)) %>%
#   mutate(year = factor(year, levels = c("2023", "2024")))
#
# df_meteofrance_historique <- read.csv("data_meteofrance/data_meteofrance_historique.csv") %>%
#   group_by(year, week) %>%
#   summarise(RFD = sum(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
#   group_by(week) %>%
#   summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
#   #RFD_sd = sd(RFD, na.rm = T), TMN_sd = sd(TMN, na.rm = T), TMIN_sd = sd(TMIN, na.rm = T), TMAX_sd = sd(TMAX, na.rm = T)) %>%
#   mutate(RFDcum = cumsum(RFD)) %>%
#   mutate(year = "1950-2022")
#
# df_meteofrance_historique <- rbind(df_meteofrance_historique %>% mutate(nom_commune="MURVIEL-LES-MONTPELLIER"),
#                                    df_meteofrance_historique %>% mutate(nom_commune="PEROLS"))
#
# df_meteofrance_2023_2024 <-  read.csv("data_meteofrance/data_meteofrance_2023_2024.csv") %>%
#   mutate(date = as.Date(date)) %>%
#   group_by(year, week, nom_commune) %>%
#   summarise(RFD = sum(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
#   group_by(nom_commune) %>%
#   mutate(RFDcum = cumsum(RFD)) %>%
#   mutate(year = as.character(year))
#
# df_meteofrance <- rbind(df_meteofrance_historique,df_meteofrance_2023_2024) %>%
#   mutate(year = factor(year, levels = c("2023", "2024", "1950-2022")))


# pieges_data_murv <- pieges_data %>% filter(nom_commune=="MURVIEL-LES-MONTPELLIER")
# df_meteofrance_murv <- df_meteofrance %>% filter(nom_commune=="MURVIEL-LES-MONTPELLIER")
#
# scaleFactor <- max(df_meteofrance_murv$RFDcum, na.rm = T) / max(pieges_data_murv$effectif_jour_PP, na.rm = T)
#
# ggplot() +
#   geom_line(aes(x = as.factor(df_meteofrance_murv$week), y = df_meteofrance_murv$RFDcum, colour = as.factor(df_meteofrance_murv$year), group =  as.factor(df_meteofrance_murv$year)), size = 0.5) +
#   geom_boxplot(aes(x = as.factor(pieges_data_murv$week), y = pieges_data_murv$effectif_jour_PP * scaleFactor, fill = as.factor(pieges_data_murv$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
#   scale_y_continuous(name = "RFDcum", sec.axis = sec_axis(~./scaleFactor, name = "nb larves albo")) +
#   scale_fill_manual(values = cbp1) +
#   scale_color_manual(values = cbp1) +
#   theme_bw()
#
#
#
#
# pieges_data_per <- pieges_data %>% filter(nom_commune=="PEROLS")
# df_meteofrance_per <- df_meteofrance %>% filter(nom_commune=="PEROLS")
#
# scaleFactor <- max(df_meteofrance_per$RFDcum, na.rm = T) / max(pieges_data_per$effectif_jour_PP, na.rm = T)
#
# ggplot() +
#   geom_line(aes(x = as.factor(df_meteofrance_per$week), y = df_meteofrance_per$RFDcum, colour = as.factor(df_meteofrance_per$year), group =  as.factor(df_meteofrance_per$year)), size = 0.5) +
#   geom_boxplot(aes(x = as.factor(pieges_data_per$week), y = pieges_data_per$effectif_jour_PP * scaleFactor, fill = as.factor(pieges_data_per$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
#   scale_y_continuous(name = "RFDcum", sec.axis = sec_axis(~./scaleFactor, name = "nb larves albo")) +
#   scale_fill_manual(values = cbp1) +
#   scale_color_manual(values = cbp1) +
#   theme_bw()
#























df_model <- read.csv("df_model.csv") %>%
  filter(nom_commune  != "SAINT-MEDARD-EN-JALLES") %>%
  mutate(date_releve_jour = as.Date(date_releve_jour))%>%
  mutate(pres_larves = ifelse(effectif_jour_PP>0,1,0)) %>%
  mutate(week = floor_date(date_releve_jour, "weeks")) %>%
  mutate(week_number = week(date_releve_jour), year = year(date_releve_jour))


## boxplots nb larves capturés en fonction du temps
ggplot(df_model, aes(x = week, y = effectif_jour_PP, group = week)) +
  geom_boxplot(outlier.shape = NA)



ggplot(df_model, aes(x = as.factor(week_number), y = effectif_jour_PP, fill = as.factor(year))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  theme_bw()


## Boxplots larves ~ conditions météo
df_meteo <- read.csv("data_meteofrance/data_meteofrance.csv")  %>%
  mutate(date = as.Date(date)) %>%
  filter(date > min(df_model$date_releve_jour, na.rm = T) - 30, date < max(df_model$date_releve_jour, na.rm = T) + 30) %>%
  mutate(week = floor_date(date, "weeks")) %>%
  group_by(week) %>%
  summarise(precipitations = sum(RFD, na.rm = T), tmin = mean(TMIN, na.rm = T), tmax = mean(TMAX, na.rm = T), tmean = mean(TMN, na.rm = T))


scaleFactor <- max(df_meteo$precipitations, na.rm = T) / max(df_model$effectif_jour_PP, na.rm = T)

plot_albo_precipitations <- ggplot() +
  geom_line(aes(x = df_meteo$week, y = df_meteo$precipitations), size = 0.5, show.legend = FALSE, color='steelblue') +
  geom_boxplot(aes(x = df_model$week, y = df_model$effectif_jour_PP * scaleFactor, group = df_model$week), show.legend = FALSE, outlier.shape=NA) +
  geom_jitter(aes(x = df_model$week, y = df_model$effectif_jour_PP * scaleFactor, group = df_model$week), position=position_jitter(3), cex=0.2) +
  scale_y_continuous(name = "precipitations", sec.axis = sec_axis(~./scaleFactor, name = "nb albo")) +
  scale_x_date(name = "date",date_labels = "%m/%Y", date_breaks = "1 months") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal(base_size = 10) +
  ggtitle("Precipitations")



df_meteo <- read.csv("data_meteofrance/data_meteofrance.csv")  %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2023-01-01", date < max(df_model$date_releve_jour, na.rm = T) + 30) %>%
  mutate(week = floor_date(date, "weeks")) %>%
  mutate(week_number = week(date), year = year(date)) %>%
  group_by(year,week_number) %>%
  summarise(precipitations = sum(RFD, na.rm = T), tmin = mean(TMIN, na.rm = T), tmax = mean(TMAX, na.rm = T), tmean = mean(TMN, na.rm = T))

scaleFactor <- max(df_meteo$tmax, na.rm = T) / max(df_model$effectif_jour_PP, na.rm = T)

cbp1 <-c("#999999", "#E69F00")

ggplot() +
  geom_line(aes(x = as.factor(df_meteo$week_number), y = df_meteo$tmax, color = as.factor(df_meteo$year), group = as.factor(df_meteo$year)), size = 0.5, show.legend = FALSE) +
  geom_boxplot(aes(x=as.factor(df_model$week_number), y = df_model$effectif_jour_PP * scaleFactor, fill = as.factor(df_model$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "tmax", sec.axis = sec_axis(~./scaleFactor, name = "nb larves albo")) +
  scale_fill_manual(values = cbp1) +
  scale_color_manual(values = cbp1) +
  theme_bw()





df_meteo <- read.csv("data_meteofrance/data_meteofrance.csv")  %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2023-01-01") %>%
  mutate(week_number = week(date), year = year(date)) %>%
  group_by(week_number,year) %>%
  summarise(precipitations = sum(RFD, na.rm = T), tmin = mean(TMIN, na.rm = T), tmax = mean(TMAX, na.rm = T), tmean = mean(TMN, na.rm = T)) %>%
  pivot_wider(names_from = year, values_from = precipitations:tmean) %>%
  mutate(diff_precipitation = precipitations_2024-precipitations_2023, diff_tmin = tmin_2024-tmin_2023, diff_tmax = tmax_2024-tmax_2023, diff_tmean = tmean_2024-tmean_2023) %>%
  pivot_longer(!week_number) %>%
  dplyr::filter(name %in% c("diff_tmean"))

scaleFactor <- max(df_meteo$value, na.rm = T) / max(df_model$effectif_jour_PP, na.rm = T) * 4


cbp1 <-c("#999999", "#E69F00")

ggplot() +
  geom_line(aes(x = as.factor(df_meteo$week_number), y = df_meteo$value, color = df_meteo$name,  group = df_meteo$name), size = 0.5, show.legend = FALSE) +
  geom_boxplot(aes(x=as.factor(df_model$week_number), y = df_model$effectif_jour_PP * scaleFactor, fill = as.factor(df_model$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "diff_tmean", sec.axis = sec_axis(~./scaleFactor, name = "nb larves albo")) +
  scale_color_manual(values = cbp1) +
  theme_bw()









ggplot(df_model, aes(x = TMN_0_4	, y = effectif_jour_PP, color = year(date_releve_jour))) + geom_point()  + geom_smooth(method = "gam") #+ facet_wrap(.~lieu)







df_meteo <- read.csv("data_meteofrance/data_meteofrance.csv")  %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= "2023-01-01") %>%
  mutate(week = week(date), year = year(date)) %>%
  group_by(week,year) %>%
  summarise(precipitations = sum(RFD, na.rm = T), tmin = mean(TMIN, na.rm = T), tmax = mean(TMAX, na.rm = T), tmean = mean(TMN, na.rm = T)) %>%
  pivot_wider(names_from = year, values_from = precipitations:tmean) %>%
  mutate(diff_precipitation = precipitations_2024-precipitations_2023, diff_tmin = tmin_2024-tmin_2023, diff_tmax = tmax_2024-tmax_2023, diff_tmean = tmean_2024-tmean_2023) %>%
  pivot_longer(!week)

df_model <- read.csv("df_model.csv") %>%
  filter(nom_commune  != "SAINT-MEDARD-EN-JALLES") %>%
  mutate(date_releve_jour = as.Date(date_releve_jour))%>%
  mutate(week = week(date_releve_jour), year = year(date_releve_jour))%>%
  group_by(week,year) %>%
  summarise(effectif_jour_PP = mean(effectif_jour_PP, na.rm = T)) %>%
  pivot_wider(names_from = year, values_from = effectif_jour_PP, names_prefix = "year_") %>%
  mutate(diff_pp_2023_2024 = year_2024-year_2023) %>%
  pivot_longer(!week)


dd <- rbind(df_meteo,df_model) %>%
  pivot_wider(names_from = name, values_from = value)


ggplot(dd, aes(x=diff_tmean , y=diff_pp_2023_2024)) + geom_point()
