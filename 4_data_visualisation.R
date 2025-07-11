library(tidyverse)
library(patchwork)

pieges_data <- read.csv( file.path("data","processed","df_pieges.csv")) %>%
  filter(!is.na(date_releve)) %>%
  mutate( week = week(date_releve), month = month(date_releve), year = year(date_releve)) %>%
  mutate(effectif_jour = as.numeric(effectif_jour)) %>%
  mutate(year = factor(year, levels = c("2023", "2024")))

 df_meteofrance_historique <- read.csv("data/processed/data_meteofrance_historique.csv") %>%
   rename(site = nom_commune) %>%
   mutate( week = week(date), month = month(date), year = year(date)) %>%
   group_by(site, year, week) %>%
   summarise(RFD = sum(RR, na.rm = T), TMN = mean(TM, na.rm = T), TN = mean(TN, na.rm = T), TX = mean(TX, na.rm = T), UM = mean(UM, na.rm = T)) %>%
   group_by(site, week) %>%
   summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TN = mean(TN, na.rm = T), TX = mean(TX, na.rm = T), UM = mean(UM, na.rm = T)) %>%
   mutate(RFDcum = cumsum(RFD)) %>%
   mutate(year = "moy. 1950-2022") %>%
   ungroup()



df_meteofrance_2023_2024 <- read.csv(file.path("data","processed","data_meteofrance_2022_2024.csv")) %>%
  mutate( week = week(date), month = month(date), year = year(date)) %>%
  group_by(nom_commune, week, year) %>%
  summarise(RFD = sum(RR, na.rm = T), TMN = mean(TM, na.rm = T), TN = mean(TMN, na.rm = T), TX = mean(TX, na.rm = T), UM = mean(UM, na.rm = T)) %>%
  arrange(nom_commune, year, week) %>%
  group_by(nom_commune, year) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = as.character(year)) %>%
  ungroup() %>%
  rename(site = nom_commune)


#
# df_meteofrance_proj <-  read.delim("data_meteofrance/tasmintasmaxtasprtothusssfcwind_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20230101-21001231.txt", skip = 64, sep = ",", col.names = c("Date", "Latitude", "Longitude",  "tasminAdjust", "tasmaxAdjust" ,"tasAdjust", "prtotAdjust" ,"hussAdjust", "sfcWindAdjust"),  na.strings = "-999.99") %>%
#   mutate(date = parse_date_time(Date,"ymd"), week = week(date), year = year(date)) %>%
#   group_by(year, week) %>%
#   summarise(RFD = sum(prtotAdjust, na.rm = T)*86400, TMN = mean(tasAdjust, na.rm = T)-273.15, TMIN = mean(tasminAdjust, na.rm = T)-273.15, TMAX = mean(tasmaxAdjust, na.rm = T)-273.15) %>%
#   group_by(week) %>%
#   summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
#   mutate(RFDcum = cumsum(RFD)) %>%
#   mutate(year = "proj. 2023-2100 (scenario rcp4.5)")


# df_meteofrance <- rbind(df_meteofrance_historique,df_meteofrance_2023_2024,df_meteofrance_proj) %>%
#   mutate(year = factor(year, levels = c("2023", "2024", "moy. 1950-2022","proj. 2023-2100 (scenario rcp4.5)")))



cbp1 <-c("#FD9B63", "#E7D37F","#81A263","#B60071")

# précipitations
scaleFactor1 <- max(df_meteofrance_2023_2024$RFD, na.rm = T) / max(pieges_data$effectif_jour, na.rm = T)

p1 <- ggplot() +
  geom_line(aes(x = as.factor(df_meteofrance_2023_2024$week), y = df_meteofrance_2023_2024$RFD, colour = as.factor(df_meteofrance_2023_2024$year), group =  as.factor(df_meteofrance_2023_2024$year)), size = 0.5) +
  geom_boxplot(aes(x = as.factor(pieges_data$week), y = pieges_data$effectif_jour * scaleFactor1, fill = as.factor(pieges_data$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "précipitation cumulées (mm)", sec.axis = sec_axis(~./scaleFactor1, name = "effectif jour PP")) +
  scale_fill_manual(values = cbp1, name = "Collectes Oeufs") +
  scale_color_manual(values = cbp1, name = "Facteur météorologique") +
  labs(title="Oeufs albo et précipitation cumulées", x ="Semaine") +
  theme_bw()

# températures
scaleFactor2 <- max(df_meteofrance$TMN, na.rm = T) / max(pieges_data$effectif_jour_PP, na.rm = T)

p2 <- ggplot() +
  geom_line(aes(x = as.factor(df_meteofrance$week), y = df_meteofrance$TMN, colour = as.factor(df_meteofrance$year), group =  as.factor(df_meteofrance$year)), size = 0.5) +
  geom_boxplot(aes(x = as.factor(pieges_data$week), y = pieges_data$effectif_jour_PP * scaleFactor2, fill = as.factor(pieges_data$year)), outlier.shape = NA, position = position_dodge(preserve = "single"), size = 0.3) +
  scale_y_continuous(name = "températures (°C)", sec.axis = sec_axis(~./scaleFactor2, name = "effectif jour PP")) +
  scale_fill_manual(values = cbp1, name = "Collectes Oeufs") +
  scale_color_manual(values = cbp1, name = "Facteur météorologique") +
  labs(title="Oeufs albo et températures", x ="Semaine") +
  theme_bw()

p2/p1 + plot_layout(guides = "collect")






## 3 échelles : voir https://stackoverflow.com/questions/71630756/is-it-possible-to-add-a-third-y-axis-to-ggplot2


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


pieges_data <- pieges_data %>%
  group_by(week, year, site) %>%
  summarise(effectif_jour=mean(effectif_jour, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  mutate(year=year(date))

df <- df_meteofrance_2023_2024 %>%
  mutate(year = as.numeric(year)) %>%
  left_join(pieges_data, by = c("year","week","site")) %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(date = as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  mutate(site = fct_relevel(site, c("PEROLS", "MURVIEL-LES-MONTPELLIER", "BAYONNE","SAINT-MEDARD-EN-JALLES" ,  "RENNES" )))

df <- df %>% filter(site %in% c("PEROLS","MURVIEL-LES-MONTPELLIER","SAINT-MEDARD-EN-JALLES","BAYONNE"))

df_meteofrance_historique <- df_meteofrance_historique %>%
  mutate(site = fct_relevel(site, c("PEROLS", "MURVIEL-LES-MONTPELLIER","BAYONNE",  "SAINT-MEDARD-EN-JALLES" , "RENNES" ))) %>%
  filter(site %in% c("PEROLS","MURVIEL-LES-MONTPELLIER","SAINT-MEDARD-EN-JALLES","BAYONNE"))


# Scaling factor for mosquito abundance (adjust as needed)
scaleFactor <- 0.6


dates_init <- df %>%
  group_by(year, site) %>%
  filter(effectif_jour> 0) %>%
  slice(1) %>%
  ungroup()

# Plot
ggplot(df, aes(x = date)) +
  geom_col(aes(y = RFD), fill = "lightblue", alpha = 0.6) +  # Rainfall as bars
  geom_line(aes(y = TMN, color = "Temperature"), size = 0.5) +  # Temperature as line
  geom_point(aes(y = effectif_jour * scaleFactor,  color = "Eggs per trap")) +
  geom_line(data=df[!is.na(df$effectif_jour),], aes(y = effectif_jour * scaleFactor, color = "Eggs per trap"), size = 0.5) +  # Scaled mosquito abundance
   scale_y_continuous(
     name = "Temperature (°C) / Rainfall (mm)",
     limits = c(0,50),
     sec.axis = sec_axis(~ . / scaleFactor, name = "Eggs per trap")  # Secondary axis for mosquito counts
   ) +
  scale_color_manual(values = c("Temperature" = "blue", "Eggs per trap" = "red")) +  # Custom colors
  labs(x = "Date", color = "Legend") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  facet_wrap(.~site, nrow = 4, ncol = 1) +
  geom_label(data= dates_init, aes(label=week, xintercept = date, y = 4))







# plot by site

pred_llo <- read.csv("pred_llo.csv") %>%
  mutate(date=as.Date(date)) %>%
  filter(date>="2023-01-01") %>%
  dplyr::select(site,Year, week,pred,TM_0_8,TM_0_4,RR_1_5) %>%
  rename(year = Year) %>%
  mutate(site=as.factor(site))

df1 <- df %>%
  left_join(pred_llo) %>%
  mutate(TMN_lagged = ifelse(pred=="Absence", TM_0_8, TM_0_4)) %>%
  mutate(RFD_lagged = RR_1_5)


library(ggimage)

icons <- data.frame(
  date = c(as.Date(paste(2023,  c(9, 22, 31, 40), 1, sep = "-"), "%Y-%U-%u"),as.Date(paste(2024,  c(5, 22,31, 40,49), 1, sep = "-"), "%Y-%U-%u")),
  y = rep(55,9),
  image =  c("no_mosquito.jpg", "spring.jpg",  "summer.jpg", "autumn.jpg","no_mosquito.jpg","spring.jpg",  "summer.jpg", "autumn.jpg","no_mosquito.jpg")
)

fun_plot_by_site <- function(th_site){

df2 = df1 %>%
  mutate(effectif_jour=ifelse(site=="BAYONNE" & week==1 & year==2024, 1000, effectif_jour)) %>%
  mutate(effectif_jour=ifelse(site=="SAINT-MEDARD-EN-JALLES" & week==1 & year==2024, 1000, effectif_jour)) %>%
  filter(site==th_site)

p_th_site <- ggplot(df2, aes(x = date)) +
  geom_col(aes(y = RFD), fill = "lightblue", alpha = 0.6) +  # Rainfall as bars
  ggalt::geom_xspline(aes(y = TMN, color = "Temperature"), size = 0.5) +  # Temperature as line
  geom_point(aes(y = effectif_jour * scaleFactor,  color = "Eggs/trap"), size = 0.7) +
  geom_line(data=df2[!is.na(df2$effectif_jour),], aes(y = effectif_jour * scaleFactor, color = "Eggs/trap"), size = 0.5) +  # Scaled mosquito abundance
  geom_vline(xintercept = c(as.Date(paste(2023,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u"),as.Date(paste(2024,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u")), linetype = "dashed", size = 0.2) +
  geom_image(data = icons, aes(x = date, y = y, image = image), size = 0.1) +
  scale_y_continuous(
    name = "T°/Rain",
    limits = c(0,60),
    sec.axis = sec_axis(~ . / scaleFactor, name = "Eggs/trap")  # Secondary axis for mosquito counts
  ) +
  scale_color_manual(labels = c("Observations","Temperatures"), values = c("Temperature" = "orange", "Eggs/trap" = "darkgrey")) +  # Custom colors
  labs(x = "Date", color = "Legend") +
  theme_light() +
  ggtitle(th_site) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=9),
        text = element_text(size=10),
        plot.title = element_text(size=10),
        plot.margin = margin(5, 5, 5, 5)
        ) +
  scale_x_date(limits =c(as.Date("2023-01-01"),as.Date("2024-12-31")),
                               breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "3 month"),
               minor_breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "1 month"),
               date_labels = "%Y-%m")



return(p_th_site)

}

p_perols <- fun_plot_by_site("PEROLS")
p_murviels <- fun_plot_by_site("MURVIEL-LES-MONTPELLIER")
p_bayonne <- fun_plot_by_site("BAYONNE")
p_medard <- fun_plot_by_site("SAINT-MEDARD-EN-JALLES")



df <- df %>%
  mutate(TMN = ifelse(week %% 2 ==0, TMN, NA)) %>%
  mutate(RDF = ifelse(week %% 2 ==0, RFD, NA))

#df <- df %>% mutate(RFD = ifelse(year==2024, -RFD, RFD))

scaleFactor = 0.6

# Clean color palette for years
year_colors <- c("2023" = "#1f77b4", "2024" = "#ff7f0e")  # Adjust as needed

 ggplot(df, aes(x = week)) +
  # Rainfall bars
  geom_col(aes(y = RFD, fill = as.factor(year)),position = "dodge", alpha = 0.4, width = 0.6) +
  # Temperature line (dashed)
  ggalt::geom_xspline(aes(y = TMN, colour = as.factor(year)),size = 0.8, alpha = 0.7) +
  # Mosquito abundance: line + points (scaled)
  geom_point(aes(y = effectif_jour * scaleFactor, colour = as.factor(year)), shape = 16, size = 1.8, alpha = 0.7) +
  geom_line(data = df[!is.na(df$effectif_jour), ],
            aes(y = effectif_jour * scaleFactor, colour = as.factor(year)),
            size = 0.7) +
  # Historical temperature line
  #geom_xspline(data = df_meteofrance_historique, aes(x = week, y = TMN), color = "black", alpha = 0.4, size = 0.6, linetype = "longdash") +
  # Y-axis settings
  scale_y_continuous(
    name = "Temperature (°C) / Rainfall (mm)",
    limits = c(0, 60),
    sec.axis = sec_axis(~ . / scaleFactor, name = "Eggs per trap")
  ) +
  # Manual color/fill for clarity
  scale_color_manual(name = "Year", values = year_colors) +
  scale_fill_manual(name = "Year", values = year_colors) +
  # Theme and axis tweaks
  labs(x = "Week") +
  theme_light(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # Facet by site
  facet_wrap(~ site, ncol = 2)














 library(ggimage)

 icons <- data.frame(
   week = c(9, 22.5, 31, 40,49),
   y = c(55, 55, 55, 55,55),
   image =  c("no_mosquito.jpg", "spring.jpg",  "summer.jpg", "autumn.jpg", "no_mosquito.jpg")
 )

 df_monthly <- df %>%
   mutate(
     year = year(date),
     month = month(date),
     site = as.factor(site)
   ) %>%
   group_by(site, year, month) %>%
   summarise(
     TMN_monthly = mean(TMN, na.rm = TRUE),
     RFD_monthly = mean(RFD, na.rm = TRUE),
     .groups = "drop"
   )

 df_monthly <- df_monthly %>%
   mutate(week = case_when(
     month == 1 ~ 2,
     month == 2 ~ 6,
     month == 3 ~ 10,
     month == 4 ~ 14,
     month == 5 ~ 18,
     month == 6 ~ 24,
     month == 7 ~ 28,
     month == 8 ~ 32,
     month == 9 ~ 36,
     month == 10 ~ 40,
     month == 11 ~ 45,
     month == 12 ~ 50
   ))


 df$month <- cut(df$week,
                 breaks = c(0, 4, 8, 12, 17, 21, 26, 30, 35, 39, 44, 48, 53),
                 labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                 right = FALSE)


 # Clean color palette for years
 year_colors <- c("2023" = "#1f77b4", "2024" = "#ff7f0e")  # Adjust as needed

 scaleFactor = 0.6

 ggplot(df, aes(x = week)) +

   geom_vline(xintercept = 18, linetype = "dashed", size = 0.2) +
   geom_vline(xintercept = 27, linetype = "dashed", size = 0.2) +
   geom_vline(xintercept = 35, linetype = "dashed", size = 0.2) +
   geom_vline(xintercept = 44, linetype = "dashed", size = 0.2) +

#
#    annotate("text", x = 9, y = 55, label = "No activity", size = 3, alpha = 0.6) +
#    annotate("text", x = 22.5, y = 55, label = "Spring", size = 3, alpha = 0.6) +
#    annotate("text", x = 31, y = 55, label = "Summer", size = 3, alpha = 0.6) +
#    annotate("text", x = 40, y = 55, label = "Autumn", size = 3, alpha = 0.6) +
#    annotate("text", x = 49, y = 55, label = "No activity", size = 3, alpha = 0.6) +

   geom_image(data = icons, aes(x = week, y = y, image = image), size = 0.1) +
#
    # geom_rect(aes(xmin=0, xmax=18, ymin=-Inf, ymax=Inf), fill = "#8DA0CB", alpha=0.05, inherit.aes = FALSE) +
    # geom_rect(aes(xmin=18, xmax=27, ymin=-Inf, ymax=Inf), fill = "#66C2A5", alpha=0.05, inherit.aes = FALSE) +
    # geom_rect(aes(xmin=27, xmax=35, ymin=-Inf, ymax=Inf), fill = "#FC8D62", alpha=0.05, inherit.aes = FALSE) +
    # geom_rect(aes(xmin=35, xmax=44, ymin=-Inf, ymax=Inf), fill = "#E78AC3", alpha=0.05, inherit.aes = FALSE) +
    # geom_rect(aes(xmin=44, xmax=52, ymin=-Inf, ymax=Inf), fill = "#8DA0CB", alpha=0.05, inherit.aes = FALSE) +
   # Rainfall bars
   geom_col(data = df_monthly, aes(x = week, y = RFD_monthly, fill = as.factor(year)),position = "dodge", alpha = 0.4, width = 2) +
   # Temperature line (dashed)
   geom_line(data = df_monthly,
                       aes(x = week, y = TMN_monthly, group = year, color = as.factor(year)),
                       linetype = "dashed", size = 0.5, inherit.aes = FALSE) +
   geom_point(data = df_monthly,
             aes(x = week, y = TMN_monthly, group = year, color = as.factor(year)),
              size = 1, shape = 2, inherit.aes = FALSE) +

   # Mosquito abundance: line + points (scaled)
   geom_point(aes(y = effectif_jour * scaleFactor, colour = as.factor(year)), shape = 16, size = 1.8, alpha = 0.7) +
   geom_line(data = df[!is.na(df$effectif_jour), ],
             aes(y = effectif_jour * scaleFactor, colour = as.factor(year)),
             size = 0.7) +
   # Historical temperature line
   #geom_xspline(data = df_meteofrance_historique, aes(x = week, y = TMN), color = "black", alpha = 0.4, size = 0.6, linetype = "longdash") +
   # Y-axis settings
   scale_y_continuous(
     name = "Temperature (°C) / Rainfall (mm)",
     limits = c(0, 60),
     sec.axis = sec_axis(~ . / scaleFactor, name = "Eggs per trap")
   ) +
   scale_x_continuous(
     breaks = c(2, 6, 10, 15, 19, 24, 28, 33, 37, 42, 46, 51),
     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
   ) +
   # Manual color/fill for clarity
   scale_color_manual(name = "Year", values = year_colors) +
   scale_fill_manual(name = "Year", values = year_colors) +
   # Theme and axis tweaks
   labs(x = "Week") +
   theme_light(base_size = 11) +
   theme(
     legend.position = "bottom",
     legend.title = element_blank(),
     panel.grid.minor = element_blank()
   ) +
   # Facet by site
   facet_wrap(~ site, ncol = 2)





















## Données pièges

df_pieges <- read.csv( file.path("data","processed","df_pieges.csv")) %>%
  rename(nom_commune = site) %>%
  mutate(date_releve = as.Date(date_releve)) %>%
  mutate(week = week(date_releve), year = year(date_releve)) %>%
  group_by(nom_commune,year,week) %>%
  summarise(effectif_jour_mean=mean(effectif_jour, na.rm = T), effectif_jour_sd = sd(effectif_jour, na.rm = T)) %>%
  filter(!is.na(year)) %>%
  mutate(nom_commune = as.factor(nom_commune)) %>%
  mutate(nom_commune= forcats::fct_relevel(nom_commune, "MURVIEL-LES-MONTPELLIER", "PEROLS" ,"BAYONNE","SAINT-MEDARD-EN-JALLES","RENNES" ))


## Données météo

df_meteofrance_historique <- read.csv("data/processed/data_meteofrance_historique.csv") %>%
  mutate(date=as.Date(date), year = year(date), week =  week(date)) %>%
  group_by(nom_commune,year, week) %>%
  summarise(RFD = sum(RR, na.rm = T), TMN = mean(TM, na.rm = T), TMIN = mean( TN , na.rm = T), TMAX = mean(TX, na.rm = T)) %>%
  group_by(nom_commune,week) %>%
  summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
  #RFD_sd = sd(RFD, na.rm = T), TMN_sd = sd(TMN, na.rm = T), TMIN_sd = sd(TMIN, na.rm = T), TMAX_sd = sd(TMAX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = "moy. 1950-2022")


df_meteofrance_2023_2024 <-  read.csv("data/processed/data_meteofrance_2022_2024.csv") %>%
  mutate(date = as.Date(date), year = year(date), week =  week(date)) %>%
  filter(date >= "2023-01-01") %>%
  group_by(nom_commune,year, week) %>%
  summarise(RFD = sum(RR, na.rm = T), TMN = mean(TM, na.rm = T), TMIN = mean( TN , na.rm = T), TMAX = mean(TX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = as.character(year))


df_meteofrance_proj <-  read.delim("data/processed/tasmintasmaxtasprtothusssfcwind_France_CNRM-CERFACS-CNRM-CM5_CNRM-ALADIN63_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20300101-21001231.txt", skip = 64, sep = ",", col.names = c("Date", "Latitude", "Longitude",  "tasminAdjust", "tasmaxAdjust" ,"tasAdjust", "prtotAdjust" ,"hussAdjust", "sfcWindAdjust"),  na.strings = "-999.99") %>%
  mutate(date = parse_date_time(Date,"ymd"), week = week(date), year = year(date)) %>%
  mutate(nom_commune = case_when(Longitude == -1.46695 ~ "BAYONNE",
                                 Longitude == 3.67372 ~ "MURVIEL-LES-MONTPELLIER" ,
                                 Longitude == 3.96859 ~ "PEROLS" ,
                                 Longitude == -0.75110 ~ "SAINT-MEDARD-EN-JALLES" ,
                                 Longitude == 3.87167 ~ "MONTPELLIER",
                                 Longitude == -1.69223 ~ "RENNES")) %>%
  group_by(nom_commune,year, week) %>%
  summarise(RFD = sum(prtotAdjust, na.rm = T), TMN = mean(tasAdjust, na.rm = T), TMIN = mean(tasminAdjust, na.rm = T), TMAX = mean(tasmaxAdjust, na.rm = T)) %>%
  group_by(nom_commune,week) %>%
  summarise(RFD = mean(RFD, na.rm = T), TMN = mean(TMN, na.rm = T), TMIN = mean(TMIN, na.rm = T), TMAX = mean(TMAX, na.rm = T)) %>%
  mutate(RFDcum = cumsum(RFD)) %>%
  mutate(year = "proj. 2030-2100 (scenario rcp4.5)")


df_meteofrance <- rbind(df_meteofrance_historique,df_meteofrance_2023_2024,df_meteofrance_proj) %>%
  mutate(year = factor(year, levels = c("2023", "2024", "moy. 1950-2022","proj. 2030-2100 (scenario rcp4.5)"))) %>%
  mutate(nom_commune = as.factor(nom_commune)) %>%
  mutate(nom_commune= forcats::fct_relevel(nom_commune, "MONTPELLIER", "MURVIEL-LES-MONTPELLIER", "PEROLS" ,"BAYONNE","SAINT-MEDARD-EN-JALLES","RENNES" ))




## all together

df_pieges2 <- df_pieges %>%
  ungroup() %>%
  #mutate(effectif_jour_mean = scales::rescale(effectif_jour_mean, to=c(0,1))) %>%
  dplyr::select(-effectif_jour_sd) %>%
  pivot_wider(names_from = year, values_from = effectif_jour_mean) %>%
  rename(effectif_jour_2023=`2023`,effectif_jour_2024=`2024`)

df_meteofrance2 <- df_meteofrance %>%
  ungroup() %>%
  pivot_wider(names_from = year, values_from = c(RFD ,  TMN,  TMIN,  TMAX, RFDcum )) #%>%
  #mutate_at(3:ncol(.), funs(c(scales::rescale(., to=c(0,1)))))


df2 <- df_meteofrance2 %>%
  filter(!(nom_commune=="MONTPELLIER")) %>%
  left_join(df_pieges2) %>%
  pivot_longer(!c(nom_commune,week)) %>%
  filter(!is.na(value))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## Nb oeufs capturés en fonction du temps
## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

ggplot(df_pieges, aes(x = as.factor(week), group = as.factor(year))) +
  geom_line(aes(y = effectif_jour_mean, color = as.factor(year))) +
  #geom_ribbon(aes(y = effectif_jour_mean, ymin = effectif_jour_mean - effectif_jour_sd, ymax = effectif_jour_mean + effectif_jour_sd, fill =  as.factor(year)), alpha = .05) +
  facet_wrap(.~nom_commune) +
  theme_bw() + # theme_classic()
  theme(panel.grid = element_blank())





ggplot() +
  geom_line(data = df2 %>% filter(name %in% c("effectif_jour_2023","effectif_jour_2024","TMN_2024","TMN_2023")), aes(x = week, y = value, group = as.factor(name), color = as.factor(name))) +
  #stat_smooth(data = df2 %>% filter(name %in% c("RFD_2024","RFD_2023")), aes(x = week, y = value, group = as.factor(name), color = as.factor(name)), method = "gam", se = FALSE, linewidth = 1) +
  facet_wrap(.~nom_commune) +
  theme_bw() +
  theme(panel.grid = element_blank())


ggplot() +
  geom_line(data = df2 %>% filter(name %in% c("effectif_jour_2023","effectif_jour_2024","TMN_2024","TMN_2023")), aes(x = week, y = value, group = as.factor(name), color = as.factor(name))) +
  #stat_smooth(data = df2 %>% filter(name %in% c("RFD_2024","RFD_2023")), aes(x = week, y = value, group = as.factor(name), color = as.factor(name)), method = "gam", se = FALSE, linewidth = 1) +
  facet_wrap(.~nom_commune) +
  theme_bw() +
  theme(panel.grid = element_blank())










## Boxplots larves ~ conditions météo
df_meteo <- read.csv("data_meteofrance/data_meteofrance_2022_2024.csv")  %>%
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



df_meteo <- read.csv("data_meteofrance/data_meteofrance_2023_2024.csv")  %>%
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





df_meteo <- read.csv("data_meteofrance/data_meteofrance_2023_2024.csv")  %>%
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







df_meteo <- read.csv("data_meteofrance/data_meteofrance_2023_2024.csv")  %>%
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
