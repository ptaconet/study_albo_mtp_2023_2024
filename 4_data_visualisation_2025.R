library(tidyverse)
library(patchwork)

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
  mutate(site = fct_relevel(site, c("PEROLS", "MURVIEL-LES-MONTPELLIER", "BAYONNE","SAINT-MEDARD-EN-JALLES" ,  "RENNES" )))

df <- df %>% filter(site %in% c("PEROLS","MURVIEL-LES-MONTPELLIER","SAINT-MEDARD-EN-JALLES","BAYONNE"))

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


