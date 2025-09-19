library(tidyverse)

df_model <- read.csv(file.path("data","processed","df_meteo_predictions_2025.csv"))   %>% filter(site %in% c("PEROLS","MURET"))

multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting.rds")

df_model_pred <- df_model
df_model_pred$pred_presence_prob <- predict(multiv_model_presence_nowcasting$model, df_model_pred, type = 'prob')$Presence
df_model_pred$pred_presence_absence <- predict(multiv_model_presence_nowcasting$model, df_model_pred)

df_model_pred$DOY = cos(lubridate::yday(df_model_pred$date))

# with svmpoly
# df_model_pred_abundance <- df_model_pred %>% dplyr::select(TM_0_4, UM_0_11, RR_1_5)
# df_model_pred <- df_model_pred %>%
#   mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
#   mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,df_model_pred_abundance)))) %>%
#   mutate(date=as.Date(date))

# with ranger

df_model_pred <- df_model_pred %>%
  mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
  mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,.)))) %>%
  mutate(date=as.Date(date))











df_pieges <- read.csv(file.path("data","processed","df_pieges_2025.csv")) %>%
  rename(site = Site) %>%
  group_by(site, daterec) %>%
  summarise(effectif_jour = mean(effectif_jour))   %>% filter(site %in% c("PEROLS","MURET")) %>%
  ungroup() %>%
  mutate(daterec=as.Date(daterec))


df_model_pred <- communes_abundance %>% filter(libgeo %in% c("Pérols","Muret")) %>% mutate(libgeo = ifelse(libgeo=="Pérols","PEROLS","MURET")) %>% rename(site = libgeo, pred_abundance = mean_abundance)

#dates_canicule <- data.frame(debut = as.Date(c("2025-06-27","2025-08-08")), fin = as.Date(c("2025-07-05","2025-08-18")))

ggplot() +
  geom_line(data = df_pieges, aes(x=daterec, y = effectif_jour, group = site), color = "black") +
  geom_line(data = df_model_pred, aes(x=date, y = pred_abundance, group = site), color = "red") +
  geom_point(data = df_pieges, aes(x=daterec, y = effectif_jour, group = site), color = "black", size = 1) +
  geom_point(data = df_model_pred, aes(x=date, y = pred_abundance, group = site), color = "red", size = 1) +
  facet_wrap(.~site, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  # geom_rect(
  #   data = dates_canicule,
  #   aes(xmin = debut, xmax = fin, ymin = -Inf, ymax = Inf),
  #   inherit.aes = FALSE,
  #   alpha = 0.2,
  #   fill = "red"
  # ) +
  scale_x_date(limits = c(as.Date("2024-10-01"),as.Date("2025-09-15")),
               breaks = seq(as.Date("2024-10-01"), as.Date("2025-09-15"), by = "1 month"),
               minor_breaks = seq(as.Date("2024-10-01"), as.Date("2025-09-15"), by = "1 month"),
               date_labels = "%Y-%m-%d")


######### explaining ##########

library(lime)

meteo <- read.csv(file.path("data","processed","df_meteo_predictions_2025.csv")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(week = week(date), Year = year(date)) %>%
  dplyr::filter(date>=as.Date("2025-01-01"))


model_presence_nowcasting <- multiv_model_presence_nowcasting[[1]] #### sum up of presence model
df_cv_presence_nowcasting <- multiv_model_presence_nowcasting[[2]]#### data frame with prediction
df_mod_presence_nowcasting <- multiv_model_presence_nowcasting[[3]] #### data frame which was used to build the model

model_abundance_nowcasting <- multiv_model_abundance_nowcasting[[1]] #### sum up of abundance model
df_cv_abundance_nowcasting <- multiv_model_abundance_nowcasting[[2]] #### data frame with prediction
df_mod_abundance_nowcasting <- multiv_model_abundance_nowcasting[[3]] #### data frame which was used to build the model


variables_presence <- c("TM_0_8","UM_5_11")
variables_abundance <-  c("TM_0_4","UM_0_11","RR_1_5")

fun_get_lime <- function(th_site){

  df_mod_presence_nowcasting_lime <- df_mod_presence_nowcasting %>% dplyr::select(variables_presence,"site")
  explainer_presence <- lime(df_mod_presence_nowcasting_lime, model_presence_nowcasting, n_bins = 6)

  df_mod_abundance_nowcasting_lime <- df_mod_abundance_nowcasting %>% dplyr::select(variables_abundance,"site")
  explainer_abundance <- lime(df_mod_abundance_nowcasting_lime, model_abundance_nowcasting, n_bins = 6)

  # x_presence = df_mod_presence_nowcasting %>%
  #   filter(site==th_site)

  x_presence = meteo %>%
    filter(site==th_site) %>%
    filter(!is.na(UM_5_11))

  explanation_presence <- explain(
    x = x_presence  %>% dplyr::select(variables_presence,"site"),
    explainer = explainer_presence,
    n_permutations = 5000,
    dist_fun = "gower",
    kernel_width = NULL,
    n_features = 10,
    feature_select = "highest_weights",
    labels = "Presence"
  )

  x_presence <- x_presence %>%
    slice(rep(1:n(), each = length(c(variables_presence,"site")))) %>%
    #slice(rep(1:n(), each = length(variables_presence))) %>%
    dplyr::select(site, Year, week)

  explanation_presence <- explanation_presence %>%
    bind_cols(x_presence) %>%
    mutate(model = "presence")


  # abundance

  # x_abundance = df_mod_abundance_nowcasting %>%
  #   filter(site==th_site)

  x_abundance = meteo %>%
    filter(site==th_site)

  explanation_abundance <- explain(
    x = x_abundance  %>% dplyr::select(variables_abundance,"site"),
    explainer = explainer_abundance,
    n_permutations = 5000,
    dist_fun = "gower",
    kernel_width = NULL,
    n_features = 10,
    feature_select = "highest_weights")

  x_abundance <- x_abundance %>%
    slice(rep(1:n(), each = length(c(variables_abundance,"site")))) %>%
    #slice(rep(1:n(), each = length(variables_abundance))) %>%
    dplyr::select(site, Year, week)

  explanation_abundance <- explanation_abundance %>%
    bind_cols(x_abundance) %>%
    mutate(model = "abundance")



  ## mix presence and abundance

  # a = unique(x_abundance[c("Year", "week")])
  # a$is_pres <- TRUE
  # explanation_presence <- explanation_presence %>%
  #   left_join(a) %>%
  #   filter(is.na(is_pres))

  explanation_presence <- explanation_presence %>% filter(explanation_presence$label_prob<0.5)
  a = unique(explanation_presence[c("Year", "week")])
  a$is_pres <- TRUE

  explanation_abundance <- explanation_abundance %>%
    left_join(a) %>%
    filter(is.na(is_pres))

  explanation_tot <- rbind(explanation_presence[,c("site","Year","week","model","feature","feature_value","feature_weight","feature_desc")],
                           explanation_abundance[,c("site","Year","week","model","feature","feature_value","feature_weight","feature_desc")])

  explanation_tot$date <- as.Date(paste(explanation_tot$Year, explanation_tot$week, 1, sep = "-"), "%Y-%U-%u")

  return(explanation_tot)

}

perols <- fun_get_lime("PEROLS")
murviel <- fun_get_lime("MURVIEL-LES-MONTPELLIER")
bayonne <- fun_get_lime("BAYONNE")
medard <- fun_get_lime("SAINT-MEDARD-EN-JALLES")
muret <- fun_get_lime("MURET")


plot_lime_v1 <- function(explanation){

  explanation <- explanation %>%
    mutate(feature_char = case_when(grepl("UM", feature) ~ "Humidity",
                                    grepl("TM", feature) ~ "Temperature",
                                    grepl("site", feature) ~ "site",
                                    grepl("RR", feature) ~ "Rainfall",
                                    grepl("FFM", feature) ~ "Wind")) %>%
    #mutate(feature_char = fct_relevel(feature_char, rev(c("Temperature","Rainfall","Humidity","Wind","site")))) %>%
    mutate(feature_char = fct_relevel(feature_char, c("Temperature","Rainfall","Humidity","Wind","site"))) %>%
    filter(feature_char!="site") %>%
    mutate(feature_weight = ifelse(feature_weight < -2.5, -2.5 ,feature_weight))

  explanation$feature_desc <- factor(explanation$feature_desc,levels = unique(explanation$feature_desc[order(as.numeric(explanation$feature_value))]))


  p1 <- ggplot(explanation, aes_(~date, ~feature_desc)) +
    geom_tile(aes_(fill = ~feature_weight)) +
    geom_vline(xintercept = c(as.Date(paste(2023,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u"),as.Date(paste(2024,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u")), linetype = "dashed", size = 0.2) +
    scale_y_discrete("Feature",expand = c(0, 0)) +
    #scale_fill_gradient2("Feature weight",low = "firebrick", mid = "#f7f7f7", high = "steelblue",  limit = c(-2,2.5), n.breaks = 5, labels = c("Important - reduce","","no weight","","Important - raises")) +
    scale_fill_gradientn(
      name = "Feature contribution",
      colours = c("firebrick", "#f7f7f7", "steelblue"),
      values = scales::rescale(c(-2.5, -0.75, 0, 0.75, 2.5)),  # non-linear steps
      limits = c(-2.5, 2.5),
      breaks = c(-2, -0.4, 0, 0.4, 2),
      labels = c("Important - negative", "", "no contribution", "", "Important - positive")
    ) +
    theme_light() +
    facet_grid(rows = vars(feature_char), space="free", scales = "free_y") +
    theme(panel.border = element_rect(fill = NA,colour = "grey60", size = 0.5),
          #panel.grid = element_blank(),
          legend.position = "right",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          #axis.text.x=element_blank(),
          #axis.ticks.x=element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=10),
          plot.margin = margin(5, 5, 5, 5),
          strip.text = element_text(size = rel(1.2))
    ) +
    scale_x_date(limits = c(as.Date("2025-01-01"),as.Date("2025-09-15")),
                 breaks = seq(as.Date("2025-01-01"), as.Date("2025-09-15"), by = "1 month"),
                 minor_breaks = seq(as.Date("2025-01-01"), as.Date("2025-09-15"), by = "1 month"),
                 date_labels = "%Y-%m")


  return(p1)
}


plot_lime_v2 <- function(explanation){

  explanation <- explanation %>%
    mutate(feature_char = case_when(grepl("UM", feature) ~ "Humidity",
                                    grepl("TM", feature) ~ "Temperature",
                                    grepl("site", feature) ~ "site",
                                    grepl("RR", feature) ~ "Rainfall",
                                    grepl("FFM", feature) ~ "Wind")) %>%
    mutate(feature_char = fct_relevel(feature_char, rev(c("Temperature","Rainfall","Humidity","Wind","site")))) %>%
    filter(feature_char!="site") %>%
    mutate(feature_weight = ifelse(feature_weight < -2.5, -2.5 ,feature_weight))

  explanation$feature_desc <- factor(explanation$feature_desc,levels = unique(explanation$feature_desc[order(as.numeric(explanation$feature_value))]))


  p1 <- ggplot(explanation, aes_(~date, ~feature_char)) +
    geom_tile(aes_(fill = ~feature_weight)) +
    geom_vline(xintercept = c(as.Date(paste(2023,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u"),as.Date(paste(2024,  c(18, 27, 35, 44 ), 1, sep = "-"), "%Y-%U-%u")), linetype = "dashed", size = 0.2) +
    scale_y_discrete("Feature",expand = c(0, 0)) +
    #scale_fill_gradient2("Feature weight",low = "firebrick", mid = "#f7f7f7", high = "steelblue",  limit = c(-2,2.5), n.breaks = 5, labels = c("Important - reduce","","no weight","","Important - raises")) +
    scale_fill_gradientn(
      name = "Feature contribution",
      colours = c("firebrick", "#f7f7f7", "steelblue"),
      values = scales::rescale(c(-2.5, -0.75, 0, 0.75, 2.5)),  # non-linear steps
      limits = c(-2.5, 2.5),
      breaks = c(-2, -0.4, 0, 0.4, 2),
      labels = c("Important - negative", "", "no contribution", "", "Important - positive")
    ) +
    theme_light() +
    theme(panel.border = element_rect(fill = NA,colour = "grey60", size = 0.5),
          panel.grid = element_blank(),
          legend.position = "right",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.text=element_text(size=9),
          text = element_text(size=10),
          plot.margin = margin(5, 5, 5, 5),
          strip.text = element_text(size = rel(1.2))
    ) +
    scale_x_date(limits = c(as.Date("2025-01-01"),as.Date("2025-09-15")),
                 breaks = seq(as.Date("2025-01-01"), as.Date("2025-09-15"), by = "1 month"),
                 minor_breaks = seq(as.Date("2025-01-01"), as.Date("2025-09-15"), by = "1 month"),
                 date_labels = "%Y-%m")


  return(p1)
}

plot_lime_v1(perols)
plot_lime_v1(murviel)
plot_lime_v1(bayonne)
plot_lime_v1(medard)
