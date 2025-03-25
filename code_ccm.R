library(tidyverse)
library(furrr)
library(patchwork)

df_model <- read.csv(file.path("data","processed","df_to_model.csv"))

# grouper à l'échelle de la ville-semaine de collecte :
df_model <- df_model %>%
  relocate(effectif_jour,.before = RR_0_0) %>%
  group_by(site, Year,week) %>%
  summarise_at(vars(effectif_jour:RFNO), mean, na.rm = TRUE) %>%
  ungroup()


########################
## modélisation bivariée
########################

# open dataset containing the dependant and independent variables

df_model <-  df_model %>%
  mutate(presence_oeufs = ifelse(effectif_jour>0,1,0)) %>%
  relocate(presence_oeufs, .after = effectif_jour) %>%
  filter(!is.na(presence_oeufs))


 df_model_allsites <- df_model %>%
   mutate(site = "TOUS SITES")

  df_model <- bind_rows(df_model,df_model_allsites)

df_model <- df_model %>%
  filter(site!="RENNES") %>%
  mutate(environment = case_when(site %in% c( "MURVIEL-LES-MONTPELLIER", "PEROLS") ~ "MEDITERRANEAN",
                                   site %in% c( "BAYONNE", "SAINT-MEDARD-EN-JALLES") ~ "ATLANTIC"))

predictors <- setdiff(colnames(df_model), c("presence_oeufs", "date_releve", "week", "Year", "Mois_numeric", "num_piege", "Latitude", "Longitude", "site", "effectif_jour", "Mois", "saison", "date_year", "num_releve", "idpointdecapture"))


## few lines of codes, but very long to run :
# presence
# df_model %>%
#   group_by(site) %>%
#   correlation(select = c("presence_oeufs"),
#               select2 = predictors,
#               method = "distance")
#
# # abundance
# df_model %>%
#   filter(effectif_jour>0) %>%
#   group_by(site) %>%
#   correlation(select = c("effectif_jour"),
#               select2 = predictors,
#               method = "distance")


## more code, but much faster :

fun_compute_correlation_univ <- function(df,indicator){

  if(indicator == "presence"){
    var_to_keep = "presence_oeufs"
  } else if (indicator == "abundance"){
    var_to_keep = "effectif_jour"
  }

    func <- function(x){
      df2 <- df %>% dplyr::select(var_to_keep,!!x)
      ret <- correlation::correlation(df2,method = "distance")
      return(ret)
    }

  possible_a <- possibly(func, otherwise = NA_real_)

  spearman_univs <- furrr::future_map(colnames(df[6:ncol(df)]), possible_a)

  spearman_univs <- do.call(rbind.data.frame, spearman_univs)

  spearman_univs$site <- unique(df$site)
  #spearman_univs$environment <- unique(df$environment)


  return(spearman_univs)
}


# presence
corr_univ_presence <- df_model %>%
  group_split(site) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "presence")) %>%
  as.tibble() %>%
  mutate(indicator = "presence") %>%
  mutate(r = ifelse(r<0,0,r))

# abundance
corr_univ_abundance <- df_model %>%
  filter(effectif_jour>0) %>%
  group_split(site) %>%
  map_dfr(.,~fun_compute_correlation_univ(., "abundance")) %>%
  as.tibble() %>%
  mutate(indicator = "abundance") %>%
  mutate(r = ifelse(r<0,0,r))





################v
## plotting
##############v

# function to plot the CCM (simple plot : only the CCM)
fun_ccm_plot2 <- function(correlation_df, var){

  if(length(unique(correlation_df$correlation))!=1){ # to deal with case all correlation values are NAs
    most_corr <- correlation_df %>% filter(correlation == max(correlation, na.rm = T))
    most_corr2 <- correlation_df %>% arrange(desc(correlation)) %>% filter(correlation >= most_corr$correlation * 0.9)
  } else {
    most_corr <- most_corr2 <- correlation_df[1,]
  }

  ccm_plot <- ggplot(data = correlation_df, aes(time_lag_1, time_lag_2, fill = correlation)) +
    geom_tile(color = "white", show.legend = TRUE, size = 0.05,aes(width=1, height=1)) +
    geom_tile(data = most_corr2 , color = "black", size = 0.2, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    geom_tile(data = most_corr , color = "deeppink3", size = 0.6, show.legend = FALSE,aes(width=1, height=1)) +  # ,aes(width=1, height=1)
    theme_minimal() +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.title = element_text(size = 8),
          legend.key.size = unit(0.8, "cm"),
          legend.title=element_text(size=10),
          legend.position = "none"
    ) +
    ggtitle(var) +
    annotate("text", size = 3,x = min(correlation_df$time_lag_1), y = max(correlation_df$time_lag_2), vjust = "inward", hjust = "inward", label = paste0("r(",most_corr$time_lag_2,",",most_corr$time_lag_1,") = ",round(most_corr$correlation,2))) +
    coord_fixed() +
    ylab("time lag 1") +
    xlab("time lag 2") +
    scale_fill_gradient2(low = "white", high = "red", limit = c(0,1), space = "Lab", name = "Distance correlation", na.value = "grey")


  return(ccm_plot)

}




univ_spearman_temporal_mf <- corr_univ_presence %>%
  bind_rows(corr_univ_abundance) %>%
  filter(Parameter2!="RFNO") %>%
  mutate(var = sub('\\_.*', '', Parameter2)) %>%
  mutate(label = case_when(var == "RR" ~ "Cumulated rainfall",
                           var == "RRMAX" ~ "Maximum rainfall ",
                           var == "DRR" ~ "Rainfall duration",
                           var == "TN" ~ "Minimum temperature",
                           var == "TX" ~ "Maximum temperature",
                           var == "TM" ~ "Average temperature",
                           var == "TAMPLI" ~ "Temperature amplitude",
                           var == "FFM" ~ "Average Wind speed",
                           var == "FXY" ~ "Max Wind speed",
                           var == "UM" ~ "Relative humidity",
                           var == "GDDjour" ~ "GDDjour",
                           var == "GDDacc" ~ "GDDacc",
                           var == "GDDbound" ~ "GDDbound")) %>%
  mutate(time_lag_1 = as.numeric(sub('.*\\_', '', Parameter2)), time_lag_2 = as.numeric(stringr::str_match( Parameter2, '([^_]+)(?:_[^_]+){1}$')[,2])) %>%
  arrange(site,var, indicator, time_lag_1, time_lag_2) %>%
  rename(correlation = r) %>%
  mutate(correlation = ifelse(p<=0.2,correlation,NA)) %>%
  nest(-c(site,indicator,var))


plots_univ_spearman_temporal_mf <- univ_spearman_temporal_mf %>%
  arrange(rev(indicator),factor(var, levels = c("TM","TN","TX","TAMPLI","GDDjour","GDDacc","GDDbound","UM","RR","RRMAX","DRR","FFM","FXY")),factor(site, levels = c("TOUS SITES", "MURVIEL-LES-MONTPELLIER", "PEROLS" ,"BAYONNE","SAINT-MEDARD-EN-JALLES"))) %>%
  #arrange(rev(indicator),factor(var, levels = c("TM","TN","TX","TAMPLI","GDDjour","GDDacc","GDDbound","UM","RR","RRMAX","DRR","FFM","FXY")),factor(environment, levels = c("MEDITERRANEAN","ATLANTIC"))) %>%
  dplyr::filter(!var %in% c("GDDjour","GDDacc","GDDbound","TAMPLI","FXY","DRR")) %>%
  mutate(univ_temporal = pmap(list(data,indicator), ~fun_ccm_plot2(correlation_df = ..1, var = ..1$label[1]))) %>%
  nest(-c(site,indicator)) %>%
  mutate(univ_temporal = map(data, ~patchwork::wrap_plots(.x$univ_temporal, nrow = 1, ncol = 7))) %>%
  mutate(univ_temporal = pmap(list(univ_temporal,site), ~..1 + patchwork::plot_annotation(title = ..2))) %>%
  dplyr::select(-data)






p_meteo_presence <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[1:5], ncol = 1, nrow = 5)
p_meteo_abundance <- patchwork::wrap_plots(plots_univ_spearman_temporal_mf$univ_temporal[6:10], ncol = 1, nrow = 5)
