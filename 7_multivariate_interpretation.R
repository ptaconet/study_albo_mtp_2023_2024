########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(iml) ## Version '0.11.3'
library(patchwork) ## Version ‘1.2.0.9000’
library(precrec) ## Version ‘0.14.4’

########################### Open dataset containing the results of presence and abundance models

multiv_model_presence_explanatory <- readRDS("res_multiv_model_presence_explanatory.rds")
multiv_model_abundance_explanatory <- readRDS("res_multiv_model_abundance_explanatory.rds")

multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting.rds")

multiv_model_presence_forecasting <- readRDS("res_multiv_model_presence_forecasting_llo.rds")
multiv_model_abundance_forecasting <- readRDS("res_multiv_model_abundance_forecasting_llo.rds")

model_presence_explanatory <- multiv_model_presence_explanatory[[1]] #### sum up of presence model
df_cv_presence_explanatory <- multiv_model_presence_explanatory[[2]]#### data frame with prediction
df_mod_presence_explanatory <- multiv_model_presence_explanatory[[3]] #### data frame which was used to build the model

model_abundance_explanatory <- multiv_model_abundance_explanatory[[1]] #### sum up of abundance model
df_cv_abundance_explanatory <- multiv_model_abundance_explanatory[[2]] #### data frame with prediction
df_mod_abundance_explanatory <- multiv_model_abundance_explanatory[[3]] #### data frame which was used to build the model

model_presence_nowcasting <- multiv_model_presence_nowcasting[[1]] #### sum up of presence model
df_cv_presence_nowcasting <- multiv_model_presence_nowcasting[[2]]#### data frame with prediction
df_mod_presence_nowcasting <- multiv_model_presence_nowcasting[[3]] #### data frame which was used to build the model

model_abundance_nowcasting <- multiv_model_abundance_nowcasting[[1]] #### sum up of abundance model
df_cv_abundance_nowcasting <- multiv_model_abundance_nowcasting[[2]] #### data frame with prediction
df_mod_abundance_nowcasting <- multiv_model_abundance_nowcasting[[3]] #### data frame which was used to build the model


model_presence_forecasting <- multiv_model_presence_forecasting[[1]] #### sum up of presence model
df_cv_presence_forecasting <- multiv_model_presence_forecasting[[2]]#### data frame with prediction
df_mod_presence_forecasting <- multiv_model_presence_forecasting[[3]] #### data frame which was used to build the model

model_abundance_forecasting <- multiv_model_abundance_forecasting[[1]] #### sum up of abundance model
df_cv_abundance_forecasting <- multiv_model_abundance_forecasting[[2]] #### data frame with prediction
df_mod_abundance_forecasting <- multiv_model_abundance_forecasting[[3]] #### data frame which was used to build the model



df_cv_presence_explanatory2 <- df_cv_presence_explanatory %>%
  mutate(pred_stat_presence_explanatory = ifelse(pred_final == "Presence",1,0)) %>%
  dplyr::select(site,week,Year,pred_stat_presence_explanatory)

df_cv_presence_nowcasting2 <- df_cv_presence_nowcasting %>%
  mutate(pred_stat_presence_nowcasting = ifelse(pred_final == "Presence",1,0)) %>%
  dplyr::select(site,week,Year,pred_stat_presence_nowcasting)

df_cv_presence_forecasting2 <- df_cv_presence_forecasting %>%
  mutate(pred_stat_presence_forecasting = ifelse(pred_final == "Presence",1,0)) %>%
  dplyr::select(site,week,Year,pred_stat_presence_forecasting)


df_cv_abundance_explanatory2 <- df_cv_abundance_explanatory %>%
  mutate(pred_stat_abundance_explanatory = exp(pred)) %>%
  dplyr::select(site,week,Year,pred_stat_abundance_explanatory)

df_cv_abundance_nowcasting2 <- df_cv_abundance_nowcasting %>%
  mutate(pred_stat_abundance_nowcasting = exp(pred)) %>%
  dplyr::select(site,week,Year,pred_stat_abundance_nowcasting)

df_cv_abundance_forecasting2 <- df_cv_abundance_forecasting %>%
  mutate(pred_stat_abundance_forecasting = exp(pred)) %>%
  dplyr::select(site,week,Year,pred_stat_abundance_forecasting)

df_cv_paul <- df_cv_presence_explanatory2 %>%
  left_join(df_cv_presence_nowcasting2) %>%
  left_join(df_cv_presence_forecasting2) %>%
  left_join(df_cv_abundance_explanatory2) %>%
  left_join(df_cv_abundance_nowcasting2) %>%
  left_join(df_cv_abundance_forecasting2) %>%
  mutate(pred_final_explanatory = ifelse(pred_stat_presence_explanatory==0, pred_stat_presence_explanatory,pred_stat_abundance_explanatory)) %>%
  mutate(pred_final_nowcasting = ifelse(pred_stat_presence_nowcasting==0, pred_stat_presence_nowcasting,pred_stat_abundance_nowcasting)) %>%
  mutate(pred_final_forecasting = ifelse(pred_stat_presence_forecasting==0, pred_stat_presence_forecasting,pred_stat_abundance_forecasting)) %>%
  dplyr::select(site,week,Year,pred_final_explanatory,pred_final_nowcasting,pred_final_forecasting) %>%
  rename(pred_stat_explanatory = pred_final_explanatory, pred_stat_nowcasting = pred_final_nowcasting, pred_stat_forecasting = pred_final_forecasting)

df_cv_paul <- df_cv_paul %>%
  dplyr::select(-pred_stat_explanatory)

# data andrea
load("dataframe_Metelmann_Arbocarto_Andrea.RData") # abundance_tot_df

df_cv_andrea <- abundance_tot_df %>%
  rename(Year = year) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  filter(!(site %in% c("MONTPELLIER","TOULOUSE")), Year>2022)

df <- df_cv_andrea %>%
  left_join(df_cv_paul) %>%
  filter(site!="RENNES")

df %>%
  dplyr::select(-pred_stat_forecasting) %>%
  add_row(site = "BAYONNE", Year = 2024, week = 1, obs = NA, pred_metelmann = NA, pred_arbocarto = NA, pred_stat_nowcasting = NA) %>%
  add_row(site = "SAINT-MEDARD-EN-JALLES", Year = 2024, week = 1, obs = NA, pred_metelmann = NA, pred_arbocarto = NA, pred_stat_nowcasting = NA) %>%
  rename(obs_mathematical = obs) %>%
  mutate(obs_statistical = obs_mathematical) %>%
  group_by(site) %>%
  mutate_at(c("obs_mathematical","pred_metelmann", "pred_arbocarto"), funs(c(scales::rescale(., to = c(0, 1))))) %>%
  ungroup() %>%
  pivot_longer(-c('site', 'Year', 'week')) %>%
  mutate(date = as.Date(paste(Year, week, 1, sep = "-"), "%Y-%U-%u")) %>%
  mutate(model = case_when(
    name %in% c("obs_mathematical","pred_metelmann", "pred_arbocarto") ~ 'Mathematical models',
    #name %in% c("obs_statistical", "pred_stat_nowcasting","pred_stat_forecasting") ~ 'Statistical models'  )) %>%
    name %in% c("obs_statistical", "pred_stat_nowcasting") ~ 'Machine learning model'  )) %>%
  mutate(model = fct_relevel(model, c('Mathematical models','Machine learning model'))) %>%
  # Duplicate "obs" so it appears in both "mathematical" and "statistical" facets
  mutate(site = fct_relevel(site, c( "PEROLS", "MURVIEL-LES-MONTPELLIER",  "BAYONNE","SAINT-MEDARD-EN-JALLES" , "RENNES" ))) %>%
  ggplot(aes(x = date, y = value, color = name, group = name, size = ifelse(name %in% c("obs_mathematical","obs_statistical"), 0.6, 0.5))) +
  geom_line() +
  geom_point(size = 0.4) +
  #ggh4x::facet_grid2(cols = vars(model), rows = vars(site), scales = "free_y", independent = "y") +
  facet_grid(cols = vars(site), rows = vars(model), scales = "free_y") +
 scale_color_manual(values = c(
    "obs_mathematical" = "#49423c",  # Observed values in black
    "obs_statistical" = "#49423c",  # Observed values in black
    "pred_arbocarto" = "#6a994e",  # Similar blue for Arbocarto
    "pred_metelmann" = "#a7c957",  # Slightly different but close blue for Metelman
    #"pred_stat_explanatory" = "#E69F00",
    "pred_stat_nowcasting" = "#457b9d",
    "pred_stat_forecasting" = "#a8dadc"
    # "pred_arbocarto" = "#bc6c25",  # Similar blue for Arbocarto
    # "pred_metelmann" = "#2DD881",  # Slightly different but close blue for Metelman
    # "pred_stat_nowcasting" = "#E64F90",
    # "pred_stat_forecasting" = "#E69F00"
  ),
  labels = c(
    "obs_mathematical" = "Observations",
    "obs_statistical" = "",
    "pred_arbocarto" = "Arbocarto predictions",
    "pred_metelmann" = "Metelmann predictions",
    #"pred_stat_explanatory" = "ML explanatory",
    "pred_stat_nowcasting" = "ML predictions",
    "pred_stat_forecasting" = "ML forecasting"))+
  scale_size_identity() +
  ylab("Egg abundance") +
  labs(color = 'Model') +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.x=element_line(colour = "grey"),
    axis.ticks.y=element_line(colour = "grey")
    #text = element_text(family = "Georgia")
  )


### calculer courbes AUC #####
# code to compute the ROC AUC of Metelmann predicitons

library(pROC)

df %>%
  mutate(obs = ifelse(obs==0, 0, 1)) %>%
  pivot_longer(-c('site', 'Year', 'week',"obs")) %>%
  group_by(name,site) %>%
  summarise(auc = as.numeric(auc(obs, value)))




## obs vs pred values
df %>%
  mutate(site = fct_relevel(site,c( "PEROLS", "MURVIEL-LES-MONTPELLIER", "BAYONNE", "SAINT-MEDARD-EN-JALLES" ))) %>%
  group_by(site) %>%
  summarise(spearman_metelmann = round(cor(obs, pred_metelmann, method="spearman", use = "complete.obs"),2),
            spearman_arbocarto = round(cor(obs, pred_arbocarto, method="spearman", use = "complete.obs"),2),
            spearman_ML_nowcasting = round(cor(obs, pred_stat_nowcasting, method="spearman", use = "complete.obs"),2),
            spearman_ML_forecasting = round(cor(obs, pred_stat_forecasting, method="spearman", use = "complete.obs"),2),
            pearson_metelmann = round(cor(obs, pred_metelmann, method="pearson", use = "complete.obs"),2),
            pearson_arbocarto = round(cor(obs, pred_arbocarto, method="pearson", use = "complete.obs"),2),
            pearson_ML_nowcasting = round(cor(obs, pred_stat_nowcasting, method="pearson", use = "complete.obs"),2),
            pearson_ML_forecasting = round(cor(obs, pred_stat_forecasting, method="pearson", use = "complete.obs"),2)
            )




# ## delays between start of the seasons and modeled values
# begin_season_obs <- df_cv_andrea %>%
#   left_join(df_cv_paul) %>%
#   arrange(site,Year,week) %>%
#   filter(obs > 0) %>%  # Keep only rows where obs is not zero
#   group_by(site, Year) %>%  # Group by site and year
#   slice_min(week) %>%
#   ungroup() %>%
#   dplyr::select(site, Year,  week)
#
# begin_season_metelmann <- df_cv_andrea %>%
#   left_join(df_cv_paul) %>%
#   arrange(site,Year,week) %>%
#   filter(pred_metelmann > 0) %>%  # Keep only rows where obs is not zero
#   group_by(site, Year) %>%  # Group by site and year
#   slice_min(week) %>%
#   ungroup() %>%
#   dplyr::select(site, Year,  week)


### Variable importance plot

library(vip)

sites <- c( "PEROLS", "MURVIEL-LES-MONTPELLIER", "BAYONNE", "SAINT-MEDARD-EN-JALLES" )

pfun_presence <- function(object, newdata) {
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
}

pfun_abundance <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
}

# Mean absolute error
spear <- function(truth, estimate) {
  cor(truth, estimate, method = "spearman")
}


vis_presence <- data.frame()
vis_abundance <- data.frame()

for(i in 1:length(sites)){

  th_vis_presence <- vi(model_presence_nowcasting, method = "permute", train = df_mod_presence_nowcasting %>% dplyr::filter(site==sites[i]), target = "PRES_ALBO", metric = "roc_auc",pred_wrapper = pfun_presence, nsim = 30, event_level = 'first', feature_names = model_presence_nowcasting$finalModel$xNames)
  th_vis_abundance <- vi(model_abundance_nowcasting, method = "permute", train = df_mod_abundance_nowcasting %>% dplyr::filter(site==sites[i]), target = "NB_ALBO_TOT", metric = spear, smaller_is_better = FALSE, pred_wrapper = pfun_abundance, nsim = 30, feature_names = model_abundance_nowcasting$finalModel$xNames)

  th_vis_presence$site = sites[i]
  th_vis_abundance$site = sites[i]

  vis_presence <- rbind(vis_presence,th_vis_presence)
  vis_abundance <- rbind(vis_abundance,th_vis_abundance)

}

vis_presence <- vis_presence %>%
  mutate(Variable = as.factor(Variable)) %>%
  mutate(model = "Presence") %>%
  mutate(Variable = fct_relevel(Variable, c("TM_0_8","UM_5_11"))) %>%
  mutate(site = fct_relevel(site,sites))

vis_abundance <- vis_abundance %>%
  mutate(Variable = as.factor(Variable)) %>%
  mutate(model = "Abundance") %>%
  mutate(Variable = fct_relevel(Variable, c("TM_0_4","UM_0_11","RR_1_5"))) %>%
  mutate(site = fct_relevel(site,sites))

vip_presence <- ggplot(vis_presence, aes(x = Variable, y = Importance, fill = site)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  ggtitle("Presence model") +
  ylab("Variable importance\n(loss in ROC AUC)") +
  theme( axis.title.x=element_blank(),
        axis.text.y=element_text(size=8)#,
        #axis.title.y = element_text(size=10)
        ) +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_fill_manual(values = c("#8d5a99","#ff9e17","#7d8be3","#e95ab7")) +
  guides(fill="none")


vip_abundance <- ggplot(vis_abundance, aes(x = Variable, y = Importance, fill = site)) +
  geom_bar(position="dodge", stat="identity") +
  theme_bw() +
  ggtitle("Abundance model") +
  ylab("Variable importance\n(loss in Spearman corr.)") +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(size=8)#,
        #axis.title.y = element_text(size=10)
        ) +
  scale_x_discrete(guide = guide_axis(angle = 0)) +
  scale_fill_manual(values = c("#8d5a99","#ff9e17","#7d8be3","#e95ab7")) +
  guides(fill="none")



### partial dependence plot

library(pdp)

pred_wrapper_classif <- function(object, newdata) {
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
  c("avg" = mean(p))
}

pdps_presence <- list()

variables_presence <- as.character(unique(vis_presence$Variable))

for(i in 1:length(variables_presence)){


  pd1 <- pdp::partial(model_presence_nowcasting, pred.var = variables_presence[i], pred.fun = pred_wrapper_classif, train = df_mod_presence_nowcasting)
  pd1$site = "average"

  sites <- c( "PEROLS", "MURVIEL-LES-MONTPELLIER", "BAYONNE", "SAINT-MEDARD-EN-JALLES" )

  pd <- data.frame()

  for(j in 1:length(sites)){
    th_pd <- pdp::partial(model_presence_nowcasting, pred.var = variables_presence[i], pred.fun = pred_wrapper_classif, train = df_mod_presence_nowcasting %>% filter(site==sites[j])) ## array that returns predictions of a variable in a model
    th_pd$site <- sites[j]
    pd <- rbind(pd,th_pd)
  }

  #pd <- rbind(pd1,pd)

  pd$yhat[which(pd$yhat<0)] <-0
  pd$site <- fct_relevel(pd$site, sites)

  pdps_presence[[i]] <- ggplot() +
    geom_smooth(data=pd, aes_string(x=variables_presence[i], y="yhat", group = "site", color = "site"), se = F, method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 0.5) +
    geom_rug(data=df_mod_presence_nowcasting, aes_string(x = variables_presence[i]), sides="b") +
    theme_bw() +
    ylim(c(0,1)) +
    ylab("Presence probability") +
    xlab(paste(variables_presence[i],
               case_when(grepl("TM|TX|TN",variables_presence[i]) ~ "(°C)",
                         grepl("UM",variables_presence[i]) ~ "(%)",
                         grepl("RR",variables_presence[i]) ~ "(mm)"))) +
    theme(legend.position = "none",
          axis.text.y=element_text(size=8),
          axis.title.y = element_text(size=10),
          axis.title.x = element_text(size=10)
          ) +
    scale_color_manual(values = c("#8d5a99","#ff9e17","#7d8be3","#e95ab7"))


  # if(i > 1){
  #   pdps_presence[[i]] <- pdps_presence[[i]] +  theme(axis.title.y = element_blank())
  # }

}

#plot_pdps_presence <- patchwork::wrap_plots(pdps_presence[[1]],pdps_presence[[2]],pdps_presence[[4]],pdps_presence[[3]], ncol = 4) + plot_annotation(title = "Presence model : PDP") + plot_layout(guides = "collect")
plot_pdps_presence <- patchwork::wrap_plots(pdps_presence) + plot_annotation(title = "Presence model : PDP") + plot_layout(guides = "collect")



# Abundance

pred_wrapper_reg <- function(object, newdata) {
  p <- predict(object, newdata = newdata)
  c("avg" = mean(p))
}


pdps_abundance <- list()

variables_abundance <- as.character(unique(vis_abundance$Variable))

for(i in 1:length(variables_abundance)){

  pd1 <- pdp::partial(model_abundance_nowcasting, pred.var = variables_abundance[i], pred.fun = pred_wrapper_reg, train = df_mod_abundance_nowcasting)
  pd1$site = "average"

  sites <- c( "PEROLS", "MURVIEL-LES-MONTPELLIER",  "BAYONNE" , "SAINT-MEDARD-EN-JALLES")

  pd <- data.frame()

  for(j in 1:length(sites)){
    th_pd <- pdp::partial(model_abundance_nowcasting, pred.var = variables_abundance[i], pred.fun = pred_wrapper_reg, train = df_mod_abundance_nowcasting %>% filter(site==sites[j])) ## array that returns predictions of a variable in a model
    th_pd$site <- sites[j]
    pd <- rbind(pd,th_pd)
  }

  #pd <- rbind(pd1,pd)

  pd$yhat[which(pd$yhat<0)] <-0
  pd$yhat <- exp(pd$yhat)
  pd$site <- fct_relevel(pd$site, sites)

  pdps_abundance[[i]] <- ggplot() +
    geom_smooth(data=pd, aes_string(x=variables_abundance[i], y="yhat", group = "site", color = "site"), se = F, method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 0.5) +
    geom_rug(data=df_mod_abundance_nowcasting, aes_string(x = variables_abundance[i]), sides="b") +
    theme_bw() +
    ylim(c(0,40)) +
    ylab("Abundance") +
    xlab(paste(variables_abundance[i],
               case_when(grepl("TM|TX|TN",variables_abundance[i]) ~ "(°C)",
                         grepl("UM",variables_abundance[i]) ~ "(%)",
                         grepl("RR",variables_abundance[i]) ~ "(mm)"))) +
    theme(legend.position = "bottom",
          axis.text.y=element_text(size=8),
          axis.title.y = element_text(size=10),
          axis.title.x = element_text(size=10)
          ) +
    scale_color_manual(values = c("#8d5a99","#ff9e17","#7d8be3","#e95ab7"))


}

plot_pdps_abundance <- patchwork::wrap_plots(pdps_abundance) + plot_annotation(title = "Abundance model : PDP") + plot_layout(guides = "collect") & theme(legend.position='bottom')


#(vip_presence + plot_pdps_presence  + plot_layout(widths = c(1, 5))) / (vip_abundance + plot_pdps_abundance  + plot_layout(widths = c(1, 5)))

vip_presence + plot_pdps_presence  + plot_layout(widths = c(1, 3))


# abundance : interaction rain and temperature

th_pd <- pdp::partial(model_abundance_nowcasting, pred.var = c("TM_0_4","RR_1_5"), pred.fun = pred_wrapper_reg, train = df_mod_abundance_nowcasting) ## array that returns predictions of a variable in a model
th_pd$yhat = exp(th_pd$yhat)
#p=pdp::plotPartial(th_pd, rug = T, train = df_mod_abundance_nowcasting)

pdp_interaction_abundance <- ggplot() +
  metR::geom_contour_fill(data = th_pd, aes(x = TM_0_4, y = RR_1_5, z = yhat), breaks = seq(0,46,2)) +
  geom_point(data = df_mod_abundance_nowcasting, aes(x=TM_0_4, y=RR_1_5, size = exp(NB_ALBO_TOT)), shape = 1, colour = "white") +
  scale_fill_viridis_b(breaks = seq(0, 46, 2), labels = NULL)+#, labels = c("Lowest",rep("",20),"Highest")) +
  #labs(x = "Average temperature\nover the month preceding collection (°C)", y = "Cumulative rainfall\nover the month preceding collection (mm)", fill = "Predicted\nabundance", size = "Observed\nabundance") +
  labs(x = "TM_0_4 (°C)", y = "RR_1_5 (mm)", fill = "Predicted\nabundance", size = "Observed\nabundance") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_size_continuous(range = c(0.5, 4)) +
  guides(size = "none") +
  theme(#legend.position = "bottom",
        legend.title=element_text(size=10),
        axis.text.y=element_text(size=8),
        axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10))


######
# abundance : TMAX
######

multiv_model_abundance_nowcasting_tmax <- readRDS("res_multiv_model_abundance_nowcasting_tmax.rds")

model_abundance_nowcasting_tmax <- multiv_model_abundance_nowcasting_tmax[[1]] #### sum up of abundance model
df_cv_abundance_nowcasting_tmax <- multiv_model_abundance_nowcasting_tmax[[2]] #### data frame with prediction
df_mod_abundance_nowcasting_tmax <- multiv_model_abundance_nowcasting_tmax[[3]] #### data frame which was used to build the model

pd <- data.frame()
for(j in 1:length(sites)){
  th_pd <- pdp::partial(model_abundance_nowcasting_tmax, pred.var = "TX_0_4", pred.fun = pred_wrapper_reg, train = df_mod_abundance_nowcasting_tmax %>% filter(site==sites[j])) ## array that returns predictions of a variable in a model
  th_pd$site <- sites[j]
  pd <- rbind(pd,th_pd)
}
pd$yhat[which(pd$yhat<0)] <-0
pd$yhat <- exp(pd$yhat)
pd$site <- fct_relevel(pd$site, sites)

pdp_tmax <- ggplot() +
   geom_smooth(data=pd, aes_string(x="TX_0_4", y="yhat", group = "site", color = "site"), se = F, method = "gam", formula = y ~ s(x, bs = "cs"), linewidth = 0.5) +
   geom_rug(data=df_mod_abundance_nowcasting_tmax, aes_string(x = "TX_0_4"), sides="b") +
  theme_bw() +
  ylim(c(0,40)) +
  xlab("TX_0_4 (°C)") +
  theme(axis.text.y=element_text(size=7),
        axis.title.y = element_blank(),
        legend.position = "bottom",
        axis.title.x = element_text(size=10)) +
  scale_color_manual(values = c("#8d5a99","#ff9e17","#7d8be3","#e95ab7"))



row1 <- vip_presence + pdps_presence[[1]] + pdps_presence[[2]] +   plot_spacer() + plot_layout(ncol = 4, widths = c(1, 1, 1, 1.4), axis_titles = "collect")
row2 <- vip_abundance + pdps_abundance[[1]] + pdps_abundance[[2]] + pdps_abundance[[3]] +  plot_layout(ncol = 4, axis_titles = "collect")

(vip_presence + pdps_presence[[1]] + pdps_presence[[2]] +   plot_spacer()) +  vip_abundance + pdps_abundance[[1]] + pdps_abundance[[2]] + pdps_abundance[[3]] + plot_layout(nrow=2,guides = 'collect', axis_titles = "collect") &  theme(legend.position = 'bottom')


# Row 1
row1 <- vip_presence + pdps_presence[[1]] + pdps_presence[[2]] +
  plot_spacer() + plot_spacer() +
  plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 2.1), axis_titles = "collect")

# Row 2: make last plot wider
row2 <- vip_abundance + pdps_abundance[[1]] + pdps_abundance[[2]] +
  pdps_abundance[[3]] + pdp_interaction_abundance +
  plot_layout(ncol = 5, widths = c(1, 1, 1, 1, 1.6),guides = 'collect', axis_titles = "collect")  # <– wider last plot!

# Combine rows
row1 / row2 +
  plot_layout(nrow = 2, guides = 'collect', axis_titles = "collect") &
  theme(legend.position = 'bottom')
#
#
# row1 <- vip_presence + pdps_presence[[1]] + pdps_presence[[2]] +
#   plot_layout(ncol = 3, widths = c(1, 1, 1), axis_titles = "collect")
#
# row2 <- vip_abundance + pdps_abundance[[1]] + pdps_abundance[[2]] +
#   plot_layout(ncol = 3, widths = c(1, 1, 1), axis_titles = "collect")
#
# row3 <- pdps_abundance[[3]] + pdp_interaction_abundance +
#   plot_layout(ncol = 3, widths = c(1, 1.5, 0.5),guides = 'collect', axis_titles = "collect")
#
#
# row1 / row2 / row3 +
#   plot_layout(nrow = 3, guides = 'collect', axis_titles = "collect") &
#   theme(legend.position = 'bottom')

########################s
## Local interpretation
########################

library(lime)

meteo <- read.csv(file.path("data","processed","df_meteo_predictions.csv")) %>%
  mutate(date=as.Date(date)) %>%
  mutate(week = week(date), Year = year(date)) %>%
  dplyr::filter(date>=as.Date("2023-01-01" & date < "2025-01-01"))



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
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         legend.text=element_text(size=9),
         text = element_text(size=10),
         plot.margin = margin(5, 5, 5, 5),
         strip.text = element_text(size = rel(1.2))
   ) +
    scale_x_date(limits = c(as.Date("2023-01-01"),as.Date("2024-12-31")),
                       breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "3 month"),
                       minor_breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "1 month"),
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
    scale_x_date(limits = c(as.Date("2023-01-01"),as.Date("2024-12-31")),
                 breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "3 month"),
                 minor_breaks = seq(as.Date("2023-01-01"), as.Date("2025-01-01"), by = "1 month"),
                 date_labels = "%Y-%m")


  return(p1)
}


df_cv_paul <- df_cv_paul %>%
  mutate(date = as.Date(paste(Year, week, 1, sep="-"), "%Y-%U-%u"))  %>%
  add_row(site = "BAYONNE", Year = 2024, week = 1, pred_stat_nowcasting = 2000, pred_stat_forecasting = NA) %>%
  add_row(site = "SAINT-MEDARD-EN-JALLES", Year = 2024, week = 1, pred_stat_nowcasting = 2000, pred_stat_forecasting = NA)

# df_cv_paul <- read.csv("pred_llo.csv") %>%
#   mutate(date = as.Date(date)) %>%
#   dplyr::filter(date>as.Date("2023-01-01")) %>%
#   rename(pred_stat_nowcasting = pred_abundance)


scaleFactor = 0.6

dd_perols <- p_perols +
  geom_point(data=df_cv_paul %>% filter(site=="PEROLS"), aes(y = pred_stat_nowcasting * scaleFactor,  color = "Predictions (ML)"), size = 0.7) +
  geom_line(data=df_cv_paul %>% filter(site=="PEROLS"), aes(y = pred_stat_nowcasting * scaleFactor, color = "Predictions (ML)"), size = 0.5) +
  scale_color_manual(labels = c("Observations","Predictions (ML)","Temperatures"), values = c("Temperature" = "orange", "Eggs/trap" = "#49423c", "Predictions (ML)" = "#457b9d"))

dd_murviels <- p_murviels +
  geom_point(data=df_cv_paul %>% filter(site=="MURVIEL-LES-MONTPELLIER"), aes(y = pred_stat_nowcasting * scaleFactor,  color = "Predictions (ML)"), size = 0.7) +
  geom_line(data=df_cv_paul %>% filter(site=="MURVIEL-LES-MONTPELLIER"), aes(y = pred_stat_nowcasting * scaleFactor, color = "Predictions (ML)"), size = 0.5) +
  scale_color_manual(labels = c("Observations","Predictions (ML)","Temperatures"), values = c("Temperature" = "orange", "Eggs/trap" = "#49423c", "Predictions (ML)" = "#457b9d"))

dd_bayonne <- p_bayonne +
  geom_point(data=df_cv_paul %>% filter(site=="BAYONNE"), aes(y = pred_stat_nowcasting * scaleFactor,  color = "Predictions (ML)"), size = 0.7) +
  geom_line(data=df_cv_paul %>% filter(site=="BAYONNE"), aes(y = pred_stat_nowcasting * scaleFactor, color = "Predictions (ML)"), size = 0.5) +
  scale_color_manual(labels = c("Observations","Predictions (ML)","Temperatures"), values = c("Temperature" = "orange", "Eggs/trap" = "#49423c", "Predictions (ML)" = "#457b9d"))

dd_medard <- p_medard +
  geom_point(data=df_cv_paul %>% filter(site=="SAINT-MEDARD-EN-JALLES"), aes(y = pred_stat_nowcasting * scaleFactor,  color = "Predictions (ML)"), size = 0.7) +
  geom_line(data=df_cv_paul %>% filter(site=="SAINT-MEDARD-EN-JALLES"), aes(y = pred_stat_nowcasting * scaleFactor, color = "Predictions (ML)"), size = 0.5) +
  scale_color_manual(labels = c("Observations","Predictions (ML)","Temperatures"), values = c("Temperature" = "orange", "Eggs/trap" = "#49423c", "Predictions (ML)" = "#457b9d"))


(
  (dd_perols + dd_murviels) /
    (plot_lime_v2(perols) + plot_lime_v2(murviel)) /
  (dd_bayonne + dd_medard) /
    (plot_lime_v2(bayonne) + plot_lime_v2(medard))
) +
  plot_layout(guides = "collect", heights = c(2, 1, 2, 1), axis_titles = "collect")



dd_perols/plot_lime_v1(perols)+ plot_layout(heights = c(1, 2))
dd_murviels/plot_lime_v1(murviel)+ plot_layout(heights = c(1, 2))
dd_bayonne/plot_lime_v1(bayonne)+ plot_layout(heights = c(1, 2))
dd_medard/plot_lime_v1(medard)+ plot_layout(heights = c(1, 2))













###########################
#########'Presence model
#########'First step: Evaluation  plots, with site cross validation and with cross site validation and session cross validation
#########'Second step: Validation with AUC using the site cross validation
#########'Third step: realization of Variable Importance Plots (VIP) (from the site cross validation)
#########'Last step: realization of Partial Dependent Plots (PDP) (from the site cross validation)
###########################


#### First step: Model evaluation plots

## With only site cross validation: plot with observation and prediction for the different site, trap and numero session

 df_cv_presence %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
   mutate(val = case_when(pred_final=="Presence" & name == "Predicted" ~ "Presence",
                            pred_final=="Absence" & name == "Predicted" ~ "Absence",
                            name == "Observed" & value == 0 ~ "Absence",
                          name == "Observed" & value == 1 ~ "Presence")) %>%
   mutate(name = fct_relevel(name,c("Predicted","Observed"))) %>%
  ggplot(aes(x=week, y=name, fill = val,width=1, height=1)) +
  geom_tile( size=0.5, colour = "grey50") +
  facet_grid(site~Year) +
   scale_x_continuous(limits = c(1,52)) +
   theme_light()

 df_cv_presence_twomodels %>% mutate(obs = ifelse(obs == 1, "Presence","Absence")) %>%
   as_tibble() %>%
   pivot_longer(c('pred_final','pred_andrea','obs')) %>%
   mutate(name = case_when(name=="pred_final" ~ "Predicted ML",
                           name=="obs" ~ "Observed",
                           name=="pred_andrea" ~ "Predicted Metelmann")) %>%
   mutate(name = fct_relevel(name,c("Predicted ML","Predicted Metelmann","Observed"))) %>%
   ggplot(aes(x=week, y=name, fill = value,width=1, height=1)) +
   geom_tile( size=0.5, colour = "grey50") +
   facet_grid(site~Year) +
   scale_x_continuous(limits = c(1,52)) +
   theme_light()


ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/test/test_Article/presence_evaluation.pdf",plot =plot_eval_presence_model, device = "pdf", width = 11, height = 8)

#### Second step: Model validation plots: ROC

AUC = MLmetrics::AUC(df_cv_presence$pred, df_cv_presence$obs) ## To calculate the AUC

precrec_obj <- precrec::evalmod(scores = df_cv_presence$pred, labels = df_cv_presence$obs)

plot_validation_presence <- autoplot(precrec_obj,curvetype = c("ROC")) +
  ggtitle(paste0("Presence model : ROC curve (AUC = ",round(AUC,2),")")) +
  theme(axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8))

ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/presence_validation.pdf",plot =plot_validation_presence, device = "pdf", width = 11, height = 8) ## to save

#### Third step: VIP

model = multiv_model_presence$model
df = multiv_model_presence$df_mod
df_cv <-  multiv_model_presence$df_cv

## To select the importance of each variable from the model and to transform in data frame
imp <- model$finalModel$variable.importance
imp <- as.data.frame(imp)
imp$var <- rownames(imp)

## To arrange by order of importance and to categorize by type of variable
imp <- imp %>%
  dplyr::rename(importance = imp) %>%
  mutate(label = forcats::fct_reorder(var, importance)) %>%
  arrange(-importance)


## To plot the importance of the variables
plot_imp_presence <- ggplot(imp, aes(x = importance , y = label, label = label)) +
  geom_bar(position = 'dodge', stat="identity", width = 0.6) +
  theme_bw() +
  geom_text(size=3,position = position_dodge(0.9),hjust=-0.1,label.padding = unit(0.2, "lines")) + #,aes(fontface=2)
  #   geom_label(size=2, aes(fontface=2), label.padding = unit(0.15, "lines"), x = 0.05, alpha = 0.5) +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(size = 7),
        plot.subtitle = element_text(size = 7, face="bold")
  ) +
  ylab("") +
  xlab("") +
  xlim(NA,max(imp$importance, na.rm = T) + max(imp$importance, na.rm = T)*2.5) +
  labs(title = "Presence model : VIP")

#ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/presence_VIP.pdf",plot =plot_imp_presence, device = "pdf", width = 11, height = 8) ## to save

#### Last step: PDP

## To create a function which predicts the probability of presence according different variabes
pred_wrapper_classif <- function(object, newdata) {
  p <- predict(object, newdata = newdata, type ="prob")[,"Presence"]
  c("avg" = mean(p))
}

pdps <- list()

imp <- imp %>% filter(var!="site")

df$site=as.factor(df$site)

for(i in 1:length(imp$var)){

  # pd <- pdp::partial(model, pred.var = c(imp$var[i],"site"), pred.fun = pred_wrapper_classif, train = df)
  # ggplot(pd, aes(x=TM_0_8, y=yhat, colour = site))+ geom_line()

  pd <- pdp::partial(model, pred.var = imp$var[i], pred.fun = pred_wrapper_classif, train = df) ## array that returns predictions of a variable in a model
  pd$yhat[which(pd$yhat<0)] <-0
  p <- autoplot(pd, smooth = T)
  dat1 <- ggplot_build(p)$data[[1]]
  dat2 <- ggplot_build(p)$data[[2]]


    dat1$x <- c("Absence","Presence")
    df[,imp$var[i]][which(df[,imp$var[i]]==1)] <- "Presence"
    df[,imp$var[i]][which(df[,imp$var[i]]==0)] <- "Absence"

    pdps[[i]] <- ggplot() +
      geom_bar(data = dat1, aes(x = x, y = y), size = 0.5, fill = "#009E73", stat = "identity") +  ## indicate the different value of the variable present on the data frame
      geom_rug(data = df, aes_string(x = imp$var[i]), sides="b", length = unit(0.05, "npc")) +
      ylim(c(0,1)) +
      theme_bw() +
      xlab(imp$var[i]) +
      ylab("")

    df[,imp$var[i]][which(df[,imp$var[i]]=="Presence")] <- "1"
    df[,imp$var[i]][which(df[,imp$var[i]]=="Absence")] <- "0"
    df[,imp$var[i]] <- as.numeric(df[,imp$var[i]])


}

plot_pdps_presence <- patchwork::wrap_plots(pdps) + plot_annotation(title = "Presence model : PDP") ## put all the plots together

#ggsave(filename = "02_Data/processed_data/plots/modelling_adults_abundance/test/test_Article/presence_PDP.pdf",plot =plot_pdps_presence, device = "pdf", width = 11, height = 8) ## To save








## Abundance


plot_eval_abundance_model <- df_cv_abundance %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  mutate(date = as.Date(paste(Year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot(aes(x=date, y = value, color = name, group = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~site) +
  theme_bw() +
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F)
  xlab("entomological survey") +
  ylab("mean abundance") +
  labs(color='Number of Ae. Albopictus') +
  theme(legend.position="bottom") +
  ggtitle('Abundance models : observed vs. predicted values by site and entomological survey')


  ## Presence + abundance


  df_cv_presence2 <- df_cv_presence %>%
    mutate(pred = ifelse(pred_final == "Presence",1,0)) %>%
    dplyr::select(-pred_final) %>%
    mutate(model = "presence_model")

  df_cv_abundance2 <- df_cv_abundance %>%
    mutate(obs=exp(obs),pred=exp(pred)) %>%
    mutate(model = "abundance_model")

  #  pour un trouple {site,week,Year} :
  # - quand le modèle de présence/absence prédit une absence, on conserve les résultats du modèle de présence
  # - quand le modèle de présence/absence prédit une présence, on conserve les résultats du modèle d'abondance

  df_cv_presence2 %>%
    left_join(df_cv_abundance2, by = c( "site", "week", "Year"), suffix = c("_mod_presence","_mod_abundance")) %>%
    mutate(obs_final = ifelse(obs_mod_presence==0, obs_mod_presence,obs_mod_abundance)) %>%
    mutate(pred_final = ifelse(pred_mod_presence==0, pred_mod_presence,pred_mod_abundance)) %>%
    dplyr::select(pred_final,obs_final,site,week,Year) %>%
    rename(pred = pred_final, obs = obs_final) %>%
    pivot_longer(c('pred','obs')) %>%
    mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
    mutate(date = as.Date(paste(Year, week, 1, sep="-"), "%Y-%U-%u")) %>%
    ggplot(aes(x=date, y = value, color = name, group = name)) +
    geom_point() +
    geom_line() +
    facet_wrap(.~site) +
    theme_bw() +
    scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F) +
    ylab("mean abundance") +
    labs(color='Number of Ae. Albopictus') +
    theme(legend.position="bottom") +
    ggtitle('Models : observed vs. predicted values by site and entomological survey')



  dd=df_cv_presence2 %>%
    left_join(df_cv_abundance2, by = c( "site", "week", "Year"), suffix = c("_mod_presence","_mod_abundance")) %>%
    mutate(obs_final = ifelse(obs_mod_presence==0, obs_mod_presence,obs_mod_abundance)) %>%
    mutate(pred_final = ifelse(pred_mod_presence==0, pred_mod_presence,pred_mod_abundance)) %>%
    dplyr::select(pred_final,obs_final,site,week,Year) %>%
    rename(pred = pred_final, obs = obs_final) %>%
    pivot_longer(c('pred','obs')) %>%
    mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
    mutate(date = as.Date(paste(Year, week, 1, sep="-"), "%Y-%U-%u"))

    ggplot(dd,aes(x=as.factor(week), y = value, group = name, color = name)) +
    geom_line() +
    facet_wrap(site~Year)  +
    theme_bw() +
    scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F)



    df_mod_abundance <- df_mod_abundance %>%
      arrange(site,Year,week)
    df_cv_abundance <- df_cv_abundance %>%
      arrange(site,Year,week)

df_cv_abundance %>%
  mutate(obs=exp(obs),pred=exp(pred)) %>%
  as_tibble() %>%
  pivot_longer(c('pred','obs')) %>%
  mutate(name = ifelse(name=="pred","Predicted","Observed")) %>%
  mutate(date = as.Date(paste(Year, week, 1, sep="-"), "%Y-%U-%u")) %>%
  ggplot(aes(x=date, y = value, color = name, group = name)) +
  geom_point() +
  geom_line() +
  facet_wrap(.~site) +
  theme_bw() +
  scale_colour_manual(values=c("#009E73","#E69F00"),na.translate = F)
xlab("entomological survey") +
  ylab("mean abundance") +
  labs(color='Number of Ae. Albopictus') +
  theme(legend.position="bottom") +
  ggtitle('Abundance models : observed vs. predicted values by site and entomological survey')



