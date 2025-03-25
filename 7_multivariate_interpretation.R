########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(iml) ## Version '0.11.3'
library(patchwork) ## Version ‘1.2.0.9000’
library(precrec) ## Version ‘0.14.4’

########################### Open dataset containing the results of presence and abundance models

multiv_model_presence_explanatory <- readRDS("res_multiv_model_presence.rds")
multiv_model_abundance_explanatory <- readRDS("res_multiv_model_abundance.rds")

multiv_model_presence_forecasting <- readRDS("res_multiv_model_presence_forecasting_llo.rds")
multiv_model_abundance_forecasting <- readRDS("res_multiv_model_abundance_forecasting_llo.rds")

model_presence_explanatory <- multiv_model_presence_explanatory[[1]] #### sum up of presence model
df_cv_presence_explanatory <- multiv_model_presence_explanatory[[2]]#### data frame with prediction
df_mod_presence_explanatory <- multiv_model_presence_explanatory[[3]] #### data frame which was used to build the model

model_abundance_explanatory <- multiv_model_abundance_explanatory[[1]] #### sum up of abundance model
df_cv_abundance_explanatory <- multiv_model_abundance_explanatory[[2]] #### data frame with prediction
df_mod_abundance_explanatory <- multiv_model_abundance_explanatory[[3]] #### data frame which was used to build the model


model_presence_forecasting <- multiv_model_presence_forecasting[[1]] #### sum up of presence model
df_cv_presence_forecasting <- multiv_model_presence_forecasting[[2]]#### data frame with prediction
df_mod_presence_forecasting <- multiv_model_presence_forecasting[[3]] #### data frame which was used to build the model

model_abundance_forecasting <- multiv_model_abundance_forecasting[[1]] #### sum up of abundance model
df_cv_abundance_forecasting <- multiv_model_abundance_forecasting[[2]] #### data frame with prediction
df_mod_abundance_forecasting <- multiv_model_abundance_forecasting[[3]] #### data frame which was used to build the model



df_cv_presence_explanatory2 <- df_cv_presence_explanatory %>%
  mutate(pred_stat_presence_explanatory = ifelse(pred_final == "Presence",1,0)) %>%
  dplyr::select(site,week,Year,pred_stat_presence_explanatory)

df_cv_presence_forecasting2 <- df_cv_presence_forecasting %>%
  mutate(pred_stat_presence_forecasting = ifelse(pred_final == "Presence",1,0)) %>%
  dplyr::select(site,week,Year,pred_stat_presence_forecasting)


df_cv_abundance_explanatory2 <- df_cv_abundance_explanatory %>%
  mutate(pred_stat_abundance_explanatory = exp(pred)) %>%
  dplyr::select(site,week,Year,pred_stat_abundance_explanatory)

df_cv_abundance_forecasting2 <- df_cv_abundance_forecasting %>%
  mutate(pred_stat_abundance_forecasting = exp(pred)) %>%
  dplyr::select(site,week,Year,pred_stat_abundance_forecasting)

df_cv_paul <- df_cv_presence_explanatory2 %>%
  left_join(df_cv_presence_forecasting2) %>%
  left_join(df_cv_abundance_explanatory2) %>%
  left_join(df_cv_abundance_forecasting2) %>%
  mutate(pred_final_explanatory = ifelse(pred_stat_presence_explanatory==0, pred_stat_presence_explanatory,pred_stat_abundance_explanatory)) %>%
  mutate(pred_final_forecasting = ifelse(pred_stat_presence_forecasting==0, pred_stat_presence_forecasting,pred_stat_abundance_forecasting)) %>%
  dplyr::select(site,week,Year,pred_final_explanatory,pred_final_forecasting) %>%
  rename(pred_stat_explanatory = pred_final_explanatory, pred_stat_forecasting = pred_final_forecasting)

# data andrea
load("dataframe_Metelmann_Arbocarto_Andrea.RData") # abundance_tot_df

df_cv_andrea <- abundance_tot_df %>%
  rename(Year = year) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  filter(!(site %in% c("MONTPELLIER","TOULOUSE")), Year>2022)

df <- df_cv_andrea %>%
  left_join(df_cv_paul)

df %>%
  rename(obs_mathematical = obs) %>%
  mutate(obs_statistical = obs_mathematical) %>%
  mutate_at(c("obs_mathematical","pred_metelmann", "pred_arbocarto"), funs(c(scales::rescale(., to = c(0, 1))))) %>%
  pivot_longer(-c('site', 'Year', 'week')) %>%
  mutate(date = as.Date(paste(Year, week, 1, sep = "-"), "%Y-%U-%u")) %>%
  mutate(model = case_when(
    name %in% c("obs_mathematical","pred_metelmann", "pred_arbocarto") ~ 'Mathematical models',
    name %in% c("obs_statistical","pred_stat_explanatory", "pred_stat_forecasting") ~ 'Statistical models'  )) %>%
  # Duplicate "obs" so it appears in both "mathematical" and "statistical" facets
  mutate(site = fct_relevel(site, c( "PEROLS", "MURVIEL-LES-MONTPELLIER", "SAINT-MEDARD-EN-JALLES" , "BAYONNE", "RENNES" ))) %>%
  ggplot(aes(x = date, y = value, color = name, group = name, size = ifelse(name %in% c("obs_mathematical","obs_statistical"), 0.5, 0.3))) +
  geom_line() +
  facet_grid(cols = vars(site), rows = vars(model), scales = "free_y") +
 scale_color_manual(values = c(
    "obs_mathematical" = "#49423c",  # Observed values in black
    "obs_statistical" = "#49423c",  # Observed values in black
    "pred_arbocarto" = "#56B4E9",  # Similar blue for Arbocarto
    "pred_metelmann" = "#4292c6",  # Slightly different but close blue for Metelman
    "pred_stat_explanatory" = "#E69F00",
    "pred_stat_forecasting" = "#D55E00"
  ),
  labels = c(
    "obs_mathematical" = "Observations",
    "obs_statistical" = "",
    "pred_arbocarto" = "Arbocarto",
    "pred_metelmann" = "Metelmann",
    "pred_stat_explanatory" = "ML explanatory",
    "pred_stat_forecasting" = "ML forecasting"))+
  scale_size_identity() +
  ylab("Egg abundance") +
  labs(color = 'Model') +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
    axis.title.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.x=element_line(colour = "grey"),
    axis.ticks.y=element_line(colour = "grey")
  )


## obs vs pred values
df %>%
  group_by(site) %>%
  summarise(spearman_metelmann = round(cor(obs, pred_metelmann, method="spearman", use = "complete.obs"),2),
            spearman_arbocarto = round(cor(obs, pred_arbocarto, method="spearman", use = "complete.obs"),2),
            spearman_ML_explanatory = round(cor(obs, pred_stat_explanatory, method="spearman", use = "complete.obs"),2),
            spearman_ML_forecasting = round(cor(obs, pred_stat_forecasting, method="spearman", use = "complete.obs"),2),
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



