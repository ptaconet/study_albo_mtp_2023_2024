########################### Opening packages

library(tidyverse) ## Version ‘2.0.0’
library(caret) ## Version ‘6.0.94’
library(CAST) ## Version ‘1.0.2’
library(ranger) ## Version ‘0.16.0’
library(correlation) ## Version ‘0.8.5’

########################### Open dataset containing the dependant and independent variables

df_model <- read.csv(file.path("data","processed","df_to_model.csv"))

# grouper à l'échelle de la ville-semaine de collecte :
df_model <- df_model %>%
  relocate(effectif_jour,.before = RR_0_0) %>%
  group_by(site, Year,week) %>%
  summarise_at(vars(effectif_jour:RFNO), mean, na.rm = TRUE) %>%
  ungroup()


df_model <- df_model %>%
  rename(NB_ALBO_TOT = effectif_jour) %>%
  mutate(PRES_ALBO = ifelse(NB_ALBO_TOT>0,"Presence","Absence")) %>% ## to create a "character" variable for presence or absence of Aedes albopictus
  mutate(PRES_ALBO = fct_relevel(PRES_ALBO,c("Presence","Absence"))) %>%
  mutate(PRES_ALBO_NUMERIC = ifelse(PRES_ALBO=="Presence",1,0)) %>% ## to create a numeric variable for presence or absence of Aedes albopictus
  filter(!is.na(NB_ALBO_TOT)) %>%
  dplyr::filter(site!="RENNES")

###########################
#########'Presence model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################


##### First step: to select variables for presence models
predictors_presence <- c("TM_3_8","TN_3_8","TX_3_8","UM_5_11","RR_7_8","FFM_3_8")

#### Final data frame for the multivariate analysis
df_model_presence <- df_model %>%
  dplyr::select("site","Year", "week" ,  "NB_ALBO_TOT", "PRES_ALBO", predictors_presence)



###########################
#########'Abundance model preparation
#########'First step: to select for meteorological and pollutants variables, for every type of variable, the time lag for which the r2 was the highest. Same work is realized for micro climatic, land cover (for each buffer) and socio demographic data.
#########'Second step: to evaluate the correlation between these variables.
#########'Third step: to select the variables not correlated with the highest sense ecological. The first selection is crossed with the other selection done with the VIF with the corSelect function of the fuzzySim package to select variables with the lowest VIF. The final selection is a mixed of both methods.
###########################

##### First step: select variables for abundance models
predictors_abundance <- c("TM_3_8","TN_3_8","TX_3_8","UM_3_8","RR_3_8","FFM_3_8")

#### Final data frame for the multivariate analysis
df_model_abundance <- df_model %>%
  filter(NB_ALBO_TOT>0) %>%
  dplyr::select("site","Year", "week" ,  "NB_ALBO_TOT", "PRES_ALBO", predictors_abundance)

df_model_abundance$NB_ALBO_TOT <- log(df_model_abundance$NB_ALBO_TOT)


## leave location out

#### First step: to parameter the model: leave-one-site-out cross validation
cv_col <- "site"

#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)

indices_cv <- CAST::CreateSpacetimeFolds(df_model_presence, spacevar = cv_col, k = length(unique(unlist(df_model_presence[,cv_col])))) #### Take into acocunt spatil avariability

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv", ## Definition of method sampling: cross validation
                  index = indices_cv$index,  ##  list of elements to sampling
                  indexOut = indices_cv$indexOut,##  list of items to be set aside for each resampling
                  summaryFunction = twoClassSummary,#comboSummary, ## Calcul of ROC and AUC
                  classProbs = TRUE,
                  savePredictions = 'final',
                  verboseIter = FALSE
)


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the ROC
mod_presence <- CAST::ffs(predictors = df_model_presence[,predictors_presence], response = df_model_presence$PRES_ALBO, method = "ranger", tuneLength = 10, trControl = tr, metric = "ROC", maximize = TRUE,  preProcess = c("center","scale"))


#### Last step: to put predictions on same data frame
df_model_presence$rowIndex <- seq(1,nrow(df_model_presence),1)
df_cv_presence <- mod_presence$pred %>%
  left_join(df_model_presence) %>%
  dplyr::select(pred,Presence,obs,site,week,Year) %>%
  mutate(obs = ifelse(obs == "Absence",0,1)) %>%
  dplyr::rename(pred_final = pred, pred = Presence)

res_multiv_model_presence <- list(model = mod_presence, df_cv = df_cv_presence, df_mod = df_model_presence) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_presence,"res_multiv_model_presence_forecasting_llo.rds")

##############
#####" abundance
#############


#### Second step: It will train the model on data from all traps except one location, recursively on all locations. At the end: a table with predicted data for all traps (predicted with data)
indices_cv <- CAST::CreateSpacetimeFolds(df_model_abundance, spacevar = cv_col,k = length(unique(unlist(df_model_abundance[,cv_col]))))

spearmcor <- function(data,lev = NULL,model = NULL) {
  out <- cor(x = data$pred, y = data$obs)
  names(out) <- "spearman"
  out
}

## Optimising the various model parameters: finding them as a function of predictive power, in relation to a predictive value (ROC, MAE, etc)
tr = trainControl(method="cv",
                  index = indices_cv$index,
                  indexOut = indices_cv$indexOut,
                  savePredictions = 'final',
                  summaryFunction = spearmcor)


#### Third step: realisation of the model of random forest, with the method of permutation to evaluate variable importance and calculating the MAE
mod_abundance <- CAST::ffs(predictors = df_model_abundance[,predictors_abundance], response = df_model_abundance$NB_ALBO_TOT, method = "ranger", tuneLength = 10, trControl = tr, metric = "spearman", maximize = TRUE,  preProcess = c("center","scale"))

#### Last step: to put predictions on same data frame
df_model_abundance$rowIndex <- seq(1,nrow(df_model_abundance),1)
df_cv_abundance <- mod_abundance$pred %>%
  left_join(df_model_abundance) %>%
  dplyr::select(pred,obs,site,week,Year)

res_multiv_model_abundance <- list(model = mod_abundance, df_cv = df_cv_abundance, df_mod = df_model_abundance) ## to save models, data frame of the model and predictions
saveRDS(res_multiv_model_abundance,"res_multiv_model_abundance_forecasting_llo.rds")



