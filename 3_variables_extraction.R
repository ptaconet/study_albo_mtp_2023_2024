library(tidyverse)

meteo <- read.csv("data_meteofrance/data_meteofrance_2023_2024.csv") %>%
  mutate(date = as.Date(date)) %>%
  mutate(TMIN_GDD = ifelse(TMIN>11, TMIN, 11),TMAX_GDD = case_when(TMAX<11~11, TMAX==11~11, TMAX==30~30, 11<TMAX & TMAX<30~TMAX, TMAX>30~30), GDDjour=(TMAX_GDD+TMIN_GDD)/2-11 ) %>%
  dplyr::select(-TMIN_GDD) %>%
  dplyr::select(-TMAX_GDD)


pieges_data <- read.csv("piege_data.csv") %>%
  filter(!is.na(date_releve_jour)) %>%
  mutate(date_releve_jour = parse_date_time(date_releve_jour,"d/m/y"), date_instal_jour = parse_date_time(date_instal_jour,"d/m/y")) %>%
  mutate(num_releve = seq(1,nrow(.),1)) %>%
  mutate(idpointdecapture = paste0(num_piege,"_",num_releve))


lag_max <- 42



meteo$GDDacc<-NA
meteo$GDDbound<-NA
i<-1
## calcul GDD accumulé et limité à 1350 depuis 1e janvuer
while (i<=nrow(meteo)){
  date<-meteo$date[i]
  if (date<"2023-01-01"){
    meteo$GDDacc[i]<-NA
    meteo$GDDbound[i]<-NA
  }
  else{
    gdd<-subset(meteo, meteo$date>="2023-01-01"& meteo$date<=meteo$date[i])
    gdd_acc<-sum(gdd$GDDjour)
    meteo$GDDacc[i]<-gdd_acc
    if (gdd_acc<=1350){
      meteo$GDDbound[i]<-gdd_acc}
    else{
      meteo$GDDbound[i]<-meteo$GDDbound[i-1]-meteo$GDDjour[i]
    }
  }
  i<-i+1
}



#### Functions

fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize){

  if(grepl("RFD|GDDjour",var_to_summarize)){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      group_by(idpointdecapture, lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      dplyr::summarise(val=sum(val, na.rm = T),date = min(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(n()-1,0,-1)) %>%
      mutate(var = var_to_summarize) %>%
      as_tibble() %>%
      dplyr::select(-year)
  } else {
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      summarise(val=mean(val, na.rm = T),date = min(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(n()-1,0,-1)) %>%
      mutate(var = var_to_summarize) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }
  return(df_meteo_pieges2_summarize)

}


# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){

  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))

  max_col <- ncol(df_timeseries_wide)

  for(i in 2:(max_col-1)){
    for(j in (i+1):max_col){
      column_name <- paste0(colnames(df_timeseries_wide[i]),"_",(j-2))
      if(function_to_apply=="mean"){
        df_timeseries_wide[column_name] <- rowMeans(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="sum"){
        df_timeseries_wide[column_name] <- rowSums(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }

  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)

}


meteo <- meteo %>%
  filter(!is.na(RFD), !is.na(TMIN), !is.na(TMAX), !is.na(TMN), !is.na(TAMP), !is.na(TSD), !is.na(RHMIN), !is.na(RHMAX), !is.na(RHMN), !is.na(RHAMP), !is.na(RHSD), !is.na(WINDMIN), !is.na(WINDMAX), !is.na(WINDMN) ) %>%
  mutate(date = as.character(date))


pieges_data2 <- pieges_data %>%
  dplyr::select(idpointdecapture, num_releve, num_piege, date_releve_jour, nom_commune) %>%
  mutate(date_releve_jour=as.Date(date_releve_jour))


df_meteo_pieges <- data.frame(idpointdecapture = character(), num_releve = numeric(),num_piege = character(), nom_commune = character(), date = character(), stringsAsFactors = F)
for(i in 1:nrow(pieges_data2)){
  cat(i/nrow(pieges_data2)*100,"%\n")
  for(j in 0:lag_max){
    df_meteo_pieges <- rbind(df_meteo_pieges,
                             data.frame(idpointdecapture = pieges_data2$idpointdecapture[i],
                                        num_releve = pieges_data2$num_releve[i],
                                        num_piege = pieges_data2$num_piege[i],
                                        nom_commune = pieges_data2$nom_commune[i],
                                        date = as.character(as.Date(pieges_data2$date_releve_jour[i]-j)),
                                        lag_n = j,
                                        stringsAsFactors = F))
  }
}

# summarizing to weeks
df_meteo_pieges2 <- df_meteo_pieges %>%
  left_join(meteo, by = c("date","nom_commune")) %>%
  pivot_longer(!(idpointdecapture:numer_sta), names_to = "var", values_to = 'val')



df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RFD") %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMIN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMAX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TMN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TAMP")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TSD")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RHMIN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RHMAX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RHMN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RHAMP")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RHSD")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"WINDMIN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"WINDMAX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"WINDMN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDjour")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDacc")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDbound"))



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RFD","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TMIN","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"TMAX","mean")
df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TMN","mean")
df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"TAMP","mean")
df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"TSD","mean")
df_meteo_pieges_summ_wide7 <- fun_ccm_df(df_meteo_pieges_summ,"RHMIN","mean")
df_meteo_pieges_summ_wide8 <- fun_ccm_df(df_meteo_pieges_summ,"RHMAX","mean")
df_meteo_pieges_summ_wide9 <- fun_ccm_df(df_meteo_pieges_summ,"RHMN","mean")
df_meteo_pieges_summ_wide10 <- fun_ccm_df(df_meteo_pieges_summ,"RHAMP","mean")
df_meteo_pieges_summ_wide11 <- fun_ccm_df(df_meteo_pieges_summ,"RHSD","mean")
df_meteo_pieges_summ_wide12 <- fun_ccm_df(df_meteo_pieges_summ,"WINDMIN","mean")
df_meteo_pieges_summ_wide13 <- fun_ccm_df(df_meteo_pieges_summ,"WINDMAX","mean")
df_meteo_pieges_summ_wide14 <- fun_ccm_df(df_meteo_pieges_summ,"WINDMN","mean")
df_meteo_pieges_summ_wide15 <- fun_ccm_df(df_meteo_pieges_summ,"GDDjour","sum")
df_meteo_pieges_summ_wide16 <- fun_ccm_df(df_meteo_pieges_summ,"GDDacc","mean")
df_meteo_pieges_summ_wide17 <- fun_ccm_df(df_meteo_pieges_summ,"GDDbound","mean")

df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3) %>%
  left_join(df_meteo_pieges_summ_wide4) %>%
  left_join(df_meteo_pieges_summ_wide5) %>%
  left_join(df_meteo_pieges_summ_wide6) %>%
  left_join(df_meteo_pieges_summ_wide7) %>%
  left_join(df_meteo_pieges_summ_wide8) %>%
  left_join(df_meteo_pieges_summ_wide9) %>%
  left_join(df_meteo_pieges_summ_wide10) %>%
  left_join(df_meteo_pieges_summ_wide11) %>%
  left_join(df_meteo_pieges_summ_wide12) %>%
  left_join(df_meteo_pieges_summ_wide13) %>%
  left_join(df_meteo_pieges_summ_wide14) %>%
  left_join(df_meteo_pieges_summ_wide15) %>%
  left_join(df_meteo_pieges_summ_wide16) %>%
  left_join(df_meteo_pieges_summ_wide17)


df_model <- pieges_data %>%
  dplyr::select(idpointdecapture , nom_commune, num_piege, date_releve_jour, effectif_jour_PP ) %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance, by = "idpointdecapture")

write.csv(df_model, "df_model.csv", row.names = F)
