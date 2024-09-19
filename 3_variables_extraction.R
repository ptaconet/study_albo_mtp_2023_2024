library(tidyverse)

meteo <- read.csv(file.path("data","processed","data_meteofrance_2023_2024.csv")) %>%
  rename(site = nom_commune) %>%
  mutate(date = as.Date(date)) %>%
  mutate(TN_GDD = ifelse(TN>11, TN, 11),TX_GDD = case_when(TX<11~11, TX==11~11, TX==30~30, 11<TX & TX<30~TX, TX>30~30), GDDjour=(TX_GDD+TN_GDD)/2-11 ) %>%
  dplyr::select(-TN_GDD) %>%
  dplyr::select(-TX_GDD)

meteo2 <- meteo %>% filter(site=="PEROLS") %>% mutate(site="MONTPELLIER")

meteo = bind_rows(meteo,meteo2)

pieges_data <- read.csv( file.path("data","processed","df_pieges.csv")) %>%
  filter(!is.na(date_releve)) %>%
  mutate(num_releve = seq(1,nrow(.),1)) %>%
  mutate(idpointdecapture = paste0(num_piege,"_",num_releve))


lag_max <- 48



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

fun_summarize_week <- function(df_meteo_pieges2,var_to_summarize,fun_summarize,new_var_name){

  if(fun_summarize=="sum"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture, lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/7)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      dplyr::summarise(val=sum(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  } else if (fun_summarize == "mean"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/7)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=mean(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "max"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/7)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=max(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "min"){
    df_meteo_pieges2_summarize <- df_meteo_pieges2 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/7)) %>%  # 7 is for 7 days
      group_by(idpointdecapture, lag_n, year = lubridate::year(date)) %>%
      summarise(val=min(val, na.rm = T),date = max(date)) %>%
      group_by(idpointdecapture) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }
  return(df_meteo_pieges2_summarize)

}


meteo <- meteo %>%
  #filter(!is.na(RFD), !is.na(TMIN), !is.na(TMAX), !is.na(TMN), !is.na(TAMP), !is.na(TSD), !is.na(RHMIN), !is.na(RHMAX), !is.na(RHMN), !is.na(RHAMP), !is.na(RHSD), !is.na(WINDMIN), !is.na(WINDMAX), !is.na(WINDMN) ) %>%
  mutate(date = as.character(date))


pieges_data2 <- pieges_data %>%
  dplyr::select(idpointdecapture, num_releve, num_piege, date_releve, site) %>%
  mutate(date_releve=as.Date(date_releve))


df_meteo_pieges <- data.frame(idpointdecapture = character(), num_releve = numeric(),num_piege = character(), site = character(), date = character(), stringsAsFactors = F)
for(i in 1:nrow(pieges_data2)){
  cat(i/nrow(pieges_data2)*100,"%\n")
  for(j in 0:lag_max){
    df_meteo_pieges <- rbind(df_meteo_pieges,
                             data.frame(idpointdecapture = pieges_data2$idpointdecapture[i],
                                        num_releve = pieges_data2$num_releve[i],
                                        num_piege = pieges_data2$num_piege[i],
                                        site = pieges_data2$site[i],
                                        date = as.character(as.Date(pieges_data2$date_releve[i]-j)),
                                        lag_n = j,
                                        stringsAsFactors = F))
  }
}

# summarizing to weeks
df_meteo_pieges2 <- df_meteo_pieges %>%
  left_join(meteo, by = c("date","site")) %>%
  pivot_longer(!(idpointdecapture:lag_n), names_to = "var", values_to = 'val')



df_meteo_pieges_summ <- fun_summarize_week(df_meteo_pieges2,"RR","sum","RR") %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"RR","max","RRMAX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"DRR","sum","DRR")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TN","min","TN")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TX","max","TX")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TM","mean","TM")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"TAMPLI","mean","TAMPLI")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"FFM","mean","FFM")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"FXY","mean","FXY")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"UM","mean","UM")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDjour","sum","GDDjour")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDacc","mean","GDDacc")) %>%
  bind_rows(fun_summarize_week(df_meteo_pieges2,"GDDbound","mean","GDDbound"))



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
      } else if (function_to_apply=="max"){
        df_timeseries_wide[column_name] <- max(df_timeseries_wide[,i:j], na.rm = T)
      } else if (function_to_apply=="min"){
        df_timeseries_wide[column_name] <- min(df_timeseries_wide[,i:j], na.rm = T)
      }
    }
  }

  for(i in 2:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)

}



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RR","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"RRMAX","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"DRR","sum")
df_meteo_pieges_summ_wide4 <- fun_ccm_df(df_meteo_pieges_summ,"TN","mean")
df_meteo_pieges_summ_wide5 <- fun_ccm_df(df_meteo_pieges_summ,"TX","mean")
df_meteo_pieges_summ_wide6 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide7 <- fun_ccm_df(df_meteo_pieges_summ,"TAMPLI","mean")
df_meteo_pieges_summ_wide8 <- fun_ccm_df(df_meteo_pieges_summ,"FFM","mean")
df_meteo_pieges_summ_wide9 <- fun_ccm_df(df_meteo_pieges_summ,"FXY","mean")
df_meteo_pieges_summ_wide10 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")
df_meteo_pieges_summ_wide11 <- fun_ccm_df(df_meteo_pieges_summ,"GDDjour","sum")
df_meteo_pieges_summ_wide12 <- fun_ccm_df(df_meteo_pieges_summ,"GDDacc","mean")
df_meteo_pieges_summ_wide13 <- fun_ccm_df(df_meteo_pieges_summ,"GDDbound","mean")




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
  left_join(df_meteo_pieges_summ_wide13)




df_model <- pieges_data %>%
  dplyr::select(idpointdecapture , site, num_piege, Latitude, Longitude , date_releve, effectif_jour_PP ) %>%
  left_join(df_meteo_pieges_summ_wide_meteofrance, by = "idpointdecapture")

write.csv(df_model, "df_model.csv", row.names = F)
