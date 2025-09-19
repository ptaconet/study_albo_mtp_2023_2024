# meteo <- read.csv(file.path("data","processed","data_meteofrance_2022_2025.csv")) %>%
#   rename(site = nom_commune) %>%
#   mutate(date = as.Date(date)) %>%
#   dplyr::select(site ,date,RR,TM,UM)

meteo_perols <- openmeteo::weather_history(
  location = c("PEROLS"),
  daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
  model = "meteofrance_arome_france_hd",
  start = as.Date("2024-09-01"),
  end = today())

# meteo_bayonne <- openmeteo::weather_history(
#   location = c("BAYONNE"),
#   daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
#   model = "meteofrance_arome_france_hd",
#   start = as.Date("2024-09-01"),
#   end = today())
#
# meteo_stmedard <- openmeteo::weather_history(
#   location = c("SAINT-MEDARD-EN-JALLES"),
#   daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
#   model = "meteofrance_arome_france_hd",
#   start = as.Date("2024-09-01"),
#   end = today())
#
# meteo_murviel <- openmeteo::weather_history(
#   location = c("MURVIEL-LES-MONTPELLIER"),
#   daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
#   model = "meteofrance_arome_france_hd",
#   start = as.Date("2024-09-01"),
#   end = today())

meteo_muret <- openmeteo::weather_history(
  location = c("MURET"),
  daily = c("temperature_2m_mean","relative_humidity_2m_mean","precipitation_sum"),
  model = "meteofrance_arome_france_hd",
  start = as.Date("2024-09-01"),
  end = today())


meteo_perols <- meteo_perols %>%
  mutate(site="PEROLS") %>%
  rename(TM = daily_temperature_2m_mean, UM = daily_relative_humidity_2m_mean, RR = daily_precipitation_sum)
#
# meteo_bayonne <- meteo_bayonne %>%
#   mutate(site="BAYONNE") %>%
#   rename(TM = daily_temperature_2m_mean, UM = daily_relative_humidity_2m_mean, RR = daily_precipitation_sum)
#
# meteo_stmedard <- meteo_stmedard %>%
#   mutate(site="SAINT-MEDARD-EN-JALLES") %>%
#   rename(TM = daily_temperature_2m_mean, UM = daily_relative_humidity_2m_mean, RR = daily_precipitation_sum)
#
# meteo_murviel <- meteo_murviel %>%
#   mutate(site="MURVIEL-LES-MONTPELLIER") %>%
#   rename(TM = daily_temperature_2m_mean, UM = daily_relative_humidity_2m_mean, RR = daily_precipitation_sum)

meteo_muret <- meteo_muret %>%
  mutate(site="MURET") %>%
  rename(TM = daily_temperature_2m_mean, UM = daily_relative_humidity_2m_mean, RR = daily_precipitation_sum)

#meteo <- rbind(meteo_perols, meteo_bayonne, meteo_stmedard, meteo_murviel, meteo_muret)
meteo <- rbind(meteo_perols, meteo_muret)


lag_max <- 84

meteo2 <- meteo %>%
  dplyr::select(site,date) %>%
  mutate(year = year(date), week = week(date), weekday = wday(date)) %>%
  filter(weekday==1) %>%
  slice(rep(1:n(), each = lag_max)) %>%
  group_by(site, year, week) %>%
  mutate(lag_n = row_number()) %>%
  ungroup() %>%
  dplyr::select(-weekday) %>%
  rename(th_date = date) %>%
  mutate(date = th_date - lag_n)


# summarizing to weeks
meteo3 <- meteo2 %>%
  left_join(meteo, by = c("date","site")) %>%
  pivot_longer(!(site:date), names_to = "var", values_to = 'val')



#### Functions

fun_summarize_week <- function(meteo3,var_to_summarize,fun_summarize,new_var_name,n_days_agg){

  if(fun_summarize=="sum"){
    meteo3_summarize <- meteo3 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture, lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
      dplyr::summarise(val=sum(val, na.rm = T),date = max(date)) %>%
      group_by(site, th_date) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  } else if (fun_summarize == "mean"){
    meteo3_summarize <- meteo3 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
      summarise(val=mean(val, na.rm = T),date = max(date)) %>%
      group_by(site, th_date) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "max"){
    meteo3_summarize <- meteo3 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
      summarise(val=max(val, na.rm = T),date = max(date)) %>%
      group_by(site, th_date) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }  else if (fun_summarize == "min"){
    meteo3_summarize <- meteo3 %>%
      filter(var==var_to_summarize) %>%
      #group_by(idpointdecapture,lag_n = lubridate::week(date), year = lubridate::year(date)) %>%
      mutate(lag_n = floor(lag_n/n_days_agg)) %>%  # 7 is for 7 days
      group_by(site, th_date, lag_n, year = lubridate::year(date)) %>%
      summarise(val=min(val, na.rm = T),date = max(date)) %>%
      group_by(site, th_date) %>%
      mutate(lag_n=seq(0,n()-1,1)) %>%
      mutate(var = new_var_name) %>%
      as_tibble() %>%
      dplyr::select(-year)
  }
  return(meteo3_summarize)

}

df_meteo_pieges_summ <- fun_summarize_week(meteo3,"RR","sum","RR",7) %>%
  bind_rows(fun_summarize_week(meteo3,"TM","mean","TM",7)) %>%
  bind_rows(fun_summarize_week(meteo3,"UM","mean","UM",7))

## je ne sais pas pourquoi cela va jusque 8 parfois, mais on enleve pour avoir 7 (0+7, donc 8 en tout) semaines
df_meteo_pieges_summ <- df_meteo_pieges_summ %>% filter(lag_n<12)

# function to create the data.frame for CCM
fun_ccm_df <- function(df_timeseries, varr, function_to_apply){

  df_timeseries_wide <- df_timeseries %>%
    filter(var==varr) %>%
    dplyr::select(-c("date","var")) %>%
    arrange(lag_n) %>%
    pivot_wider(values_from = val, names_from = lag_n, names_prefix = paste0(varr,"_"))

  max_col <- ncol(df_timeseries_wide)

  for(i in 3:(max_col-1)){
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

  for(i in 3:max_col){
    colnames(df_timeseries_wide)[i] <- paste0(colnames(df_timeseries_wide)[i],"_",sub('.*\\_', '', colnames(df_timeseries_wide)[i]))
  }

  return(df_timeseries_wide)

}



df_meteo_pieges_summ_wide1 <- fun_ccm_df(df_meteo_pieges_summ,"RR","sum")
df_meteo_pieges_summ_wide2 <- fun_ccm_df(df_meteo_pieges_summ,"TM","mean")
df_meteo_pieges_summ_wide3 <- fun_ccm_df(df_meteo_pieges_summ,"UM","mean")


df_meteo_pieges_summ_wide_meteofrance <- df_meteo_pieges_summ_wide1 %>%
  left_join(df_meteo_pieges_summ_wide2) %>%
  left_join(df_meteo_pieges_summ_wide3)


df_meteo_predictions <- df_meteo_pieges_summ_wide_meteofrance %>%
  dplyr::select(site,th_date,TM_0_8,UM_5_11,TM_0_4, UM_0_11,RR_1_5, TM_0_3,UM_2_2,RR_3_3) %>%
  rename(date=th_date)


write.csv(df_meteo_predictions,file.path("data","processed","df_meteo_predictions_2025.csv"), row.names = F)
