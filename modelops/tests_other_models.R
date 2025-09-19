
df_meteo_predictions = df_meteo2

######################################################
######### Génération des prédictions
######################################################

# df_meteo_predictions$sin_doy = sin(2 * pi * lubridate::yday(df_meteo_predictions$date) / 365)
# df_meteo_predictions$cos_doy = cos(2 * pi * lubridate::yday(df_meteo_predictions$date) / 365)
 df_meteo_predictions$sin_doy = sin(lubridate::yday(df_meteo_predictions$date))
 df_meteo_predictions$cos_doy = cos(lubridate::yday(df_meteo_predictions$date))
 df_meteo_predictions$week = lubridate::week(df_meteo_predictions$date)


multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting2.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting2.rds")


df_meteo_predictions$pred_presence_prob <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions, type = 'prob')$Presence
df_meteo_predictions$pred_presence_absence <- predict(multiv_model_presence_nowcasting$model, df_meteo_predictions)

# with svmpoly
# df_model_pred_abundance <- df_meteo_predictions %>% dplyr::select(TM_0_4, UM_0_11, RR_1_5, cos_doy, week )
# df_meteo_predictions <- df_meteo_predictions %>%
#   mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
#   mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,df_model_pred_abundance)))) %>%
#   mutate(date=as.Date(date))

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(presence_flag = ifelse(pred_presence_absence=="Absence",0, 1)) %>%
  mutate(pred_abundance = ifelse(pred_presence_absence=="Absence",0,exp(predict(multiv_model_abundance_nowcasting$model,.))))

df_meteo_predictions <- df_meteo_predictions %>%
  left_join(coords_retain)


######################################################
######### Rasterisation
######################################################

# to create a regular grid (evenly spaced)
grid_res <- 0.05

df_meteo_predictions <- df_meteo_predictions %>%
  mutate(
    X_snap = round(X / grid_res) * grid_res,
    Y_snap = round(Y / grid_res) * grid_res
  )

res_x <- min(diff(sort(unique(df_meteo_predictions$X_snap))))
res_y <- min(diff(sort(unique(df_meteo_predictions$Y_snap))))

# Compute raster extent from centers
xmin <- min(df_meteo_predictions$X_snap) - res_x / 2
xmax <- max(df_meteo_predictions$X_snap) + res_x / 2
ymin <- min(df_meteo_predictions$Y_snap) - res_y / 2
ymax <- max(df_meteo_predictions$Y_snap) + res_y / 2

# Create template raster
r_template <- rast(
  extent = c(xmin, xmax, ymin, ymax),
  resolution = c(res_x, res_y),
  crs = "EPSG:4326"
)

# Make sure date is character for naming
df_meteo_predictions$date <- as.character(df_meteo_predictions$date)

# Convert to SpatVector
v <- vect(df_meteo_predictions, geom = c("X_snap", "Y_snap"), crs = "EPSG:4326")

# Unique dates
dates <- unique(df_meteo_predictions$date)

# Rasterize each date separately
rasters <- lapply(dates, function(d) {
  v_d <- v[v$date == d, ]
  r <- terra::rasterize(v_d, r_template, field = "pred_abundance", fun = "mean")
  names(r) <- d
  #d = gsub("-","",d)
  #writeRaster(r,paste0("data/raw/results_raster_2024/aedesabundance_",d,".tif"))
  r

})

# Combine into a single multilayer raster
r_stack <- rast(rasters)



######################################################
######### Aggrégation au departement et à la commune pour les prédictions entomo
######################################################

communes <- st_read("modelops/data/raw/communes_simplified.gpkg")

library(exactextractr)

communes_mean_abundance <- exact_extract(r_stack, communes, c('mean')) %>%
  bind_cols(communes) %>%
  pivot_longer(cols = starts_with("mean"),names_to = "date",values_to = "mean_abundance") %>%
  mutate(date = gsub("\\.","-",date)) %>%
  mutate(date = gsub("mean-","",date)) %>%
  dplyr::select(codgeo, date, mean_abundance) %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.nan(mean_abundance))

df_model_pred <- communes_mean_abundance %>% filter(codgeo %in% c(31395,34198)) %>% mutate(site = ifelse(codgeo==34198,"PEROLS","MURET"))



df_pieges <- read.csv(file.path("data","processed","df_pieges_2025.csv")) %>%
  rename(site = Site) %>%
  group_by(site, daterec) %>%
  summarise(effectif_jour = mean(effectif_jour))   %>% filter(site %in% c("PEROLS","MURET")) %>%
  ungroup() %>%
  mutate(daterec=as.Date(daterec))



ggplot() +
  geom_line(data = df_pieges, aes(x=daterec, y = effectif_jour, group = site), color = "black") +
  geom_line(data = df_model_pred, aes(x=date, y = mean_abundance, group = site), color = "red") +
  geom_point(data = df_pieges, aes(x=daterec, y = effectif_jour, group = site), color = "black", size = 1) +
  geom_point(data = df_model_pred, aes(x=date, y = mean_abundance, group = site), color = "red", size = 1) +
  facet_wrap(.~site, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  +
  scale_x_date(limits = c(as.Date("2024-10-01"),as.Date("2025-09-15")),
               breaks = seq(as.Date("2024-10-01"), as.Date("2025-09-15"), by = "1 month"),
               minor_breaks = seq(as.Date("2024-10-01"), as.Date("2025-09-15"), by = "1 month"),
               date_labels = "%Y-%m-%d")
