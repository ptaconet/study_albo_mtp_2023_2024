library(terra)
library(tidyverse)
library(sf)

safran <- st_read("data/raw/climate_change/grille_safran_elab/SafranDomain.shp")


scenario_year <- "baseline_1996_2005"

nc_hum <- paste0("data/raw/climate_change/",scenario_year,"/huss.nc")
nc_temp <- paste0("data/raw/climate_change/",scenario_year,"/tas.nc")
nc_rr <- paste0("data/raw/climate_change/",scenario_year,"/prtot.nc")


fun_create_df_from_nc_drias <- function(path_to_nc_drias, var, start_week_offset, end_week_offset, fun_to_apply, path_to_nc_temp = NULL){

  r <- rast(path_to_nc_drias)
  crs(r)  <- "epsg:4326"
  r <- terra::flip(r,"vertical")
  ext(r) <- ext(safran)

  if(var == 'precipitation'){
    r = r*86400 # conversion from kg/m2/s to mm
  }
  if(var == "temperature"){
    r = r-273.15
  }
  if(var == "humidity"){
    r2 <- rast(path_to_nc_temp)
    r2 <- terra::flip(r2,"vertical")
    ext(r2) <- c(-4.962154,9.573783,41.33729,51.04974)
    r2 <- r2-273.15

    p <- 1013  # pressure in hPa

    # Calculate vapor pressure e (hPa)
    e <- (r * p) / (0.622 + (1 - 0.622) * r)

    # Calculate saturation vapor pressure e_s (hPa)
    e_s <- 6.112 * exp((17.67 * r2) / (r2 + 243.5))

    # Calculate relative humidity (%)
    r <- (e / e_s) * 100

  }
  # Extract time as Date
  dates <- time(r)  # should return a Date or POSIXct vector
  head(dates)

  # Create a data.frame of time info
  time_df <- data.frame(
    date = dates,
    index = 1:length(dates),
    week = format(dates, "%Y-%U")  # Year-week string
  )

  # Get unique weeks, sorted
  weeks <- unique(time_df$week)
  weeks <- sort(weeks)

  # Prepare output list
  weekly_means <- list()
  weekly_dates <- list()

  n_weeks <- length(weeks)

  pb <- txtProgressBar(min = 0, max = length(weeks), style = 3)

  for (i in seq_along(weeks)) {
    #cat(i," ")
    # Calculate actual window indices relative to current week
    window_start <- i + start_week_offset
    window_end <- i + end_week_offset

    # Skip if window outside valid range
    if (window_start < 1 || window_end > n_weeks || window_start > window_end) {
      next
    }

    # Define window weeks
    window_weeks <- weeks[window_start:window_end]

    # Find raster indices for window weeks
    window_dates <- time_df %>%
      filter(week %in% window_weeks) %>%
      pull(index)

    # Subset raster layers and compute mean
    r_window <- r[[window_dates]]

    if(fun_to_apply == "mean"){
     r_mean <- mean(r_window, na.rm = TRUE)
    } else if(fun_to_apply == "sum"){
      r_mean <- sum(r_window, na.rm = TRUE)
    }

    # Store results
    weekly_means[[length(weekly_means) + 1]] <- r_mean
    weeks_iso <- gsub("-","-W",weeks[i])
    weeks_iso <- gsub("W00","W01",weeks_iso)
    weekly_dates[[length(weekly_dates) + 1]] <- as.Date(ISOweek::ISOweek2date(paste0(weeks_iso, "-1")))

    setTxtProgressBar(pb, i)
  }

  close(pb)
  # Combine results into a SpatRaster stack
  result_stack <- rast(weekly_means)
  names(result_stack) <- paste0("moving_window_mean_week_", seq_along(weekly_means))
  time(result_stack) <- do.call(c, weekly_dates)

  result_stack <- crop(result_stack, vect(v))

  return(result_stack)

}



TM_0_8 <- fun_create_df_from_nc_drias(nc_temp,"temperature",-8,0, "mean")
UM_5_11 <- fun_create_df_from_nc_drias(nc_hum,"humidity",-11,-5, "mean", nc_temp)
TM_0_4 <- fun_create_df_from_nc_drias(nc_temp,"temperature",-4,0, "mean")
UM_0_11 <- fun_create_df_from_nc_drias(nc_hum,"humidity",-11,0, "mean", nc_temp)
RR_1_5 <- fun_create_df_from_nc_drias(nc_rr,"precipitation",-5,-1, "sum")

names(TM_0_8) <- rep("TM_0_8",length(names(TM_0_8)))
names(UM_5_11) <- rep("UM_5_11",length(names(UM_5_11)))
names(TM_0_4) <- rep("TM_0_4",length(names(TM_0_4)))
names(UM_0_11) <- rep("UM_0_11",length(names(UM_0_11)))
names(RR_1_5) <- rep("RR_1_5",length(names(RR_1_5)))


multiv_model_presence_nowcasting <- readRDS("res_multiv_model_presence_nowcasting.rds")
multiv_model_abundance_nowcasting <- readRDS("res_multiv_model_abundance_nowcasting.rds")


common_dates <- Reduce(intersect, list(time(TM_0_8), time(UM_5_11), time(TM_0_4), time(UM_0_11), time(RR_1_5)))
common_dates <- as.Date(common_dates)

preds_presence <- list()
preds_abundance <- list()

pb <- txtProgressBar(min = 0, max = length(common_dates), style = 3)
for(i in 1:length(common_dates)){

  # presence models
  t <- TM_0_8[[which(time(TM_0_8) == common_dates[i])]]
  h <- UM_5_11[[which(time(UM_5_11) == common_dates[i])]]

  raster_presence <- c(t[[1]],h[[1]])

  pred_presence <- terra::predict(raster_presence, multiv_model_presence_nowcasting$model, na.rm = TRUE, type = 'prob')$Presence
  time(pred_presence) <- common_dates[i]
  preds_presence[[i]] <- pred_presence


  # abundance models
  t <- TM_0_4[[which(time(TM_0_4) == common_dates[i])]]
  h <- UM_0_11[[which(time(UM_0_11) == common_dates[i])]]
  r <- RR_1_5[[which(time(RR_1_5) == common_dates[i])]]

  raster_abundance <- c(t[[1]],h[[1]],r[[1]])

  pred_abundance <- terra::predict(raster_abundance, multiv_model_abundance_nowcasting$model, na.rm = TRUE)
  time(pred_abundance) <- common_dates[i]
  preds_abundance[[i]] <- exp(pred_abundance)

  setTxtProgressBar(pb, i)

}
close(pb)


result_presence <- rast(preds_presence)
result_abundance <- rast(preds_abundance)


# Suppose r is your SpatRaster with weekly layers over 10 years
dates <- as.POSIXct(time(result_presence), origin = "1970-01-01", tz = "UTC")

# Get week number of each date (1-53)
week_nums <- isoweek(dates)  # lubridate::isoweek gives ISO week numbers

unique_weeks <- sort(unique(week_nums))

weekly_averages_pres <- list()
weekly_averages_abun <- list()

for (w in unique_weeks) {
  # indices of layers for week w
  idx <- which(week_nums == w)

  # subset raster layers
  r_sub_pres <- result_presence[[idx]]
  r_sub_abun <- result_abundance[[idx]]

  # calculate mean across these layers
  r_mean_pres <- mean(r_sub_pres, na.rm = TRUE)
  r_mean_abun <- mean(r_sub_abun, na.rm = TRUE)

  weekly_averages_pres[[as.character(w)]] <- r_mean_pres
  weekly_averages_abun[[as.character(w)]] <- r_mean_abun

}

# Combine averaged layers into one SpatRaster
r_weekly_avg_pres <- rast(weekly_averages_pres)
names(r_weekly_avg_pres) <- paste0("week_", unique_weeks)

r_weekly_avg_abun <- rast(weekly_averages_abun)
names(r_weekly_avg_abun) <- paste0("week_", unique_weeks)

# create a raster that combines presence and abundance
r_pres_abundance <- ifel(r_weekly_avg_pres >= 0.5, r_weekly_avg_abun, 0)



# Plot with terra, set breaks to control color mapping
#colors <- colorRampPalette(c("blue", "white", "red"))(10)
# Set breaks so that 0.5 corresponds roughly to middle color (50th break)
# breaks <- seq(0,1, length.out = 11)
# plot(r_weekly_avg, col=colors, breaks=breaks)


#  library(tidyterra)
# ggplot() +
#   geom_spatraster(data = r_pres_abundance) +
#   facet_wrap(~lyr)+
#   scale_fill_gradientn(
#     colours = c("blue", "white", "red"),  # 0 = blue, high = red
#     values = scales::rescale(c(0, 0.001, 50)),
#     limits = c(0, 50),
#     oob = scales::squish
#   ) +
#   theme_minimal()


# extract values
vals_pres_abundance2 <- as.data.frame(r_pres_abundance, xy = TRUE)

write.csv(vals_pres_abundance2,paste0("data/raw/climate_change/results/",scenario_year,"_all_coordinates.csv"), row.names = F)


data_long <- vals_pres_abundance2 %>%
  dplyr::select(-c("x","y")) %>%
  gather(factor_key=TRUE) %>%
  group_by(key) %>%
  summarise(mean= mean(value, na.rm = T), sd= sd(value, na.rm = T), max = max(value, na.rm = T),min = min(value, na.rm = T)) %>%
  rename(week = key) %>%
  mutate(week = as.numeric(gsub(".*_","",week))) %>%
  mutate(scenario = scenario_year)


write.csv(data_long,paste0("data/raw/climate_change/results/",scenario_year,"_all.csv"), row.names = F)


# values by departement
departements <- st_read("data/admin_data/departements-20180101-shp/departements-20180101.shp")
departements$ID <- seq(1:nrow(departements))

mean <- terra::extract(r_pres_abundance,departements, fun = "mean", na.rm = T)
sd <- terra::extract(r_pres_abundance,departements, fun = "sd", na.rm = T)

mean <- mean %>% left_join(departements) %>% dplyr::select(week_1:nom) %>% dplyr::select(-code_insee) %>% pivot_longer(-nom) %>% dplyr::filter(!is.nan(value)) %>% rename(mean = value, week = name)
sd <- sd %>% left_join(departements) %>% dplyr::select(week_1:nom) %>% dplyr::select(-code_insee) %>% pivot_longer(-nom) %>% dplyr::filter(!is.nan(value)) %>% rename(sd = value, week = name)

all <- full_join(mean,sd) %>%
  mutate(week = as.numeric(gsub(".*_","",week))) %>%
  rename(departement = nom) %>%
  mutate(scenario = scenario_year)

write.csv(all,paste0("data/raw/climate_change/results/",scenario_year,"_bydepartement.csv"), row.names = F)

#ggplot(all, aes(x = week, y = mean)) + geom_line() + facet_wrap(.~departement)




 # list.files(file.path("data","raw","climate_change","results"), full.names = T, pattern = "departement") %>%
 #  purrr::map_dfr(.,~read.csv(.)) %>%
 #  ggplot(aes(x = week, y = mean, group = scenario, colour = scenario)) + geom_line() + facet_wrap(.~departement)
 #
 #
 # list.files(file.path("data","raw","climate_change","results"), full.names = T, pattern = "all") %>%
 #  purrr::map_dfr(.,~read.csv(.)) %>%
 #   ggplot(aes(x = week, y = mean, group = scenario, colour = scenario)) + geom_line()
