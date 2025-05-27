library(terra)

fun_create_df_from_nc_drias <- function(path_to_nc_drias, var, start_week_offset, end_week_offset, fun_to_apply){

  r <- rast(path_to_nc_drias)
  r <- terra::flip(r,"vertical")
  ext(r) <- c(-1.83808,9.89639,41.3183,45.9452)

  if(var == 'precipitation'){
    r = r*86400 # conversion from kg/m2/s to mm
  }
  if(var == "temperature"){
    r = r-273.15
  }
  if(var == "humidity"){

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

  for (i in seq_along(weeks)) {
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
  }

  # Combine results into a SpatRaster stack
  result_stack <- rast(weekly_means)
  names(result_stack) <- paste0("moving_window_mean_week_", seq_along(weekly_means))
  time(result_stack) <- do.call(c, weekly_dates)


  return(result_stack)

}


nc_hum <- "/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/data/raw/climate_change/scenario45_2050_2059/tasAdjustprtotAdjusthussAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231_25052712142934068/hussAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"
nc_temp <- "/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/data/raw/climate_change/scenario45_2050_2059/tasAdjustprtotAdjusthussAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231_25052712142934068/tasAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"
nc_rr <- "/home/ptaconet/contributions_diverses_projets_mivegec/study_albo_mtp_2023_2024/data/raw/climate_change/scenario45_2050_2059/tasAdjustprtotAdjusthussAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231_25052712142934068/prtotAdjust_France_IPSL-IPSL-CM5A-MR_IPSL-WRF381P_rcp4.5_METEO-FRANCE_ADAMONT-France_SAFRAN_day_20500101-20591231.nc"

TM_0_8 <- fun_create_df_from_nc_drias(nc_temp,"temperature",-8,0, "mean")
UM_5_11 <- fun_create_df_from_nc_drias(nc_hum,"humidity",-11,-5, "mean")
TM_0_4 <- fun_create_df_from_nc_drias(nc_temp,"temperature",-4,0, "mean")
UM_0_11 <- fun_create_df_from_nc_drias(nc_hum,"humidity",-11,0, "mean")
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

preds <- list()

for(i in 1:length(common_dates)){

  cat(i)
  # presence models
  t <- TM_0_8[[which(time(TM_0_8) == common_dates[i])]]
  h <- UM_5_11[[which(time(UM_5_11) == common_dates[i])]]

  raster <- c(t[[1]],h[[1]])

  pred <- terra::predict(raster, multiv_model_presence_nowcasting$model, na.rm = TRUE, type = 'prob')$Presence
  time(pred) <- common_dates[i]
  preds[[i]] <- pred
}


result_presence <- rast(preds)


# Suppose r is your SpatRaster with weekly layers over 10 years
dates <- as.POSIXct(time(result_stack), origin = "1970-01-01", tz = "UTC")

# Get week number of each date (1-53)
week_nums <- isoweek(dates)  # lubridate::isoweek gives ISO week numbers

unique_weeks <- sort(unique(week_nums))

weekly_averages <- list()

for (w in unique_weeks) {
  # indices of layers for week w
  idx <- which(week_nums == w)

  # subset raster layers
  r_sub <- result_presence[[idx]]

  # calculate mean across these layers
  r_mean <- mean(r_sub, na.rm = TRUE)

  weekly_averages[[as.character(w)]] <- r_mean
}

# Combine averaged layers into one SpatRaster
r_weekly_avg <- rast(weekly_averages)
names(r_weekly_avg) <- paste0("week_", unique_weeks)

# Plot with terra, set breaks to control color mapping
colors <- colorRampPalette(c("blue", "white", "red"))(10)

# Set breaks so that 0.5 corresponds roughly to middle color (50th break)

breaks <- seq(0,1, length.out = 11)

plot(r_weekly_avg, col=colors, breaks=breaks)

