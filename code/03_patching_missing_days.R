# Implement the patching procedure

# This script relies on running the 02_check_weather_data.R so we can source it...

source("code/02_check_weather_data.R")

# In this project, we are going to implement a different patching approach compared to our previous works.
# We will estimate the bias for Tmin and Tmax for each station compared to the complete list of available
# auxiliary records.


# This step will prepare the list of weather stations ordered by Tmin and Tmax bias

bias_patching_info <- list()

for (k in 1 : length(weather_data_90)){
  for (i in 1 : length(patching_list_20)){
    
    # Merge the data for the primary weather station with the data from the auxiliary sources
    prim_plus_aux <- left_join(weather_data_90[[k]], patching_list_20[[i]],
                               by = c("DATE", "Year", "Month", "Day"))
    
    # Compute the bias for tmin and tmax inside the prim_plus_aux data frame
    prim_plus_aux["bias_tmin"] <- prim_plus_aux$Tmin.x - prim_plus_aux$Tmin.y
    prim_plus_aux["bias_tmax"] <- prim_plus_aux$Tmax.x - prim_plus_aux$Tmax.y
    
    # Compute some metrics for both the tmin and tmax variables
    bias_tmin <- mean(prim_plus_aux$bias_tmin, na.rm = TRUE)
    sd_tmin <- sd(prim_plus_aux$bias_tmin, na.rm = TRUE)
    days_tmin <- length(which(!is.na(prim_plus_aux$bias_tmin)))
    perc_tmin <- (days_tmin / nrow(prim_plus_aux)) * 100
    
    bias_tmax <- mean(prim_plus_aux$bias_tmax, na.rm = TRUE)
    sd_tmax <- sd(prim_plus_aux$bias_tmax, na.rm = TRUE)
    days_tmax <- length(which(!is.na(prim_plus_aux$bias_tmax)))
    perc_tmax <- (days_tmax / nrow(prim_plus_aux)) * 100
    
    # Implement a dataframe summarizing the metrics computed
    metrics <- data.frame(Primary_station = names(weather_data_90[k]),
                          Auxiliary_station = names(patching_list_20[i]),
                          Bias_tmin = bias_tmin,
                          Bias_tmax = bias_tmax,
                          Bias_tmin_abs = abs(bias_tmin),
                          Bias_tmax_abs = abs(bias_tmax),
                          sd_tmin = sd_tmin,
                          sd_tmax = sd_tmax,
                          days_tmin = days_tmin,
                          days_tmax = days_tmax,
                          perc_tmin = perc_tmin, 
                          perc_tmax = perc_tmax)
    
    # Merge the data frame to start again with the auxiliary weather station i + 1
    if (i == 1) bias_summary <- metrics else bias_summary <- bind_rows(bias_summary, metrics)}
  
  # Order the summary data frame for the primary weather station k according to tmin bias
  bias_summary_order_tmin <- bias_summary[order(bias_summary$Bias_tmin_abs), ]
  
  # Implement the same but for tmax now
  bias_summary_order_tmax <- bias_summary[order(bias_summary$Bias_tmax_abs), ]
  
  # Remove the first row, since it corresponds to the same primary weather station (avoid redundancy)
  bias_summary_order_tmin <- bias_summary_order_tmin[-1, ]
  bias_summary_order_tmax <- bias_summary_order_tmax[-1, ]
  
  # Save the outputs in a list
  
  bias_patching_info[[k]] <- list(Tmin = bias_summary_order_tmin,
                                  Tmax = bias_summary_order_tmax)
  
  # Name the list
  names(bias_patching_info)[k] <- names(weather_data_90)[k]
  
  }


# Filter the bias patching info list according to mean bias. For now, I will set this to 3 celsius degree

bias_patching_info_filtered <- lapply(bias_patching_info, function(x){
                                                            Tmin <- filter(x[["Tmin"]], Bias_tmin_abs <= 3)
                                                            Tmax <- filter(x[["Tmax"]], Bias_tmax_abs <= 3)
  
                                                            return(list(Tmin = Tmin,
                                                                        Tmax = Tmax))})


# For loop to fill gaps by patching them with weather data from low-bias stations

weather_patched <- list()

for (i in 143 : length(weather_data_90)){
  
  # Weather stations used to patch the weather data in the primary source
  weather_stations_tmin <- bias_patching_info_filtered[[i]][["Tmin"]][["Auxiliary_station"]]
  weather_stations_tmax <- bias_patching_info_filtered[[i]][["Tmax"]][["Auxiliary_station"]]
  
  
  # Fill the gaps using data from low-bias weather stations
  tmin_patched <- patch_daily_temperatures(weather_data_90[[i]],
                                           patching_list_20[weather_stations_tmin],
                                           vars = "Tmin", max_mean_bias = 3, max_stdev_bias = 3)
  
  tmax_patched <- patch_daily_temperatures(weather_data_90[[i]],
                                           patching_list_20[weather_stations_tmax],
                                           vars = "Tmax", max_mean_bias = 3, max_stdev_bias = 3)
  
  
  # Merge both patched weather data frames
  
  weather_patched_df <- left_join(tmin_patched$weather[which(colnames(tmin_patched$weather) != "Tmax")],
                                  tmax_patched$weather[which(colnames(tmax_patched$weather) != "Tmin")],
                                  by = c("YEARMODA", "DATE", "Year", "Month", "Day", "Tmean", "Prec"))
  
  # Collect the results in a list
  
  weather_patched[[i]] <- list(weather = weather_patched_df,
                               statistics = list(Tmin = tmin_patched[["statistics"]],
                                                 Tmax = tmax_patched[["statistics"]]))
  
  # Set the name of the element i of the list
  names(weather_patched)[i] <- names(weather_data_90)[i]
  
  }










