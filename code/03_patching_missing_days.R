# Implement the patching procedure

# This script relies on running the 02_check_weather_data.R so we can source it...

source("code/02_check_weather_data.R")


# For loop to fill gaps by patching them with weather data from close weather stations

weather_patched <- list()

for (i in 1 : length(weather_data_90)){
  
  # Weather station of interest
  weather_station <- names(weather_data_90)[i]
  
  # Define the point of interest (latitude and longitude of WS "i")
  my_point <- patching_df_20[which(patching_df_20$STATION.NAME == weather_station), c("Long", "Lat")]
  
  # Create a temporary data frame for further use
  temp_df <- patching_df_20
  
  # Compute the distance between my point (the point of the i element of the list) and the other 711
  # weather stations in the data frame containing all the WS used in this project
  temp_df["distance"] <- round(sp::spDistsN1(as.matrix(temp_df[, c("Long", "Lat")]),
                                             c(my_point[1, 1], my_point[1, 2]), longlat = TRUE), 2)
  
  # Order the temporary data frame according to the distance between my point and the WS in the database
  temp_df <- temp_df[order(temp_df$distance), ]
  
  # Fill the gaps using data from the 40 closest weather stations
  weather_patched[[i]] <- patch_daily_temperatures(weather_data_90[[i]],
                                                   patching_list_20[temp_df[2 : 51, "STATION.NAME"]], 
                                                   max_mean_bias = 4, max_stdev_bias = 4)
  
  # Set the name of the element i of the list
  names(weather_patched)[i] <- weather_station
  
  }



