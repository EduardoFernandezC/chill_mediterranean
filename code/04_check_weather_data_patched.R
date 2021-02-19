library(tidyverse)
library(chillR)
library(dormancyR)

# This script is to implement a quality check on the patching procedure performed in 03_patching_missing_days.R


# Compute the number of missing days for each weather station after the patching procedure

quality_check_patched <- lapply(weather_patched, function(x) perc_complete(x[["weather"]]))

# Transform the quality check list into a data frame

quality_check_patched_df <- bind_rows(quality_check_patched, .id = "Weather station")

# Get the relevant information in the quality check list to be passed to the weather stations data frame

quality_check_patched_df <- filter(quality_check_patched_df, Variable %in% c("Tmin", "Tmax"))

# Pivot wider to get the data in the right format to merge with the weather stations data frame

quality_check_patched_df <- pivot_wider(quality_check_patched_df, names_from = "Variable",
                                        values_from = c("Percentage", "Total_missing"))

# Extract the number of weather stations used for each station in the patching procedure

quality_check_patched_stations_used <- lapply(weather_patched,
                                              function(x) c(WS_used_Tmin = length(x[["statistics"]][["Tmin"]]),
                                                            WS_used_Tmax = length(x[["statistics"]][["Tmax"]])))

# Add the information the quality check df

quality_check_patched_stations_used_df <- bind_rows(quality_check_patched_stations_used,
                                                    .id = "Weather station")

# Join the data frames

quality_check_patched_df <- left_join(quality_check_patched_df, quality_check_patched_stations_used_df,
                                      by = c("Weather station"))



# Save the patched list into the folder

# Create the folder

dir.create("data/historic_temps_patched")

# Apply the save temperature scenarios function

save_temperature_scenarios(lapply(weather_patched, function(x) x[["weather"]]),
                           "data/historic_temps_patched/", prefix = "temps_patched")

# Save the data frame of stations as well as the df of quality metrics

write.csv(quality_check_patched_df, "data/quality_check_patching.csv", row.names = FALSE)
  
  
  
  
