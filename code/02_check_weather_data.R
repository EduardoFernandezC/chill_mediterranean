library(chillR)
library(dormancyR)
library(tidyverse)

# This script will upload the data and implement some functions to compute metrics related to data completeness

# Load the weather station data frame

weather_stations <- read.csv("data/weather_stations.csv")

# Repeat some steps from the script 01_download_weather_data.R to modify the weather stations data frame

# Check the weather_stations data frame for duplicated weather stations

weather_stations[duplicated(weather_stations$STATION.NAME), ]

# There are 6 repeated weather stations. For now, we will retain them to later check which has the longest
# record. We will add an ID to identify them

repeated_WS <- paste0(weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"], "_2")

# Replace the stations name in the main data frame with the name plus the ID

weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"] <- repeated_WS

# Remove weird characters ("/") from the name of the weather stations for later purposes

weather_stations[["STATION.NAME"]] <- gsub("/", "-", weather_stations$STATION.NAME)


# Load the data into R

weather_data <- load_temperature_scenarios("data/historic_temps/",
                                           "downloaded_data")


# Exclude the weather stations having no data for the period of interest from the weather stations data frame

weather_stations_filtered <- weather_stations[which(weather_stations$STATION.NAME %in% names(weather_data)), ]

# Compute the percentage of data complete for each weather station

quality_check <- lapply(weather_data, perc_complete)

# Transform the quality check list into a data frame

quality_check_df <- bind_rows(quality_check, .id = "Weather station")

# Get the relevant information in the quality check list to be passed to the weather stations data frame

quality_check_df <- filter(quality_check_df, Variable %in% c("Tmin", "Tmax"))

# Pivot wider to get the data in the right format to merge with the weather stations data frame

quality_check_df <- pivot_wider(quality_check_df, names_from = "Variable",
                                values_from = c("Percentage", "Total_missing"))

# Add the information to the weather stations data frame

weather_stations_filtered <- left_join(weather_stations_filtered, quality_check_df,
                                       by = c("STATION.NAME" = "Weather station"))

# Select those weather stations having at least 90% of data complete

weather_stations_90_complete <- weather_stations_filtered[weather_stations_filtered$Percentage_Tmin >= 90 &
                                                            weather_stations_filtered$Percentage_Tmax >= 90, ]


# Visualize the location of weather stations with at least 90% of data complete 

# Define the borders of the countries across the Mediterranean

countries <- borders("world",
                     fill = "goldenrod", colour = "grey40",
                     size = 0.3)

# Plot the stations in a map

ggplot() + countries + 
  geom_point(aes(Long, Lat), data = weather_stations_90_complete, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))

ggsave("figures/available_WS_90_complete.png", height = 5.05, width = 6.5, dpi = 600)


# Plot the percentage of data complete

ggplot(pivot_longer(weather_stations_90_complete, c("Percentage_Tmax", "Percentage_Tmin"),
                    names_to = "Var", values_to = "values"),
       aes(Var, values)) +
  geom_boxplot() +
  geom_jitter()


# Unfortunately, Libya and Egypt had no weather stations with at least 90% of data complete for Tmin and Tmax
# I will include weather stations showing above 70% of data complete in these countries plus Tunisia
# as primary source of data

libya_egypt_70 <- weather_stations_filtered[weather_stations_filtered$Percentage_Tmin < 90 &
                                              weather_stations_filtered$Percentage_Tmin >= 70 &
                                              weather_stations_filtered$CTRY %in% c("TS", "LY", "EG"), ]


# Filter the data for the weather stations showing 90% of data complete plus the weather stations in 
# Libya and Egypt

weather_data_90 <- weather_data[c(weather_stations_90_complete$STATION.NAME,
                                  libya_egypt_70$STATION.NAME)]


# Select the auxiliary list and data frame to be used as patching list

patching_df_20 <- weather_stations_filtered[-which(weather_stations_filtered$Percentage_Tmin < 20 &
                                                       weather_stations_filtered$Percentage_Tmax < 20), ]

# Patching list

patching_list_20 <- weather_data[patching_df_20$STATION.NAME]

# Summary: after this procedure weather_data_90 is the list to be patched, patching_df_20 the available weather
# stations to be used as patching, and the patching_list_20 is the list of auxiliary data frames
# clean the global environment to maintain only the needed elements
