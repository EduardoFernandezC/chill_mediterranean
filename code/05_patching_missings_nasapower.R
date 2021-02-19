library(chillR)
library(tidyverse)
library(nasapower)

# Load the patched data and the quality patching data frame

weather_patched <- load_temperature_scenarios("data/historic_temps_patched/",
                                              prefix = "temps_patched")

weather_stations_patched <- read.csv("data/quality_check_patching.csv")


# Load all available weather stations information

weather_stations_all <- read.csv("data/weather_stations.csv")

# Check the weather_stations data frame for duplicated weather stations

weather_stations_all[duplicated(weather_stations_all$STATION.NAME), ]

# There are 6 repeated weather stations. For now, we will retain them to later check which has the longest
# record. We will add an ID to identify them

repeated_WS <- paste0(weather_stations_all[duplicated(weather_stations_all$STATION.NAME), "STATION.NAME"], "_2")

# Replace the stations name in the main data frame with the name plus the ID

weather_stations_all[duplicated(weather_stations_all$STATION.NAME), "STATION.NAME"] <- repeated_WS

# Remove weird characters ("/") from the name of the weather stations for later purposes

weather_stations_all[["STATION.NAME"]] <- gsub("/", "-", weather_stations_all$STATION.NAME)



# The patching procedure did not work quite well for all stations. None was fully patched and some stations still
# remain with lot of missing days.

# At this part, I will retain only the station having less than 100 missing days. This could work in the
# linear interpolation approach if these days are outside the chilling period or, are distributed across the whole
# period.

patched_stations_100 <- filter(weather_stations_patched,
                               Total_missing_Tmin <= 100 & Total_missing_Tmax <= 100)


# Select the weather stations with issues

problematic_stations <- weather_stations_patched[!(weather_stations_patched$Weather.station %in%
                                                     patched_stations_100$Weather.station), "Weather.station"]

# Plot the remaining weather stations

# Define the borders of the countries across the Mediterranean

countries <- borders("world",
                     fill = "goldenrod", colour = "grey40",
                     size = 0.3)

# Plot the stations in a map

ggplot() + countries + 
  geom_point(aes(Long, Lat),
             data = weather_stations_all[weather_stations_all$STATION.NAME %in%
                                            problematic_stations, ],
             color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))



# Since the first patching procedure did not work as expected, we will implement a second patching strategy with
# data from the nasapower database. It should be noted that the fit between the data recorded in-situ and the
# data downloaded from this data base for the same especific place is VERY BAD. However, we can estimate the mean
# bias and then adjust for it to fill in the remaining gaps. This should be better than linear interpolation...
# The problem is that missing days before 1981 will remain incomplete since the database starts after that year

# Download the nasapower data

# Get the list of weather stations required

weather_stations <- weather_stations_patched$Weather.station

# Create a list to save the outputs

nasapower_data <- list()

for (weather_station in weather_stations){
  
  # This part downloads the data
  nasapower_data[[weather_station]] <- get_power("AG",
                                                 pars = c("T2M_MAX", "T2M_MIN"),
                                                 temporal_average = "DAILY",
                                                 lonlat = c(weather_stations_all[weather_stations_all$STATION.NAME ==
                                                                                   weather_station, "Long"],
                                                            weather_stations_all[weather_stations_all$STATION.NAME ==
                                                                                   weather_station, "Lat"]),
                                                 dates = c("1981-01-01", "2020-12-31"))
}


# Convert the nasapower data to chillR format

nasapower_data_chillR <- lapply(nasapower_data, function (x) make_all_day_table(data.frame(Year = x$YEAR,
                                                                                           Month = x$MM,
                                                                                           Day = x$DD,
                                                                                           Tmin = x$T2M_MIN,
                                                                                           Tmax = x$T2M_MAX)))

# Save the nasapower data

dir.create("data/nasapower")

save_temperature_scenarios(nasapower_data_chillR, "data/nasapower/",
                           prefix = "nasapower_temp")


# Implement the patching procedure with data from the napower database. At this time, I will set the bias
# threshold to NULL to leave the function use all the information and adjust for that bias.
# The important part is to do it only with one auxiliary set corresponding to the specific location of the 
# weather station

weather_patched_nasa <- list()

for (weather_station in weather_stations){
  
  weather_patched_nasa[[weather_station]] <- patch_daily_temperatures(weather_patched[[weather_station]],
                                                                      nasapower_data_chillR[[weather_station]])
  
}

# Check how did it go

quality_patched_nasa <- lapply(weather_patched_nasa, function (x) dormancyR::perc_complete(x[["weather"]]))

# Transform the quality check list into a data frame

quality_patched_nasa_df <- bind_rows(quality_patched_nasa, .id = "Weather station")

# Get the relevant information in the quality check list to be passed to the weather stations data frame

quality_patched_nasa_df <- filter(quality_patched_nasa_df, Variable %in% c("Tmin", "Tmax"))

# Pivot wider to get the data in the right format to merge with the weather stations data frame

quality_patched_nasa_df <- pivot_wider(quality_patched_nasa_df, names_from = "Variable",
                                       values_from = c("Percentage", "Total_missing"))

# Add the mean and sd bias information

bias_and_sd <-  lapply(weather_patched_nasa, function (x){ c(Tmin_bias = x[["statistics"]][[1]][1, "mean_bias"],
                                                             Tmin_sd_bias = x[["statistics"]][[1]][1, "stdev_bias"],
                                                             Tmax_bias = x[["statistics"]][[1]][2, "mean_bias"],
                                                             Tmax_sd_bias = x[["statistics"]][[1]][2, "stdev_bias"])})
  
# Make a data frame out of the list

bias_and_sd_df <- bind_rows(bias_and_sd, .id = "Weather station")

# Merge the result to the quality patched nasa data frame

quality_patched_nasa_df <- left_join(quality_patched_nasa_df, bias_and_sd_df)

# Select the stations having less than 100 days missings in both Tmin and Tmax

patched_stations_100 <- filter(quality_patched_nasa_df, Total_missing_Tmin <= 108 &
                                 Total_missing_Tmax <= 108)


# Identify the problematic weather stations

problematic_stations <- quality_patched_nasa_df[!(quality_patched_nasa_df$`Weather station` %in%
                                                     patched_stations_100$`Weather station`), "Weather station"]

# Plot the remaining weather stations

# Define the borders of the countries across the Mediterranean

countries <- borders("world",
                     fill = "goldenrod", colour = "grey40",
                     size = 0.3)

# Plot the stations in a map

ggplot() + countries + 
  geom_point(aes(Long, Lat),
             data = weather_stations_all[weather_stations_all$STATION.NAME %in%
                                           patched_stations_100$`Weather station`, ],
             color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))

  
# Select all stations having more than 2 missing days for Tmin or Tmax to take a closer look (day by day)

stations_to_look_at <- filter(quality_patched_nasa_df, Total_missing_Tmin > 2 |
                                Total_missing_Tmax > 2)$`Weather station`

# After looking at the specific missing days, the stations to remove are "those". The remaining stations may
# make it through the linear interpolation since their missing days are not as much and not continuous during the 
# dormancy season

stations_to_remove <- stations_to_look_at[c(7, 16, 21, 24, 26, 27, 28, 29, 36, 51, 54, 58, 59, 60,
                                             61, 63, 64, 65, 67, 68, 69, 70, 71, 72)]

# Define the final interpolate-able weather stations.

stations_to_interpolate <- filter(quality_patched_nasa_df, !(`Weather station` %in% stations_to_remove))

stations_to_interpolate_data <- weather_patched_nasa[!(names(weather_patched_nasa) %in% stations_to_remove)]


# Visualize the stations to interpolate

ggplot() + countries + 
  geom_point(aes(Long, Lat),
             data = weather_stations_all[weather_stations_all$STATION.NAME %in% 
                                           stations_to_interpolate$`Weather station`, ],
             color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))

# Save the output

ggsave("figures/stations_to_interpolate_363.png", device = "png", dpi = 600)


# Save the outputs patched data and quality check df

write.csv(stations_to_interpolate, "data/quality_check_nasa_patching.csv", row.names = FALSE)

dir.create("data/nasapower_patched")

save_temperature_scenarios(lapply(stations_to_interpolate_data, function(x) x[["weather"]]),
                           "data/nasapower_patched/", prefix = "nasa_patched")
