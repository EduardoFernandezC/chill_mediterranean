library(chillR)
library(tidyverse)

# This scrip will produce the data to be used in the weather generator

# Load the data for the weather stations and the data generated with the nasapower patching

weather_stations <- read.csv("data/weather_stations.csv")

quality_check_nasa_patched <- read.csv("data/quality_check_nasa_patching.csv")

# Load the weather data now

weather_data <- load_temperature_scenarios("data/nasapower_patched/", prefix = "nasa_patched")

# Fill the remaining gaps trhough linear interpolation

weather_data_fixed <- lapply(weather_data, fix_weather)


# Save the outputs from the linear interpolation

# Weather data fixed

dir.create("data/weather_data_fixed")

save_temperature_scenarios(lapply(weather_data_fixed, function (x) x[["weather"]]),
                           "data/weather_data_fixed/", prefix = "fixed")

# Metrics

dir.create("data/metrics_fixed_data")

save_temperature_scenarios(lapply(weather_data_fixed, function (x) x[["QC"]]),
                           "data/metrics_fixed_data/", prefix = "QC")

