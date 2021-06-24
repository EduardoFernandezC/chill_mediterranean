library(chillR)
library(ggplot2)
library(tidyverse)

# This script will look for available weather stations near the Mediterranean area

# First define the location of interest. In this case I will first use a point in the middle of the Mediterranean sea
# latitude =  36.898146 and longitude =  16.987244

latitude <- 36.898146
longitude <- 16.987244

# List the 5,000 weather stations close to the point of interest. These weather stations are from the GSOD database

available_WS <- handle_gsod("list_stations", location = c(longitude, latitude),
                            stations_to_choose_from = 5000)


# Visualize the location of all available weather stations 

# Define the borders of the countries across the Mediterranean

countries <- borders("world",
                     fill = "goldenrod", colour = "grey40",
                     size = 0.3)


ggplot() + countries + 
  geom_point(aes(Long, Lat), data = available_WS, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude", title = "Available WS (5,000)") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))
  
ggsave("figures/available_WS_5000.png", height = 5.05, width = 6.5, dpi = 600)


# Filter the weather stations ending before 1970 and starting after 2000

available_WS_filtered <- filter(available_WS, BEGIN < 19740101 & END > 20201231)


# Include only those stations starting before 1st Jan 1980 and ending after 31st Dec 2017 (n = 346)

ggplot() + countries + 
  geom_point(aes(Long, Lat), data = available_WS_filtered, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude", title = "Filtered (1225; Begin < 1974 and End > 2020)") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))

ggsave("figures/available_WS_filtered_begin_end_v2.png", height = 5.05, width = 6.5, dpi = 600)


# Remove the weather stations located above 47.5 and below 25.0

available_WS_filtered_25_48 <- filter(available_WS_filtered, Lat > 25 & Lat < 48)

# Visualize the filtered stations

ggplot() + countries + 
  geom_point(aes(Long, Lat), data = available_WS_filtered_25_48, color = "black", size = 1) +
  labs(x = "Longitude", y = "Latitude", title = "Filtered (792; between 25° and 48°)") +
  coord_equal(xlim = c(-20, 50),
              ylim = c(15, 65))

ggsave("figures/available_WS_filtered_latitude_v2.png", height = 5.05, width = 6.5, dpi = 600)

# Save the final weather stations

write.csv(available_WS_filtered_25_48, "data/weather_stations.csv", row.names = FALSE)

