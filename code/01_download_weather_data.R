library(chillR)

# This script is to download the weather data from the list of stations generated in '00_look_for_stations.R'

# First, load the file with the information on the weather stations

weather_stations <- read.csv("data/weather_stations.csv")

# Check the weather_stations data frame for duplicated weather stations

weather_stations[duplicated(weather_stations$STATION.NAME), ]

# There are 6 repeated weather stations. For now, we will retain them to later check which has the longest
# record. We will add an ID to identify them

repeated_WS <- paste0(weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"], "_2")

# Replace the stations name in the main data frame with the name plus the ID

weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"] <- repeated_WS

# Remove weird characters ("/") from the name of the weather stations for later purposes

weather_stations[["STATION.NAME"]] <- gsub("/", "-", weather_stations$STATION.NAME)



# Create a for loop to download the weather data

# Generate a list to save the outputs

weather_data <- list()

# Start the for loop. Please, note that according to your stations, you should change the vector in which
# 'i' will move.
#
# This is the proposed distribution of weather stations
# 
# Erica: from 1 to 131
# Hajar: from 132 to 264
# Theresa: from 265 to 397
# Alvaro: from 398 to 530
# Marius' computer in the office: from 531 to 663
# Me: from 664 to 792
#
# Please replace the vector 'xx : xx' with your numbers

for (i in 398 : 442) {
  
  # Download the data for the weather station 'i'. I will use suppressWarnings() because the function produces
  # a warning in case no data is found for the period of interest. Since we are aware of that, we can skip the
  # the warning and deal with the problem in a later step
  
  data <- suppressWarnings(handle_gsod_2("download_weather",
                                         location = as.character(weather_stations[i, "chillR_code"]),
                                         time_interval = c(1974, 2020),
                                         station_list = weather_stations,
                                         quiet = TRUE))
  
  # Convert the downloaded data into chillR format
  
  data <- handle_gsod(data)$weather
  
  # Check if the function downloaded data or produced NA for the weather station 'i' and fill the weather_data
  # list accordingly
  
  if(is.null(data)) weather_data[[i]] <- "No data" else weather_data[[i]] <- data
  
  # Name the object 'i' in the weather_data list as the name of the weather station
  
  names(weather_data)[i] <- weather_stations[i, "STATION.NAME"]
  
}

# Identify the weather stations having no data for the period of interest

WS_no_data <- which(weather_data == "No data")

# Remove these weather stations from the weather_data list

weather_data_filtered <- weather_data[-WS_no_data]

# Remove the empty elements

weather_data_filtered <- weather_data_filtered[!is.na(names(weather_data_filtered))]

# Use make_all_day_table to complete the missing days

weather_data_filtered <- lapply(weather_data_filtered, make_all_day_table)

# Save the downloaded data in the respective folder

save_temperature_scenarios(weather_data_filtered,
                           "data/historic_temps",
                           prefix = "downloaded_data")




