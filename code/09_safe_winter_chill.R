library(tidyverse)
library(chillR)

# This script will compute safe winter chill for all possible scenarios

# Load the raw data from the repository

raw_data <- read.csv("data/all_temp_response_data.csv")

# This step will clean the data... Since the project is implemented for the Mediterranean region,
# the last season of the simulations (i.e. 2100/2101) only has data until 31 December 2100

raw_data_clean <- raw_data %>% filter(End_year != 2101)

# Compute safe winter metrics

safe_winter_metrics <- raw_data_clean %>% group_by(Weather_station, Scenario, Ref_year, Climate_model) %>% 
  
  summarize(SWC_CP = quantile(Chill_Portions, 0.1),
            SWC_CH = quantile(Chilling_Hours, 0.1),
            SWC_CU = quantile(Utah_Chill_Units, 0.1),
            SWH_GDH = quantile(GDH, 0.1))

# Read the information for the weather stations to be added to the safe metric's data frame

stations <- read.csv("data/weather_stations.csv")

# Check the weather_stations data frame for duplicated weather stations

stations[duplicated(stations$STATION.NAME), ]

# There are 6 repeated weather stations. For now, we will retain them to later check which has the longest
# record. We will add an ID to identify them

repeated_WS <- paste0(stations[duplicated(stations$STATION.NAME), "STATION.NAME"], "b")

# Replace the stations name in the main data frame with the name plus the ID

stations[duplicated(stations$STATION.NAME), "STATION.NAME"] <- repeated_WS

# Remove weird characters ("/") from the name of the weather stations for later purposes

stations[["STATION.NAME"]] <- gsub("/", "-", stations$STATION.NAME)

# Select the relevant columns

stations <- stations[, c("STATION.NAME", "CTRY", "Lat", "Long")]

# Nest the metrics data frame for ease the merging

safe_winter_metrics <- safe_winter_metrics %>% group_by(Weather_station) %>% nest()

# Join the data data frames 

all_data <- inner_join(stations, safe_winter_metrics, by = c("STATION.NAME" = "Weather_station"))

# Revert the nesting to get the data in the rigth format

all_data <- all_data %>% unnest(cols = c(data))

# Save the output to the repo

write.csv(all_data, "data/safe_winter_metrics_all.csv", row.names = FALSE)

