library(chillR)
library(tidyverse)

# This script is to generate a joint data frame containing all data generated in this study

# Load the observed chill

observed_chill <- load_temperature_scenarios("data/observed_chill/",
                                             prefix = "observed_chill")

# Remove the weather stations located in small islands since the climatewizard batabase had no data
# for them

observed_chill <- observed_chill[-which(names(observed_chill) %in% c("USTICA ISLAND", "LAMPEDUSA", "PANTELLERIA",
                                                                     "PONZA ISLAND", "MILOS", "SKIROS"))]

# Transform the list into a data frame

observed_chill_df <- bind_rows(observed_chill, .id = "Weather_station")

# Fix the issue with the "_2" in some weather stations

observed_chill_df$Weather_station <- str_replace(observed_chill_df$Weather_station, "_2", "b")

# Define the scenario

observed_chill_df["Scenario"] <- "Past_observed"


# Load the chill for historic scenarios

source("code/utilities/load_temp_scen_wrapper.R")

simulated_chill <- load_temperature_scenarios_b("data/chill_hist_scenarios/",
                                                prefix = "hist_scen_chill")

# Remove the weather stations located in small islands since the climatewizard batabase had no data
# for them

simulated_chill <- simulated_chill[-which(names(simulated_chill) %in% c("USTICA ISLAND", "LAMPEDUSA", "PANTELLERIA",
                                                                        "PONZA ISLAND", "MILOS", "SKIROS"))]

# Merge the list into a big data frame

simulated_chill_df <- bind_rows(lapply(simulated_chill, bind_rows, .id = "Ref_year"), .id = "Weather_station")

# Define the scenario

simulated_chill_df["Scenario"] <- "Past_simulated"

# Load the data for the future scenarios

# Define the rcps and time horizons

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

# Define an empty list to save the outputs of the for loop

future_chill <- list()

# Implement the for loop 

for (i in 1 : length(simulated_chill)){
  
  temp_list <- list()
  
  for(RCP in RCPs){
    for(Time in Times){
      
       temp <- load_temperature_scenarios("data/chill_fut_scenarios/",
                                          paste("future_chill_",
                                                names(simulated_chill)[i], "_",
                                                RCP, "_", Time, sep = ""))
       
       temp <- bind_rows(temp, .id = "Climate_model")
       
       temp_list[[paste0(RCP, "_", Time)]] <- temp
       
       }
  }
  
  future_chill[[names(simulated_chill)[i]]] <- bind_rows(temp_list, .id = "Scenario")
  
  rm(temp_list)
}

# Merge the list generated above into a data frame

future_chill_df <- bind_rows(future_chill, .id = "Weather_station")

# Merge all data into a single data frame

all_data <- bind_rows(observed_chill_df, simulated_chill_df, future_chill_df)

# Save the data frame

write.csv(all_data, "data/all_temp_response_data.csv", row.names = FALSE)


