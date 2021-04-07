library(chillR)

# This script will implement a for loop for generating future temperatures for each weather station

# First, load the weather data into R

weather_data <- load_temperature_scenarios("data/weather_data_fixed/",
                                           prefix = "fixed")

# Load the data frame for all weather stations

weather_stations <- read.csv("data/weather_stations.csv")

# Check the weather_stations data frame for duplicated weather stations

weather_stations[duplicated(weather_stations$STATION.NAME), ]

# There are 6 repeated weather stations. For now, we will retain them to later check which has the longest
# record. We will add an ID to identify them

repeated_WS <- paste0(weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"], "b")

# Replace the stations name in the main data frame with the name plus the ID

weather_stations[duplicated(weather_stations$STATION.NAME), "STATION.NAME"] <- repeated_WS

# Remove weird characters ("/") from the name of the weather stations for later purposes

weather_stations[["STATION.NAME"]] <- gsub("/", "-", weather_stations$STATION.NAME)




# # Compute the observed responses for each weather station
 
Start_JDay <- dormancyR::date_to_JDay("2018-11-01", format = "%Y-%m-%d")
End_JDay <- dormancyR::date_to_JDay("2019-03-01", format = "%Y-%m-%d")


# Create a folder to save the outputs

dir.create("data/observed_chill")

# Define a list to save the outputs

chill_observed <- list()

for (i in 1 : length(weather_data)){


  chill_observed[[i]] <- tempResponse_daily_list(weather_data[[i]],
                                                 latitude = weather_stations[weather_stations$STATION.NAME ==
                                                                               names(weather_data)[i], "Lat"],
                                                 Start_JDay = Start_JDay,
                                                 End_JDay = End_JDay,
                                                 misstolerance = 10)[[1]]

  names(chill_observed)[i] <- names(weather_data)[i]
}

# Save the outputs in the respective folder

save_temperature_scenarios(chill_observed,
                           "data/observed_chill",
                           prefix = "observed_chill")




# Simulate historic temperature scenarios from records

historic_weather_scenarios <- list()

for(i in 1 : length(weather_data)){

  historic_temperature_scenarios <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                      year = c(1975, 1980, 1985, 1990, 1995, 2000,
                                                                               2005, 2010, 2015, 2019))


  historic_weather_scenarios[[i]] <- temperature_generation(weather = weather_data[[i]],
                                                            year = c(1975, 1980, 1985, 1990, 1995, 2000,
                                                                     2005, 2010, 2015, 2019),
                                                            sim_years = c(2000, 2100),
                                                            temperature_scenario = historic_temperature_scenarios)

  save_temperature_scenarios(historic_weather_scenarios[[i]], "data/temps_historic_scenarios/",
                             paste("past_simulated",
                                   weather_stations[weather_stations$STATION.NAME == names(weather_data)[i],
                                                    "STATION.NAME"], sep = "_"))
}


# This section is implemented to import the data generated for the historic scenarios
# First source a wrapper around load temperature scenarios that will load all data
# regardless of the weather station and prefix

source("code/utilities/load_temp_scen_wrapper.R")

# Run the wrapper to load the data

hist_temp_scenarios <- load_temperature_scenarios_b("data/temps_historic_scenarios/",
                                                    prefix = "past_simulated")

# Create a folder to save the outputs

dir.create("data/chill_hist_scenarios")

# Estimate the responses for the historic simulated scenarios

hist_sim_chill <- list()

for (i in 1 : length(hist_temp_scenarios)){

  hist_sim_chill[[i]] <- tempResponse_daily_list(hist_temp_scenarios[[i]],
                                                 latitude = weather_stations[weather_stations$STATION.NAME == names(hist_temp_scenarios)[i], "Lat"],
                                                 Start_JDay = Start_JDay,
                                                 End_JDay = End_JDay)

  save_temperature_scenarios(hist_sim_chill[[i]],
                             "data/chill_hist_scenarios/",
                             paste("hist_scen_chill",
                                  names(hist_temp_scenarios)[i],
                                   sep = "_"))
}

# Load a helper function to check if the scenarios are ok or not. This is because in some cases the 
# climatewizard function retrieves only zeros, which fall in the threshold range -5 to 10 and do still produce
# data without warnings... Sometimes, the function also retrieves values like 676833 (just and example) for
# weather stations located near the coast.

source("code/utilities/check_scenarios.R")

# Define the rcps and time horizons

RCPs <- c("rcp45", "rcp85")
Times <- c(2050, 2085)

# Create a folder to save the outputs (plese add your name to the data folder).
# For instance dir.create("data_erica/")

dir.create("data_{YOUR_NAME}/")

# Please put your stations in the following line. Remember removing the curly brackets
# These are the stations you have got:
# Erica: 1 to 35
# Theresa: 36 to 70
# Hajar: 71 to 105

for (i in 1 : {YOUR_STATIONS}){
  
  # Define weather baseline scenarios
  
  weather_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                             year = median(c(1974, 2019)))
  
  climate_wizard_baseline_scen <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                    year = median(c(1974, 2005)))
  
  baseline_adjustment <- temperature_scenario_baseline_adjustment(weather_baseline_scen,
                                                                  climate_wizard_baseline_scen)
  
  
  # Get the climate wizard data and simulate future observations based on RCP and year scenarios
  
  # Please note that if you get an error like this:
  # "Scenario bcc-csm1-1 seems erroneous. Please check it"
  # you will need to manually modify the latitude and/or longitude in the getclimatewizard call.
  # You can access a map, look for the weather station and select a near place in the continent.
  # This error may arise because the location provided is too close to the sea.
  
  ### Very important is to remove this modification for the other i's inside the
  ### for loop... So, this will probably stop the for loop in a given i.
  ### you run the loop manually with your modifications, save the data, then remove your modifications and
  ### finally, start the for loop again from the station you just finished. The way is:
  ### for (i in {here goes i + 1} : {the length of your vector}) (LINE 144)
  
  for (RCP in RCPs){
    for (Time in Times){
      
      start_year <- Time - 15
      end_year <- Time + 15
      
      longitude <- weather_stations[weather_stations$STATION.NAME == names(weather_data)[i], "Long"]
      
      latitude <- weather_stations[weather_stations$STATION.NAME == names(weather_data)[i], "Lat"]
      
      clim_scen <- getClimateWizardData(c(longitude = longitude, # Here you should manually modify the long... Please remove after
                                          latitude = latitude), # Here you should manually modify the lat... Please remove after
                                        RCP, start_year,
                                        end_year, temperature_generation_scenarios = TRUE,
                                        baseline = c(1974, 2005))
      
      clim_scen_checked <- lapply(clim_scen, check_scenarios)
      
      clim_scen_adj <- temperature_scenario_baseline_adjustment(baseline_adjustment,
                                                                clim_scen_checked,
                                                                temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
      
      
      temps <- temperature_generation(weather = weather_data[[i]],
                                      years = c(1974, 2019), 
                                      sim_years = c(2000, 2100),
                                      temperature_scenario = clim_scen_adj,
                                      temperature_check_args = list(scenario_check_thresholds = c(-7, 15)))
      
      # Please change the name of the folder to your respective folder
      save_temperature_scenarios(temps, "data_{YOUR_NAME}/",
                                 paste("future_temps_", names(weather_data)[i], "_", RCP, "_", Time, sep = ""))
      
    }
  }

}

# During the process of weather data generation, we discarded 6 weather stations located in small islands.
# For them, the climatewizard database failed to provide valid scenarios for future conditions.
# These stations were: 13, 21, 24, 33, 52, 56

# This step will remove these stations from the list of weather data for which the for loop is indexed

weather_data_final <- weather_data[-c(13, 21, 24, 33, 52, 56)]

# Create a folder to save the outputs

dir.create("data/chill_fut_scenarios")

# Compute climate-related metrics for future scenarios

for (i in 1 : length(weather_data_final)){
  for(RCP in RCPs){
    for(Time in Times){
      
      temps <- chillR::load_temperature_scenarios("data/temps_future_scenarios/",
                                                  paste("future_temps_", names(weather_data_final)[i], "_",
                                                        RCP, "_", Time, sep = ""))
      
      latitude <- weather_stations[weather_stations$STATION.NAME == names(weather_data)[i], "Lat"]
      
      chill <- chillR::tempResponse_daily_list(temps,
                                               latitude = latitude,
                                               Start_JDay = Start_JDay,
                                               End_JDay = End_JDay)
      
      chillR::save_temperature_scenarios(chill,
                                         "data/chill_fut_scenarios",
                                         paste("future_chill_", names(weather_data_final)[i], "_",
                                               RCP, "_", Time, sep = ""))
    }
  }
}





