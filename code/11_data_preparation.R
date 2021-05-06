library(tidyverse)

# This script will prepare the data before the spatial interpolation

# Load the data from the repository

all_data <- read.csv("data/safe_winter_metrics_all.csv")

# Unfortunately, the following steps are optimized for wide but not long data frames. The next steps will
# attempt to create a wide df (only for chill accumulation in CP for now)

# Split the general data frame into past observed, simulated, and future scenarios

past_obs <- all_data %>% filter(Scenario %in% c("Past_observed"))

past_sim <- all_data %>% filter(Scenario %in% c("Past_simulated"))

future <- all_data %>% filter(!(Scenario %in% c("Past_observed", "Past_simulated")))


# In past observed, define only the relevant columns and rename when appropriate

past_obs <- past_obs[, c("STATION.NAME", "CTRY", "Lat", "Long", "SWC_CP")]

names(past_obs)[which(names(past_obs) == "SWC_CP")] <- "Past_obs_CP"


# In past simulated, define one column for each of the reference years

past_sim$Scenario <- paste(substr(past_sim$Scenario, 1, 8), past_sim$Ref_year, sep = "_")

past_sim <- past_sim[, c("STATION.NAME", "CTRY", "Lat", "Long", "Scenario", "SWC_CP")]

past_sim <- pivot_wider(past_sim, names_from = "Scenario", values_from = "SWC_CP")


# In future scenarios, summarize the climate models into optimistic (85th percentile),
# intermediate (50th), and pessimistic (15th) scenarios

future <- future %>% group_by(STATION.NAME, CTRY, Lat, Long, Scenario) %>% 
  
  summarize(optimistic_CP = quantile(SWC_CP, 0.85),
            intermediate_CP = quantile(SWC_CP, 0.5),
            pessimistic_CP = quantile(SWC_CP, 0.15)) %>% 
  
  pivot_longer(c(optimistic_CP, intermediate_CP, pessimistic_CP), values_to = "Value", names_to = "Type") %>% 
  
  mutate(temp_name = paste(.data$Scenario, .data$Type, sep = "_")) %>% 
  
  select(STATION.NAME, CTRY, Lat, Long, temp_name, Value) %>% 
  
  pivot_wider(names_from = temp_name, values_from = Value)



# Merge the dataframes

intermediate <- left_join(past_obs, past_sim)

data_wide <- left_join(intermediate, future)

# Save the outputs

write.csv(data_wide, "data/safe_winter_metrics_CP_wide.csv", row.names = FALSE)
