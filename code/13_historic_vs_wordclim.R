library(chillR)
library(tidyverse)

# This script will compute the mean temperature for each stations based on the historic records to be
# compared with the values from the worldclim data

# Read historic temperatures. These temps are generated from actual historic records plus a patching procedure plus 
# an interpolating procedure as the latest option for filling the gaps...

weather <- load_temperature_scenarios('data/weather_data_fixed/', prefix = 'fixed_')

# Generate a big data frame to work with

weather <- bind_rows(weather, .id = "STATION.NAME")

# Estimate the monthly mean for the months of Jul and Aug as well as the sd

weather <- weather %>% filter(Year %in% c(1970 : 2000) & Month %in% c(12, 1, 2)) %>% 
  
  group_by(STATION.NAME, Month) %>% mutate(Tmean_2 = (Tmax + Tmin) / 2) %>% 
  
  summarize(mean_temp = mean(Tmean_2), sd_temp = sd(Tmean_2),
            mean_Tmax = mean(Tmax), sd_Tmax = sd(Tmax),
            mean_Tmin = mean(Tmin), sd_Tmin = sd(Tmin)) %>% 
  
  pivot_wider(names_from = Month, values_from = c(mean_temp, sd_temp, mean_Tmax, sd_Tmax,
                                                  mean_Tmin, sd_Tmin)) %>% 
  
  rename("mean_temp_hist_dec_74_00" = "mean_temp_12", "mean_temp_hist_jan_74_00" = "mean_temp_1", "mean_temp_hist_feb_74_00" = "mean_temp_2",
         "sd_temp_hist_dec_74_00" = "sd_temp_12", "sd_temp_hist_jan_74_00" = "sd_temp_1", "sd_temp_hist_feb_74_00" = "sd_temp_2",
         "mean_Tmax_hist_dec_74_00" = "mean_Tmax_12", "mean_Tmax_hist_jan_74_00" = "mean_Tmax_1", "mean_Tmax_hist_feb_74_00" = "mean_Tmax_2",
         "sd_Tmax_hist_dec_74_00" = "sd_Tmax_12", "sd_Tmax_hist_jan_74_00" = "sd_Tmax_1", "sd_Tmax_hist_feb_74_00" = "sd_Tmax_2",
         "mean_Tmin_hist_dec_74_00" = "mean_Tmin_12", "mean_Tmin_hist_jan_74_00" = "mean_Tmin_1", "mean_Tmin_hist_feb_74_00" = "mean_Tmin_2",
         "sd_Tmin_hist_dec_74_00" = "sd_Tmin_12", "sd_Tmin_hist_jan_74_00" = "sd_Tmin_1", "sd_Tmin_hist_feb_74_00" = "sd_Tmin_2")

# Load the data frame resulting from script 12_worldclim_data.R

data_all <- read.csv("data/projections_plus_worldclim.csv")

# Add the historic summarized data to the main data frame

proj_worldclim_hist <- left_join(data_all, weather)

# Save the resulting data frame to the repo

write.csv(proj_worldclim_hist, "data/proj_plus_worldclim_plus_hist-summ.csv", row.names = FALSE)


# Compare the data from the worldclim database and the summarized data from the historic records

temp_data <- proj_worldclim_hist %>% 
  
  select(STATION.NAME, contains(c("dec", "jan", "feb")) & starts_with(c("avg", "min", "max", "mean"))) %>% 
  
  pivot_longer(-STATION.NAME) %>% 
  
  mutate(panel = ifelse(str_detect(.data$name, "dec"), "December",
                        ifelse(str_detect(.data$name, "jan"), "January", "February")),
         var = ifelse(str_detect(.data$name, c("avg", "mean_temp")), "Mean",
                      ifelse(str_detect(.data$name, "min"), "Minimum", "Maximum")),
         database = ifelse(str_detect(.data$name, "70_00"), "WorldClim", "Station")) %>% 
  
  select(-name) %>% 
  
  pivot_wider(names_from = "database", values_from = "value")
  
# Plot the temperatures

ggplot(temp_data, aes(WorldClim, Station)) +
  geom_ribbon(aes(ymin = WorldClim - 2, ymax = WorldClim + 2),
              fill = "grey40", alpha = 0.8) +
  # geom_smooth(method = "lm") +
  geom_point(shape = 1) +
  geom_abline(slope = 1, intercept = 0) +
  facet_grid(factor(panel, levels = c("December", "January", "February")) ~
               factor(var, levels = c("Minimum", "Mean", "Maximum"))) +
  labs(x = "WorldClim database (°C)",
       y = "Weather station database (°C)") +
  theme_bw()

# Identify the stations outside the ribbon of 2 °C

temp_data$outliers <- abs(temp_data$WorldClim - temp_data$Station) > 2

# Plot with stations labelled

ggplot(temp_data, aes(WorldClim, Station)) +
  geom_ribbon(aes(ymin = WorldClim - 2, ymax = WorldClim + 2),
              fill = "grey40", alpha = 0.8) +
  # geom_smooth(method = "lm") +
  geom_point(shape = 1) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point(data = temp_data[which(temp_data$outliers), ], color = "red") +
  ggrepel::geom_label_repel(data = temp_data[which(temp_data$outliers), ],
                            aes(label = STATION.NAME), size = 3) +
  facet_grid(factor(panel, levels = c("December", "January", "February")) ~
               factor(var, levels = c("Minimum", "Mean", "Maximum"))) +
  labs(x = "WorldClim database (°C)",
       y = "Weather station database (°C)") +
  theme_bw()

# Save the plot

ggsave("figures/worldclim_vs_stations_data.png", height = 20, width = 18, units = "cm")



