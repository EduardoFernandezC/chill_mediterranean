names(historic_weather_scenarios) <- "COZZO SPADARO"


start_time <- Sys.time()

Start_JDay <- dormancyR::date_to_JDay("2018-11-01", format = "%Y-%m-%d")
End_JDay <- dormancyR::date_to_JDay("2019-03-01", format = "%Y-%m-%d")

hist_sim_chill <- list()

hist_sim_chill[[1]] <- tempResponse_daily_list(historic_weather_scenarios[[i]],
                                               latitude = weather_stations[weather_stations$STATION.NAME ==
                                                                             names(historic_weather_scenarios)[i], "Lat"],
                                               Start_JDay = Start_JDay,
                                               End_JDay = End_JDay)
dir.create("~/Documents/speed_test")

save_temperature_scenarios(hist_sim_chill[[1]],
                           "~/Documents/speed_test",
                           paste("chill_hist_scen", names(historic_weather_scenarios)[i],
                                 "ref_year", sep = "_"))

end_time <- Sys.time()

time_taken <- round(end_time - start_time, 2)






historic_weather_scenarios <- list()

for(i in 1 : 1){
  
  historic_temperature_scenarios <- temperature_scenario_from_records(weather = weather_data[[i]],
                                                                      year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                               2005, 2009, 2013, 2017))
  
  
  historic_weather_scenarios[[i]] <- temperature_generation(weather = weather_data[[i]],
                                                            year = c(1981, 1985, 1989, 1993, 1997, 2001,
                                                                     2005, 2009, 2013, 2017),
                                                            sim_years = c(2000, 2100),
                                                            temperature_scenario = historic_temperature_scenarios)
  
}


