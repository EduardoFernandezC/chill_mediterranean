
load_temperature_scenarios_b <- function(path, prefix){
  
  files <- list.files(path)
  
  files_no_prefix <- substr(files, nchar(prefix) + 2, nchar(files))
  
  check_position <- stringr::str_locate_all(files_no_prefix, "_")
  
  check_length <- unlist(lapply(check_position, function (x) x[, "start"][1]))
  
  
  weather_stations <- unique(substr(files_no_prefix, 1, check_length - 1))
  
  
  data_listed <- list()
  
  for (i in 1 : length(weather_stations)){
    
    files_to_read <- paste0(path, prefix, "_", weather_stations[i], "_", 1 : 10,
                            "_", c(seq(1975, 2015, 5), 2019), ".csv")
    
    data <- lapply(files_to_read, read.csv)
    
    names(data) <- c(seq(1975, 2015, 5), 2019)
    
    data_listed[[i]] <- data
    
    names(data_listed)[[i]] <- weather_stations[i]
    
  }
  
  return(data_listed)
}




