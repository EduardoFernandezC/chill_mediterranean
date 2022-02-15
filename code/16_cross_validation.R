library(rgdal)
library(tmap)
library(spatstat) # Used for the dirichlet tessellation function
library(maptools) # Used for conversion from SPDF to ppp
library(raster) # Used to clip out thiessen polygons
library(gstat) # Use gstat's idw routine
library(sp) # Used for the spsample function
library(tmaptools)
library(gridExtra)
library(reshape2)#to melt a data frame

# Needed for the set up of the tmin, tmax plane

library(fields)
library(metR)
library(colorRamps)
library(sf)
library(tidyverse)
library(cartography) #needed to include structures like stripes to maps
library(data.table)

# Read the shape of the Mediterranean region
mediterranean <- readOGR('~/Downloads/ref-countries-2020-01m/CNTR_RG_01M_2020_4326.shp/CNTR_RG_01M_2020_4326.shp')

# Define the limits for the region
med_extent <- extent(-12.0, 49.0, 24.0, 50.0)

# Crop the spatial object according to the limits set above
mediterranean <- crop(mediterranean, med_extent)


# Read the main data frame with all data

data <- read.csv("data/proj_plus_worldclim_plus_hist-summ.csv")

# Create a spatial object from the data frame with information on the weather stations

Porig <- SpatialPointsDataFrame(data[, c("Long", "Lat")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = data[, which(!(colnames(data) %in% c("Lat", "Long")))])

# Match the boundaries of both elements, the shapefile and the Porig object

Porig@bbox <- mediterranean@bbox


# Create an empty grid where n is the total number of cells

grd <- as.data.frame(spsample(mediterranean, "regular", n = 50000))

names(grd) <- c("Long", "Lat")

coordinates(grd) <- c("Long", "Lat")

gridded(grd) <- TRUE

fullgrid(grd) <- TRUE

proj4string(grd) <- proj4string(Porig)


# Save the scenario names for further use in the for loop

scenarios <- colnames(select(data, starts_with(c("Past", "rcp"))))

# Load Tmin and Tmax data from the WorldClim for Jan (still need to check if Dec or Feb is better)

min_temp_jan <- raster("~/Dropbox/Doctorado/Projects/Paper_projects/Leading/chill_south_america/data/world_clim/wc2-2/wc2.1_30s_tmin_01.tif")
max_temp_jan <- raster("~/Dropbox/Doctorado/Projects/Paper_projects/Leading/chill_south_america/data/world_clim/wc2-3/wc2.1_30s_tmax_01.tif")

# Set the outline of the Mediterranean region

bb <- extent(-12.3, 50.3, 23.9, 52.8)

# Extract the Mediterranean region from the world map

min_temp_jan <- crop(min_temp_jan, bb)
max_temp_jan <- crop(max_temp_jan, bb)

# Adjust resolution of temperature map to match the grid of our project

min_temp_jan_res <- resample(min_temp_jan, raster(grd))
max_temp_jan_res <- resample(max_temp_jan, raster(grd))


# Check for outliers in the minimum and maximum temps between the observed and worldclim data

data$is_outlier_tmin <- abs(data$min_temp_jan_70_00 - data$mean_Tmin_hist_jan_74_00) > 2
data$is_outlier_tmax <- abs(data$max_temp_jan_70_00 - data$mean_Tmax_hist_jan_74_00) > 2

# Remove the stations with huge difference (above 2 °C)

stations_clean <- data[!(data$is_outlier_tmin | data$is_outlier_tmax), ]

# Function to get the chill correction for a tmin and tmax entry. Works only for individual values,
# needs to be used in a loop / apply function

get_chill_correction <-  function(tmin, tmax, lookup = pred){
  if(is.na(tmin)){
    return(NA)
  } else if(is.na(tmax)){
    return(NA)
  } else{
    tmin_index <- which.min(abs(lookup$x - tmin))
    tmax_index <- which.min(abs(lookup$y - tmax))
    return(lookup$z[tmin_index, tmax_index])
  }
}


# Some elements used inside the cross-validation loop

# Number of elements resulting from splitting the data set
k <- 8

# Repetitions for the cross-validation procedure
repetitions <- 5


# Loop implementing the repeated cross-validation
for (rep in 1 : repetitions){
  
  # Split the data set in k groups
  split_df <- split(stations_clean, sample(1 : k, nrow(stations_clean), replace = T))
  
  # Create an empty list to save the outputs
  eval_list <- vector(mode = "list", length = k)
  
  
  # Loop for the cross-validation of repetition 'rep'
  for (i in 1 : k){
    
    # Combine all the data frames minus the one corresponding to the evaluation data set
    train_df <- as.data.frame(rbindlist(split_df[-i]))
    train_df <- SpatialPointsDataFrame(train_df[, c("Long", "Lat")],
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                       data = train_df[, which(!(colnames(train_df) %in% c("Long", "Lat")))])
    
    
    # Select the evaluation data frame
    eval_df_original <- split_df[[i]]
    eval_df_original <- SpatialPointsDataFrame(eval_df_original[, c("Long", "Lat")],
                                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                               data = eval_df_original[, which(!(colnames(eval_df_original) %in%
                                                                                   c("Long", "Lat")))])
    
    
    # Produce an interpolated layer from both temperature maps of all station locations
    
    f_min_temp_jan <- as.formula(min_temp_jan_70_00 ~ Long + Lat)
    f_max_temp_jan <- as.formula(max_temp_jan_70_00 ~ Long + Lat)
    
    # Set up the variogram
    
    # At this point I will try to use a different approach by using a function that fits automatically the
    # variogram based on the data. This may be helpful since there will be no need to set the nugget, psill, and
    # range manually
    
    var_smpl_min_temp_jan <- automap::autofitVariogram(f_min_temp_jan, train_df)
    var_smpl_max_temp_jan <- automap::autofitVariogram(f_max_temp_jan, train_df)
    
    # Use the outputs in the kriging function
    
    # Implement the kriging
    
    dat_krg_min_temp_jan <- krige(f_min_temp_jan, train_df, grd, var_smpl_min_temp_jan$var_model)
    dat_krg_max_temp_jan <- krige(f_max_temp_jan, train_df, grd, var_smpl_max_temp_jan$var_model)
    
    # Transform the kriged surface to a raster object and cut for the margins of the region
    
    r_krig_min <- raster(dat_krg_min_temp_jan)
    r_krig_max <- raster(dat_krg_max_temp_jan)
    
    r_m_min <- mask(r_krig_min, mediterranean)
    r_m_max <- mask(r_krig_max, mediterranean)
    
    # Calculate a quality map, where you can see the percent difference between kriged and original temperature
    temp_diff_min <- r_m_min - min_temp_jan_res
    temp_diff_max <- r_m_max - max_temp_jan_res
    
    
    # Here we implement the for loop for interpolating chill based on a 3D model for all scenarios
    for(scen in scenarios){
      
      # Krig tmin and tmax data on a plane
      krig <- Krig(x = as.matrix(as.data.frame(train_df)[, c("min_temp_jan_70_00", "max_temp_jan_70_00")]),
                   Y = as.data.frame(train_df)[scen])
      
      pred <- predictSurface(krig)
      
      # Adjust row and column name of object pred
      
      colnames(pred$z) <- pred$y
      rownames(pred$z) <- pred$x
      
      # Save then number of rows and cols
      no_row <- nrow(r_m_min)
      no_col <- ncol(r_m_min)
      
      # Transform kriged tmin and tmax to matrix
      
      mat_krig_tmin <- matrix(r_m_min, nrow = no_row, ncol = no_col, byrow = T)
      mat_krig_tmax <- matrix(r_m_max, nrow = no_row, ncol = no_col, byrow = T)
      mat_real_tmin <- matrix(min_temp_jan_res, nrow = no_row, ncol = no_col, byrow = T)
      mat_real_tmax <- matrix(max_temp_jan_res, nrow = no_row, ncol = no_col, byrow = T)
      
      # Transform the matrix to vector and bind tmin and tmax
      t_both <- cbind(as.vector(mat_krig_tmin), as.vector(mat_krig_tmax))
      t_both_real <- cbind(as.vector(mat_real_tmin), as.vector(mat_real_tmax))
      
      
      # Extract the model chill for real and kriged tmin and tmax
      
      model_krigged_temp <- sapply(1 : nrow(t_both), function(i) get_chill_correction(t_both[i, 1], t_both[i, 2]))
      
      model_real_temp <- sapply(1:nrow(t_both_real), function(i) get_chill_correction(t_both_real[i, 1], t_both_real[i, 2]))
      
      
      # Calculate the adjustment (so the chill, which so far was not capured by krigging)
      
      model_adjust <- model_real_temp - model_krigged_temp
      
      model_adjust <- matrix(model_adjust, nrow = no_row, ncol = no_col)
      
      # Exclude adjustments which would decrease the chill
      
      # How can we transform the matrix back to a grid?
      
      raster_model_adjust <- raster(model_adjust)
      raster_model_adjust <- setExtent(raster_model_adjust, bb)
      crs(raster_model_adjust) <- crs(r_m_min)
      
      
      # Do interpolation of chill
      # Define the krigging model for the chill
      
      f.1 <- as.formula(paste(scen, "~ Long + Lat"))
      
      # Compute the sample variogram; note that the f.1 trend model is one of the
      # parameters passed to variogram(). This tells the function to create the
      # variogram on the de-trended data.
      
      # Implement the kriging using the autofiting variogram
      
      var.smpl <- automap::autofitVariogram(f.1, train_df, cloud = FALSE)
      
      # Perform the krige interpolation (note the use of the variogram model
      # created in the earlier step)
      
      dat.krg.chil <- krige(f.1, train_df, grd, var.smpl$var_model)
      
      # Assign kriged data to the raster
      
      r_krig <- raster(dat.krg.chil)
      r.m.chill <- mask(r_krig, mediterranean)
      r.m.chill <- max(r.m.chill, 0)
      
      raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 
      
      # Adjust the chill portions, prevent that chill portions become lower than zero
      r <- max(r.m.chill + raster_model_adjust, 0)
      r.m <- mask(r, mediterranean)
      
      
      # Evaluation of the interpolation result
      
      # The step to follow generates NA values estimated by the model even for locations not excluded by
      # the rule of 2 C when comparing Tmin and Tmax from WorldClim and on-site data. The following lines
      # will attempt to address this issue (TBH: I am not sure if we should keep it as is or fix it).
      # I found a solution in stackoverflow (https://stackoverflow.com/questions/27562076/if-raster-value-na-search-and-extract-the-nearest-non-na-pixel)
      # that looks for the nearest value estimated by the model...
      
      # Generates a data frame only having the coordinates
      coords_eval_df <- data.frame(x = as.data.frame(eval_df_original)$Long,
                                   y = as.data.frame(eval_df_original)$Lat)
      
      #extract values from chill map
      model_estimates <- raster::extract(r.m, coords_eval_df, method = "bilinear")
      
      # Copy the original eval_df
      eval_df <- eval_df_original
      
      # Add the model estimates eval_df to compare them latter
      eval_df$model_value <- model_estimates
      
      #transform to data frame, only use columns of interest (name, country, scen, modelvalue)
      eval_df <- as.data.frame(eval_df[, c("STATION.NAME", "CTRY", scen, "model_value")])
      
      # Pivot longer for further compatibility
      eval_df <- pivot_longer(eval_df, all_of(scen), names_to = "scenario", values_to = "observed_value")
      
      # Generate a big dataframe with multiple scenarios
      if (scen == scenarios[1]) eval_df_all <- eval_df else eval_df_all <- bind_rows(eval_df_all, eval_df)
      
    } # End of loop scenarios
    
    #safe df to list, where all evaluation values are gathered
    eval_list[[i]] <- eval_df_all
    
  } # End of loop for repetition 'rep'
  
  #combine all observations to one data frame
  eval_df_all <- as.data.frame(rbindlist(eval_list))
  
  #calculate the resiudal (observation - prediction)
  eval_df_all$residual <- eval_df_all$observed_value - eval_df_all$model_value
  
  # Add a column for the repetition
  eval_df_all$repetition <- rep
  
  #if first repetition, then create new object to store the residuals, otherwise just add columns to it
  if(rep == 1) eval_df_final <- eval_df_all else eval_df_final <- bind_rows(eval_df_final, eval_df_all)
  
} # End of cross-validation loop


# Save the final data frame after running the loop with 5 repetitions for all the scenarios
write.csv(eval_df_final, "data/cross_validation_raw.csv", row.names = FALSE)

# Load the file from folder

eval_df_final <- read.csv("data/cross_validation_raw.csv")

# Use tidyverse to summarize the residuals by weather station and scenario (mean, median, and sd)
eval_df_final_summ <- eval_df_final %>% group_by(STATION.NAME, CTRY, Long, Lat, scenario) %>% 
  
  summarize(mean_res = mean(residual, na.rm = TRUE),
            median_res = median(residual, na.rm = TRUE),
            sd = sd(residual, na.rm = TRUE))


# The same as above but using only weather stations
eval_df_final_summ_WS <- eval_df_final %>% group_by(STATION.NAME, CTRY, Long, Lat) %>% 
  
  summarize(mean_res = mean(residual, na.rm = TRUE),
            median_res = median(residual, na.rm = TRUE),
            sd = sd(residual, na.rm = TRUE))

#get general stats of residuals
summary(eval_df_final_summ_WS)

# Make a hist to see extreme values
hist(eval_df_final_summ_WS$median_res)

# Check the location of extreme values
eval_df_final_summ_WS[order(eval_df_final_summ_WS$median_res), ]
eval_df_final_summ_WS[order(eval_df_final_summ_WS$median_res, decreasing = TRUE), ]



# Plot the residuals by weather station
eval_df_final_sp <-  SpatialPointsDataFrame(eval_df_final_summ_WS[, c("Long", "Lat")],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                            data = eval_df_final_summ_WS[, -(which(colnames(eval_df_final_summ_WS) %in% 
                                                                                     c("Long", "Lat")))])
# Identify the NAs from the cross validation procedure
NAs_cross_validation <- as.data.frame(eval_df_final_sp[which(is.na(eval_df_final_sp$median_res)), ])[["STATION.NAME"]]

# Increase the coverage of the plot to fit the legends inside
b <- bbox(Porig)
b[1, ] <- c(-11, 45)
b[2, ] <- c(15, 49)
b <- bbox(t(b))

chill_residual <- tm_shape(mediterranean, bbox = b) +
  tm_fill(col = 'grey10') +
  tm_shape(mediterranean, bbox = b) +
  tm_borders(col = 'grey40') +
  tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
  tm_shape(eval_df_final_sp) +
  tm_symbols(col = "median_res", size = 0.0001, legend.col.show = FALSE,
             legend.hist = TRUE, palette = get_brewer_pal("RdYlBu", n = 30), midpoint = 0,
             breaks = seq(-40, 30, 5), legend.hist.title = "     Histogram of residuals (CP)", legend.hist.z = 4) +
  tm_shape(eval_df_final_sp) +
  tm_bubbles(col = 'median_res', size = 'sd', palette = get_brewer_pal("RdYlBu", n = 30),
             midpoint = 0, style = "cont", breaks = seq(-40, 30, 10), legend.col.reverse = TRUE,
             title.size = "       SD residual (CP)", title.col = "   Median residual (CP)",
             legend.format = list(text.align = "center"), legend.col.z = 2,
             legend.size.z = 1, border.col = "grey10", legend.col.is.portrait = FALSE,
             sizes.legend = c(1, 3, 6, 9, 12, 15)) + 
  tm_shape(Porig[which(Porig$STATION.NAME %in% NAs_cross_validation), ]) +
  tm_symbols(size = 0.2, shape = 4, col = 'firebrick') +
  tm_add_legend(type = "symbol", labels = "Missing", col = "firebrick", shape = 4, size = 0.5, z = 3) +
  tm_compass(position = c(0.5, 0.03), text.size = 0.5) +
  tm_scale_bar(position = c(0.1, 0.01), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
  tm_layout(legend.outside = F,
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            legend.title.size = 0.7,
            legend.text.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.size = 0.5,
            bg.color = "black",
            attr.color = "white",
            legend.stack = "horizontal",
            legend.width = -0.94,
            legend.just = c("bottom", "center"))

chill_residual

tmap_save(chill_residual, 'figures/final_figures/figure_5C.png',
          width = 17.6, height = 23.4 / 2, units = "cm", dpi = 600)
  
