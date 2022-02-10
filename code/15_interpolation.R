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


# This file will interpolate chill accumulation based on a 3D-model fed with maximum and 
# minimum temperatures

# Read the shape of the Mediterranean region
mediterranean <- readOGR('~/Downloads/ref-countries-2020-01m/CNTR_RG_01M_2020_4326.shp/CNTR_RG_01M_2020_4326.shp')

# Define the limits for the region
med_extent <- extent(-15.0, 48.0, 21.0, 50.0)

# Crop the spatial object according to the limits set above
mediterranean <- crop(mediterranean, med_extent)

# Read the main data frame with all data
data <- read.csv("data/proj_plus_worldclim_plus_hist-summ.csv")

# Create a spatial object from the data frame with information on the weather stations

Porig <- SpatialPointsDataFrame(data[, c("Long", "Lat")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = data[, which(!(colnames(data) %in% c("STATION.NAME",
                                                                            "Lat", "Long")))])

# Match the boundaries of both elements, the shapefile and the Porig object

Porig@bbox <- mediterranean@bbox

# Generate a base plot with strips to represent the points where the interpolation is not great

# Read the world data
data("World")

# Add the lines
mediterranean_base <- cartography::hatchedLayer(x = World, mode = "sfc", pattern = "right2left", density = 10)

# Set the boundaries of the Mediterranean region
#mediterranean_region <- extent(-1300000, 4300000, 3300000, 6300000)

# Convert the mediterranean base data to spatial object
mediterranean_base <- as_Spatial(mediterranean_base)

# Crop the required area only
#mediterranean_base <- crop(mediterranean_base, mediterranean_region)

# Transform to map
mediterranean_base <- spTransform(mediterranean_base, CRSobj = crs(Porig))

# Dashes plot check
tm_shape(mediterranean_base,  bbox = st_bbox(extent(-14, 45.3, 25, 50))) +
  tm_lines(col = "grey50") +
  tm_shape(mediterranean) + 
  tm_borders(col = 'black') +
  tm_shape(Porig) + 
  tm_dots(size = 0.05) +
  tm_grid()


# Create an empty grid where n is the total number of cells

grd <- as.data.frame(spsample(mediterranean, "regular", n = 50000))

names(grd) <- c("Long", "Lat")

coordinates(grd) <- c("Long", "Lat")

gridded(grd) <- TRUE

fullgrid(grd) <- TRUE

proj4string(grd) <- proj4string(Porig)


# Save the scenario names for further use in the for loop

scenarios <- colnames(select(data, starts_with(c("Past", "rcp"))))

# Create good scenario names to use in the main title of the plot
scenarios_fixed <- c(Past_obs_CP = "Historic observed", Past_sim_1975 = "Historic simulated (1975)",
                     Past_sim_1980 = "Historic simulated (1980)", Past_sim_1985 = "Historic simulated (1985)",
                     Past_sim_1990 = "Historic simulated (1990)", Past_sim_1995 = "Historic simulated (1995)",
                     Past_sim_2000 = "Historic simulated (2000)", Past_sim_2005 = "Historic simulated (2005)",
                     Past_sim_2010 = "Historic simulated (2010)", Past_sim_2015 = "Historic simulated (2015)",
                     Past_sim_2019 = "Historic simulated (2019)", rcp45_2050_optimistic_CP = "RCP4.5 - 2050 optimistic",
                     rcp45_2050_intermediate_CP = "RCP4.5 - 2050 intermediate",
                     rcp45_2050_pessimistic_CP = "RCP4.5 - 2050 pessimistic",
                     rcp45_2085_optimistic_CP = "RCP4.5 - 2085 optimistic",
                     rcp45_2085_intermediate_CP = "RCP4.5 - 2085 intermediate",
                     rcp45_2085_pessimistic_CP = "RCP4.5 - 2085 pessimistic",
                     rcp85_2050_optimistic_CP = "RCP8.5 - 2050 optimistic",
                     rcp85_2050_intermediate_CP = "RCP8.5 - 2050 intermediate",
                     rcp85_2050_pessimistic_CP = "RCP8.5 - 2050 pessimistic",
                     rcp85_2085_optimistic_CP = "RCP8.5 - 2085 optimistic",
                     rcp85_2085_intermediate_CP = "RCP8.5 - 2085 intermediate",
                     rcp85_2085_pessimistic_CP = "RCP8.5 - 2085 pessimistic" )


# Load Tmin and Tmax data from the WorldClim for Jan

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

# Produce an interpolated layer from both temperature maps of all station locations

f_min_temp_jan <- as.formula(min_temp_jan_70_00 ~ Long + Lat)
f_max_temp_jan <- as.formula(max_temp_jan_70_00 ~ Long + Lat)

# Set up the variogram

#var_smpl_min_temp_jan <- variogram(f_min_temp_jan, Porig)
#var_smpl_max_temp_jan <- variogram(f_max_temp_jan, Porig)

# Fit the variogrma

# dat_fit_min_temp_jan <- fit.variogram(var_smpl_min_temp_jan, fit.ranges = FALSE,
#                                       fit.sills = FALSE, 
#                                       vgm(model = "Sph", range = 800, psil = 9, nugget = 1.5))

# dat_fit_max_temp_jan <- fit.variogram(var_smpl_max_temp_jan, fit.ranges = FALSE,
#                                       fit.sills = FALSE, 
#                                       vgm(model = "Sph", range = 700, psil = 5, nugget = 2.7))

# Check the variogram (re-adjust psil and nugget if needed)

# plot(var_smpl_min_temp_jan, dat_fit_min_temp_jan)
# plot(var_smpl_max_temp_jan, dat_fit_max_temp_jan)


# At this point I will try to use a different approach by using a function that fits automatically the
# variogram based on the data. This may be helpful since there will be no need to set the nugget, psill, and
# range manually

var_smpl_min_temp_jan <- automap::autofitVariogram(f_min_temp_jan, Porig)
var_smpl_max_temp_jan <- automap::autofitVariogram(f_max_temp_jan, Porig)

# Use the outputs in the kriging function

# Implement the kriging

dat_krg_min_temp_jan <- krige(f_min_temp_jan, Porig, grd, var_smpl_min_temp_jan$var_model)
dat_krg_max_temp_jan <- krige(f_max_temp_jan, Porig, grd, var_smpl_max_temp_jan$var_model)

# Transform the kriged surface to a raster object and cut for the margins of the region

r_krig_min <- raster(dat_krg_min_temp_jan)
r_krig_max <- raster(dat_krg_max_temp_jan)

r_m_min <- mask(r_krig_min, mediterranean)
r_m_max <- mask(r_krig_max, mediterranean)

# Calculate a quality map, where you can see the percent difference between kriged and original temperature
temp_diff_min <- r_m_min - min_temp_jan_res
temp_diff_max <- r_m_max - max_temp_jan_res

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

# Create an empty list used to store chill values

chill_list <- list() 

# Create a list to save the plots

chill_plot <- list()

# Set height and width (cm) for maps when maps are saved

height <- 17
width <- 19

# Create the respective directory

# dir.create("figures/interpolation/")
# dir.create("figures/interpolation/correction_plane/")
# dir.create("figures/interpolation/maps/")
# dir.create("figures/interpolation/maps/estimation/")
# dir.create("figures/interpolation/maps/correction/")
# dir.create("figures/interpolation/maps/change/")

# Here we implement the for loop for interpolating chill based on a 3D model (also saved as a figure)

for(scen in scenarios){
  
  # Krig tmin and tmax data on a plane
  k <- Krig(x = as.matrix(stations_clean[, c("min_temp_jan_70_00", "max_temp_jan_70_00")]),
            Y = stations_clean[scen])
  
  pred <- predictSurface(k)
  
  # error <- predictSurfaceSE(k)
  
  # Adjust row and column name of object pred
  
  colnames(pred$z) <- pred$y
  rownames(pred$z) <- pred$x
  
  #colnames(error$z) <- error$y
  #rownames(error$z) <- error$x
  
  # Melt to df
  
  melted <- melt(pred$z)
  
  #melted_error <- melt(error$z)
  
  colnames(melted) <- c("min_temp_jan_70_00", "max_temp_jan_70_00", "value")
  
  #colnames(melted_error)<-c("min_temp_jul","max_temp_jul","value")
  
  #plot the grid
  correction_plane <- ggplot(melted, aes(x = min_temp_jan_70_00,
                                         y = max_temp_jan_70_00,
                                         z = value)) +
    geom_contour_fill(bins = 100) +
    scale_fill_gradientn(colours = alpha(matlab.like(15)),
                         name = paste("Safe winter chill\n(Chill Portions)", sep = ''), trans = 'reverse') +
    geom_contour(col = "black")  +
    geom_point(data = stations_clean, aes(x = min_temp_jan_70_00,
                                          y = max_temp_jan_70_00,
                                          z = NULL),
               size = 0.7) +
    geom_text_contour(stroke = 0.2, size = 2) +
    #labs(title = paste("Scenario:", scen)) +
    ylab('Monthly maximum temperature in January (°C)') +
    xlab('Monthly minimum temperature in January (°C)') +
    theme_bw(base_size = 12) +
    theme(legend.title.align = 0.5,
          legend.position = c(0.875, 0.3),
          legend.background = element_blank())
  
  correction_plane
  
  # ggsave(plot = correction_plane,
  #        filename = paste0('figures/interpolation/correction_plane/', scen, 'supplementary.png'),
  #        height = 10, width = 15, units = 'cm', dpi = 600)
  
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
  
  # To see how many pixels are outside of the correction range
  
  #test <- t_both_real[!is.na(t_both_real[, 1]), ]
  #out_test <- sapply(1 : nrow(test), function (i) get_chill_correction(test[i, 1], test[i, 2]))
  #sum(is.na(out_test)) / length(out_test)
  
  
  # Extract the model chill for real and kriged tmin and tmax
  
  model_krigged_temp <- sapply(1 : nrow(t_both), function(i) get_chill_correction(t_both[i, 1], t_both[i, 2]))
  
  model_real_temp <- sapply(1:nrow(t_both_real), function(i) get_chill_correction(t_both_real[i, 1], t_both_real[i, 2]))
  
  #test_corr_df <- data.frame('min_temp_jul' = as.vector(mat_real_tmin),'max_temp_jul' = as.vector(mat_real_tmax))
  
  #see where the datapoints are in the correction plane
  #ggplot(melted,
  #       aes(x=min_temp_jul,y=max_temp_jul,z=value)) +
  #  geom_contour_fill(bins=100) +
  #  scale_fill_gradientn(colours=alpha(matlab.like(15)),
  #                       name="Safe Chill Units", trans = 'reverse') +
  #  geom_contour(col="black")  +
  #  geom_text_contour(stroke = 0.2) +
  #  geom_point(data=test_corr_df,
  #             aes(x=min_temp_jul,y=max_temp_jul,z=NULL),
  #             size=0.7, alpha = 0.2) +
  #  theme_bw(base_size=15)
  #--> many points outside the range of the correction plane
  
  # Calculate the adjustment (so the chill, which so far was not capured by krigging)
  # problem: model_real_temp contains many NA
  
  model_adjust <- model_real_temp - model_krigged_temp
  
  model_adjust <- matrix(model_adjust, nrow = no_row, ncol = no_col)
  #model_orig <- matrix(model_real_temp,nrow = no_row, ncol = no_col)
  #model_krig <- matrix(model_krigged_temp,nrow = no_row, ncol = no_col)
  
  # Exclude adjustments which would decrease the chill
  
  # How can we transform the matrix back to a grid?
  
  raster_model_adjust <- raster(model_adjust)
  raster_model_adjust <- setExtent(raster_model_adjust, bb)
  crs(raster_model_adjust) <- crs(r_m_min)
  
  #raster_model_orig <- raster(model_orig)
  #raster_model_orig <- setExtent(raster_model_orig, bb)
  #crs(raster_model_orig) <- crs(r.m_min)
  
  #raster_model_krig <- raster(model_krig)
  #raster_model_krig <- setExtent(raster_model_krig,bb)
  #crs(raster_model_krig) <- crs(r.m_min)
  
  
  # Do interpolation of chill
  # Define the krigging model for the chill
  
  f.1 <- as.formula(paste(scen, "~ Long + Lat"))
  
  # Compute the sample variogram; note that the f.1 trend model is one of the
  # parameters passed to variogram(). This tells the function to create the
  # variogram on the de-trended data.
  
  # var.smpl <- variogram(f.1, Porig, cloud = FALSE)
  
  # Compute the variogram model by passing the nugget, sill and range values
  # to fit.variogram() via the vgm() function.
  
  # dat.fit <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
  #                          vgm(psill = 100, model = "Sph", nugget = 130, range = 1000))
  
  # Check the variogram  
  
  # plot(var.smpl,dat.fit)
  
  # Implement the kriging using the autofiting variogram
  
  var.smpl <- automap::autofitVariogram(f.1, Porig, cloud = FALSE)
  
  # Perform the krige interpolation (note the use of the variogram model
  # created in the earlier step)
  
  dat.krg.chil <- krige(f.1, Porig, grd, var.smpl$var_model)
  
  # Assign kriged data to the raster
  
  r_krig <- raster(dat.krg.chil)
  r.m.chill <- mask(r_krig, mediterranean)
  r.m.chill <- max(r.m.chill, 0)
  
  raster_model_adjust <- setExtent(raster_model_adjust, extent(r.m.chill)) 
  #raster_model_krig <- setExtent(raster_model_krig, extent(r.m.chill))
  #raster_model_orig <-  setExtent(raster_model_orig, extent(r.m.chill))
  
  
  #adjust the chill portions, prevent that chill portions become lower than zero
  r <- max(r.m.chill + raster_model_adjust, 0)
  r.m <- mask(r, mediterranean)
  
  chill_list <- append(chill_list, r.m)
  
  
  f_name <- paste0('figures/interpolation/maps/estimation/adjusted_chill_', scen, '.png')
  
  chill_map <- tm_shape(mediterranean,  bbox = st_bbox(extent(-10, 45.3, 21, 49))) +
    tm_fill(col = 'grey10') +
    tm_shape(mediterranean_base) +
    tm_lines(col = 'grey35') +
    tm_shape(r.m) +
    tm_raster(palette = c("#AF0926", "#DF422F", "#F7864E", "#FDC273", "#FEEFA7", "#EFF9DB", "#C0E3EF", "#8ABEDA", "#084A92", "#0A3A70"),
              title = "        Safe Winter Chill",
              midpoint = 35,
              breaks = seq(0, 100, by = 30), style = "cont", legend.reverse = TRUE,
              legend.format = list(suffix = " CP", text.align = "center"), 
              legend.is.portrait = FALSE) +
    tm_shape(mediterranean) +
    tm_borders(col = 'grey40') +
    tm_shape(Porig) + 
    tm_symbols(size = 0.15, shape = 4, col = 'firebrick') +
    tm_graticules(lines = F, labels.col = "black") +
    tm_compass(position = c(0.035, 0.885), size = 1.75, text.size = 0.8) +
    tm_scale_bar(position = c(0.005, 0.81), bg.color = "transparent", breaks = c(0, 200, 400, 600),
                 text.size = 0.6,  color.dark = "grey20") +
    tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
    tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
    tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
              main.title.position = "center",
              main.title.size = 1.4,
              main.title.color = "black",
              legend.title.size = 0.85,
              legend.text.size = 0.75,
              legend.stack = "horizontal",
              legend.outside = F,
              legend.position = c(0, 0),
              legend.width = -1,
              legend.bg.color = "black",
              legend.frame = "white",
              outer.margins = c(0.01, 0.01, 0.01, 0.01),
              bg.color = "black",
              attr.color = "white",
              outer.bg.color = "white")
  
  chill_map
  
  # Add the map to the list
  chill_plot <- append(chill_plot, list(chill_map))
  
  #tmap_save(chill_map, filename = f_name, height = height, width = width, units = 'cm')  
  
  new_seq <- seq(-50, 90, by = 10)
  
  f_name <- paste0('figures/interpolation/maps/correction/chill_correction_', scen, '.png')
  
  chill_correction <- tm_shape(mediterranean_base, bbox = st_bbox(extent(-10.5, 45.3, 25.25, 50))) +
    tm_lines(col = "grey50") +
    tm_shape(raster_model_adjust) +
    tm_raster(palette = get_brewer_pal("RdBu", contrast = c(0, 0.75)),
              midpoint = 0,
              title = paste(scen, "\ncorrection of\nwinter chill (Chill Portions)", sep = ''),
              breaks = new_seq, style = "cont", legend.reverse = TRUE) +
    tm_shape(Porig) +
    tm_symbols(size = 0.2, shape = 4, col = 'black') +
    tm_shape(mediterranean) +
    tm_borders(col = 'black') +
    tm_graticules(lines = F) +
    tm_compass(position = c(0.02, 0.8)) +
    tm_scale_bar(position = c(0.0225, 0.005), bg.color = "grey95", width = 0.15) +
    tm_layout(legend.outside = T, outer.margins = c(0.001, 0.001, 0, 0.001))
  
  #chill_correction
  
  #tmap_save(chill_correction, filename = f_name,height = height, width = width, units = 'cm')  

} # End of loop to create interpolation maps

# Change names in the list to scenario names
names(chill_list) <- scenarios

# Set the names of the elements of the list
names(chill_plot) <- scenarios
 
# Compute the maps for change in chill accumulation

# Generate a baseline raster scenario based on the median across historic simulated scenarios

brick_raster <- brick(chill_list[2 : 11])

# Estimate the median across raster layers

median_raster_scen <- calc(brick_raster, median)

# Create a list to save the change plots
chill_cange_plot <- list()


# Loop for change 2017 to future scenarios

for(scen in scenarios[12 : 23]){
  
  # Create file name
  f_name <- paste0('figures/interpolation/maps/change/change_hist_sim_vs_', scen, '.png')
  
  #split scenario name because so long
  x <- strsplit(scen, split = '_')
  
  change_map <- tm_shape(mediterranean,  bbox = st_bbox(extent(-10, 45.3, 21, 49))) +
    tm_fill(col = 'grey10') +
    tm_shape(mediterranean_base) +
    tm_lines(col = 'grey35') +
    tm_shape(chill_list[[scen]] - median_raster_scen) +
    tm_raster(palette = c("#AF0926", "#DF422F", "#F7864E", "#FDC273", "#FEEFA7", "#EFF9DB", "#C0E3EF", "#8ABEDA", "#084A92", "#0A3A70"),
              title = "                 Safe Winter Chill",
              midpoint = 0,
              breaks = seq(-50, 40, by = 20), style = "cont", legend.reverse = TRUE,
              legend.format = list(suffix = " CP", text.align = "center"), 
              legend.is.portrait = FALSE) +
    tm_shape(mediterranean) +
    tm_borders(col = 'grey40') +
    tm_shape(Porig) + 
    tm_symbols(size = 0.15, shape = 4, col = 'firebrick') +
    tm_graticules(lines = F, labels.col = "black") +
    tm_compass(position = c(0.035, 0.885), size = 1.75, text.size = 0.8) +
    tm_scale_bar(position = c(0.005, 0.81), bg.color = "transparent", breaks = c(0, 200, 400, 600),
                 text.size = 0.6,  color.dark = "grey20") +
    tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
    tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
    tm_layout(main.title = paste0("      ", scenarios_fixed[[scen]]),
              main.title.position = "center",
              main.title.size = 1.4,
              main.title.color = "black",
              legend.title.size = 0.85,
              legend.text.size = 0.75,
              legend.stack = "horizontal",
              legend.outside = F,
              legend.position = c(0, 0),
              legend.width = -1,
              legend.bg.color = "black",
              legend.frame = "white",
              outer.margins = c(0.01, 0.01, 0.01, 0.01),
              bg.color = "black",
              attr.color = "white",
              outer.bg.color = "white")
  
  change_map
  
  chill_cange_plot <- append(chill_cange_plot, list(change_map))
  
  #tmap_save(change_map, filename = f_name,height = height, width = width, units = 'cm')  
  
}

# Set the names of the elements in chill_change_plot
names(chill_cange_plot) <- scenarios[12 : 23]

# Calculate change 1975 to 2019 (2019 minus 1975)

scen <- scenarios[2]

f_name <- paste0('figures/interpolation/maps/change/change_2017_', scen, '.png')

# Split scenario name because so long

x <- strsplit(scen, split = '_')

change_map <- tm_shape(mediterranean,  bbox = st_bbox(extent(-10, 45.3, 21, 49))) +
  tm_fill(col = 'grey10') +
  tm_shape(mediterranean_base) +
  tm_lines(col = 'grey35') +
  tm_shape(chill_list[["Past_sim_2019"]] - chill_list[["Past_sim_1975"]]) +
  tm_raster(palette = c("#AF0926", "#DF422F", "#F7864E", "#FDC273", "#FEEFA7", "#EFF9DB", "#C0E3EF", "#8ABEDA", "#084A92", "#0A3A70"),
            title = "           Safe Winter Chill",
            midpoint = 0,
            breaks = seq(-30, 30, by = 20), style = "cont", legend.reverse = TRUE,
            legend.format = list(suffix = " CP", text.align = "center"), 
            legend.is.portrait = FALSE) +
  tm_shape(mediterranean) +
  tm_borders(col = 'grey40') +
  tm_shape(Porig) + 
  tm_symbols(size = 0.15, shape = 4, col = 'firebrick') +
  tm_graticules(lines = F, labels.col = "black") +
  tm_compass(position = c(0.035, 0.885), size = 1.75, text.size = 0.8) +
  tm_scale_bar(position = c(0.005, 0.81), bg.color = "transparent", breaks = c(0, 200, 400, 600),
               text.size = 0.6,  color.dark = "grey20") +
  tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
  tm_layout(main.title = "     Chill change 2019 - 1975",
            main.title.position = "center",
            main.title.size = 1.4,
            main.title.color = "black",
            legend.title.size = 0.85,
            legend.text.size = 0.75,
            legend.stack = "horizontal",
            legend.outside = F,
            legend.position = c(0, 0),
            legend.width = -1,
            legend.bg.color = "black",
            legend.frame = "white",
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            bg.color = "black",
            attr.color = "white",
            outer.bg.color = "white")

change_map

#tmap_save(change_map, filename = f_name, height = height, width = width, units = 'cm')  


# Plot the historic median SWC levels based on simulated scenarios

chill_map_median <- tm_shape(mediterranean,  bbox = st_bbox(extent(-10, 45.3, 21, 49))) +
  tm_fill(col = 'grey10') +
  tm_shape(mediterranean_base) +
  tm_lines(col = 'grey35') +
  tm_shape(median_raster_scen) +
  tm_raster(palette = c("#AF0926", "#DF422F", "#F7864E", "#FDC273", "#FEEFA7", "#EFF9DB", "#C0E3EF", "#8ABEDA", "#084A92", "#0A3A70"),
            title = "        Safe Winter Chill",
            midpoint = 35,
            breaks = seq(0, 100, by = 30), style = "cont", legend.reverse = TRUE,
            legend.format = list(suffix = " CP", text.align = "center"), 
            legend.is.portrait = FALSE) +
  tm_shape(mediterranean) +
  tm_borders(col = 'grey40') +
  tm_shape(Porig) + 
  tm_symbols(size = 0.15, shape = 4, col = 'firebrick') +
  tm_graticules(lines = F, labels.col = "black") +
  tm_compass(position = c(0.035, 0.885), size = 1.75, text.size = 0.8) +
  tm_scale_bar(position = c(0.005, 0.81), bg.color = "transparent", breaks = c(0, 200, 400, 600),
               text.size = 0.6,  color.dark = "grey20") +
  tm_add_legend(type = "line", labels = "Excluded", col = "grey35", lwd = 3) +
  tm_add_legend(type = "symbol", labels = "  Weather station", shape = 4, size = 0.5, col = "firebrick") +
  tm_layout(main.title = "      Historic simulated (median 1975 - 2019)",
            main.title.position = "center",
            main.title.size = 1.4,
            main.title.color = "black",
            legend.title.size = 0.85,
            legend.text.size = 0.75,
            legend.stack = "horizontal",
            legend.outside = F,
            legend.position = c(0, 0),
            legend.width = -1,
            legend.bg.color = "black",
            legend.frame = "white",
            outer.margins = c(0.01, 0.01, 0.01, 0.01),
            bg.color = "black",
            attr.color = "white",
            outer.bg.color = "white")

chill_map_median






