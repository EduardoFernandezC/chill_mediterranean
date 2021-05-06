library(sp)
library(tmap)
library(raster)

# This script will extract the data from the wordclim database and will include this
# to the safe winter chill data frame

# Read the data from the repository

data_wide <- read.csv("data/safe_winter_metrics_CP_wide.csv")

# Create a spatial object from the data frame with information on the weather stations

Porig <- SpatialPointsDataFrame(data_wide[, c("Long","Lat")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = data_wide[, which(!(colnames(data_wide) %in% c("STATION.NAME", "CTRY",
                                                                                    "Lat", "Long")))])

# Read the files of avg temp per month from WorldClim. These files can be downloaded from the following
# link https://www.worldclim.org/data/worldclim21.html. This data is for the period 1970 - 2000...

# The following code will get the files downloaded. They are quite big, so do it only once by un-commenting the lines

#dir.create("data/worldclim/")

# Average temperature 30 seconds
#download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip",
#             "data/worldclim/wc2.1_30s_tavg.zip")

# Maximum temperature 30 seconds
#download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmax.zip",
#             "data/worldclim/wc2.1_30s_tmax.zip")

# Minimum temperature 30 seconds
#download.file("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tmin.zip",
#             "data/worldclim/wc2.1_30s_tmin.zip")

# The files have monthly temperature so this will load the temps for Dec, Jan, and Feb

avg_temp_dec <- raster('data/world_clim/wc2/wc2.1_30s_tavg_12.tif')
avg_temp_jan <- raster('data/world_clim/wc2/wc2.1_30s_tavg_01.tif')
avg_temp_feb <- raster('data/world_clim/wc2/wc2.1_30s_tavg_02.tif')

min_temp_dec <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_12.tif')
min_temp_jan <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_01.tif')
min_temp_feb <- raster('data/world_clim/wc2-2/wc2.1_30s_tmin_02.tif')

max_temp_dec <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_12.tif')
max_temp_jan <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_01.tif')
max_temp_feb <- raster('data/world_clim/wc2-3/wc2.1_30s_tmax_02.tif')


# Adjust bounding box for plotting

b <- bbox(avg_temp_dec)
b[1, ] <- c(-11, 47)
b[2, ] <- c(23, 50)
b <- bbox(t(b))

# Test plot

tm_shape(avg_temp_dec, bbox = b) + 
  tm_raster(breaks = seq(-15, 25, 5), style = "cont") +
  tm_shape(Porig) + 
  tm_dots(size = 0.1) +
  tm_grid() +
  tm_legend(legend.outside = TRUE)


# Extract average temp per month (period 1970 - 2000) from the wordclim files to the data frame of chill

data_wide$avg_temp_dec_70_00 <- raster::extract(avg_temp_dec, Porig)
data_wide$avg_temp_jan_70_00 <- raster::extract(avg_temp_jan, Porig)
data_wide$avg_temp_feb_70_00 <- raster::extract(avg_temp_feb, Porig)

data_wide$min_temp_dec_70_00 <- raster::extract(min_temp_dec, Porig)
data_wide$min_temp_jan_70_00 <- raster::extract(min_temp_jan, Porig)
data_wide$min_temp_feb_70_00 <- raster::extract(min_temp_feb, Porig)

data_wide$max_temp_dec_70_00 <- raster::extract(max_temp_dec, Porig)
data_wide$max_temp_jan_70_00 <- raster::extract(max_temp_jan, Porig)
data_wide$max_temp_feb_70_00 <- raster::extract(max_temp_feb, Porig)

# Some points are missing in the world clim data. These stations will be removed to enhance the 
# interpolation procedure

final_data <- na.omit(data_wide)

# Save the outputs to the repo

write.csv(final_data, "data/projections_plus_worldclim.csv", row.names = FALSE)
  
  