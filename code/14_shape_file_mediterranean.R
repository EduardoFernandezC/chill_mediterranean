library(tmap)
library(sp)
library(raster)
library(rgdal)

# This script will generate the shape file for the Mediterranean region

# Read the main dataframe

data <- read.csv("data/proj_plus_worldclim_plus_hist-summ.csv")

# Create a spatial object from the data frame with information on the weather stations

Porig <- SpatialPointsDataFrame(data[, c("Long","Lat")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                data = data[, which(!(colnames(data) %in% c("STATION.NAME", "CTRY",
                                                                            "Lat", "Long")))])


# Generate a shape file for the Mediterranean region

# Read the world data from tmap
data("World")

# Define the limits for the region
med_extent <- extent(-1100000, 4300000, 3200000, 6300000)

# Convert to spatial
Med <- as_Spatial(World)

# Crop the spatial object according to the limits set above
Med <- crop(Med, med_extent)

# Transform the spatial object for maping
Med <- spTransform(Med, CRSobj = crs(Porig))

# Remove the area column to avoid problems when saving
Med$area <- NA

# Save the shape file
dir.create("data/Med_outline")

shapefile(Med, filename = "data/Med_outline/Med_outline.shp", overwrite = TRUE)

# Check whether this worked
Med <- readOGR('data/Med_outline/Med_outline.shp')

# Plot
tm_shape(Med, bbox = st_bbox(extent(-15, 45.3, 25.25, 50))) + 
  tm_borders(col = 'black')+
  tm_shape(Porig) + 
  tm_dots(size = 0.2)+
  tm_grid()


