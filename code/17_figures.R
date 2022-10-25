
# Figure 1 will show the historic observed chill

figure_1 <- chill_plot[["Past_obs_CP"]]

figure_1

# Save figure 1 with tmap save
tmap_save(figure_1, "figures/final_figures/figures_revised/figure_2_revised.pdf", width = 17.6, height = 23.4 / 2,
          units = "cm", dpi = 600)


# Figure 2 will show the historic simulated chill (median 1975 - 2019) and the change 2019 - 1975

figure_2 <- tmap_arrange(chill_map_median, change_map)

figure_2

# Save figure 2 with tmap save
tmap_save(figure_2, "figures/final_figures/figures_revised/figure_3_revised.pdf", width = 17.6, height = 23.4,
          units = "cm", dpi = 600)


# Figure 3 will show the change in SWC for the RCP4.5 relative to the historic simulated median

figure_3 <- tmap_arrange(chill_cange_plot[["rcp45_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp45_2085_intermediate_CP"]])

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/figures_revised/figure_4_revised.pdf", width = 17.6, height = 23.4,
          units = "cm", dpi = 600)


# Figure 4 is similar to figure 3 but showing the change for the RCP8.5 scenario

figure_4 <- tmap_arrange(chill_cange_plot[["rcp85_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp85_2085_intermediate_CP"]])

figure_4

# Save the figure to the folder
tmap_save(figure_4, "figures/final_figures/figures_revised/figure_5_revised.pdf", width = 17.6, height = 23.4,
          units = "cm", dpi = 600)


# Supplementary figures
# Figure S2 is RCP4.5 - 2050 pessimistic and RCP4.5 - 2085 pessimistic
figure_s2 <- tmap_arrange(chill_cange_plot[["rcp45_2050_pessimistic_CP"]],
                          chill_cange_plot[["rcp45_2085_pessimistic_CP"]])

figure_s2

tmap_save(figure_s2, "figures/final_figures/figure_s2.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S3 is RCP8.5 - 2050 pessimistic and RCP8.5 - 2085 pessimistic
figure_s3 <- tmap_arrange(chill_cange_plot[["rcp85_2050_pessimistic_CP"]],
                          chill_cange_plot[["rcp85_2085_pessimistic_CP"]])

figure_s3

tmap_save(figure_s3, "figures/final_figures/figure_s3.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S4 is RCP4.5 - 2050 optimistic and RCP4.5 - 2085 optimistic
figure_s4 <- tmap_arrange(chill_cange_plot[["rcp45_2050_optimistic_CP"]],
                          chill_cange_plot[["rcp45_2085_optimistic_CP"]])

figure_s4

tmap_save(figure_s4, "figures/final_figures/figure_s4.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure S5 is RCP8.5 - 2050 optimistic and RCP8.5 - 2085 optimistic
figure_s5 <- tmap_arrange(chill_cange_plot[["rcp85_2050_optimistic_CP"]],
                          chill_cange_plot[["rcp85_2085_optimistic_CP"]])

figure_s5

tmap_save(figure_s5, "figures/final_figures/figure_s5.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# New figure 1 (after revisions on Oct 2022) showing the weather stations we used. Figure created with tmap and other
# libraries

# Load the required libraries
library(tmap)

# Read the file containing the initial 387 weather stations. Remember that, for several reasons, the final number of stations
# reduced to 347
stations <- read.csv("data/table_s1.csv")

# Transform the data frame into a spatial object
stations_sp <- sp::SpatialPointsDataFrame(stations[, c("Long", "Lat")],
                                          proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                          data = stations[, -(which(colnames(stations) %in% 
                                                                      c("Long", "Lat")))])

# Read the shape of the Mediterranean region
mediterranean <- rgdal::readOGR('~/Downloads/ref-countries-2020-01m/CNTR_RG_01M_2020_4326.shp/CNTR_RG_01M_2020_4326.shp')

# Define the limits for the region
med_extent <- raster::extent(-12.0, 47, 25.0, 50.0)

# Crop the spatial object according to the limits set above
mediterranean <- terra::crop(mediterranean, med_extent)

# Replace point boundary extent with that of the Mediterranean to make sure the interpolation is done for the whole extent
# of the Mediterranean
stations_sp@bbox <- mediterranean@bbox

# Define Med extent to get elevation
elevation_med_extent <- terra::crop(mediterranean, raster::extent(-10, 45, 21, 49))

# Get the elevation model of the Mediterranean
elevation_med <- elevatr::get_elev_raster(elevation_med_extent, clip = "bbox", z = 6, neg_to_na = TRUE)

# Create the map
stations_map <- tm_shape(elevation_med, bbox = raster::extent(-10, 45, 20, 49)) +
  tm_raster(breaks = seq(0, 4500, 1500), style = "cont", title = "Elevation (m.a.s.l.)",
            legend.is.portrait = FALSE) +
  tm_shape(mediterranean, bbox = raster::extent(-10, 45, 20, 49)) +
  tm_borders(col = 'grey40', lwd = 0.8) +
  tm_graticules(lines = FALSE, labels.size = 0.6, labels.col = "black") +
  tm_shape(stations_sp) +
  tm_symbols(size = 0.1, shape = 4, col = 'steelblue4') +
  tm_compass(position = c(0.93, 0.05), text.size = 0.5) +
  tm_scale_bar(position = c(0.57, 0.055), bg.color = 'transparent', text.size = 0.5, color.dark = "grey20") +
  tm_add_legend(type = "symbol", labels = "Weather station",
                col = "steelblue4", shape = 4, size = 0.35, title = "") +
  tm_layout(legend.outside = FALSE,
            legend.position = c(0.05, 0.015),
            legend.bg.color = "transparent",
            outer.margins = c(0.02, 0.02, 0.02, 0.02),
            legend.title.size = 0.9,
            legend.text.size = 0.6,
            legend.width = -0.5,
            legend.stack = "horizontal",
            bg.color = "white",
            
            attr.color = "black")

stations_map

tmap_save(stations_map, 'figures/final_figures/figure_1_revised_white.pdf',
          height = 12.5, width = 15.6, units = 'cm', dpi = 600)

