
# Figure 1 will show the historic observed chill

figure_1 <- chill_plot[["Past_obs_CP"]]

figure_1

# Save figure 1 with tmap save
tmap_save(figure_1, "figures/final_figures/figure_1.png", width = 17.6, height = 23.4 / 2, units = "cm", dpi = 600)


# Figure 2 will show the historic simulated chill (median 1975 - 2019) and the change 2019 - 1975

figure_2 <- tmap_arrange(chill_map_median, change_map)

figure_2

# Save figure 2 with tmap save
tmap_save(figure_2, "figures/final_figures/figure_2.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 3 will show the change in SWC for the RCP4.5 relative to the historic simulated median

figure_3 <- tmap_arrange(chill_cange_plot[["rcp45_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp45_2085_intermediate_CP"]])

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/figure_3.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 4 is similar to figure 3 but showing the change for the RCP8.5 scenario

figure_4 <- tmap_arrange(chill_cange_plot[["rcp85_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp85_2085_intermediate_CP"]])

figure_4

# Save the figure to the folder
tmap_save(figure_4, "figures/final_figures/figure_4.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)

