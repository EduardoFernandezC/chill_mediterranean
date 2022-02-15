
# Figure 1 will show the historic observed chill

figure_1 <- chill_plot[["Past_obs_CP"]]

figure_1

# Save figure 1 with tmap save
tmap_save(figure_1, "figures/final_figures/figure_1C.png", width = 17.6, height = 23.4 / 2, units = "cm", dpi = 600)


# Figure 2 will show the historic simulated chill (median 1975 - 2019) and the change 2019 - 1975

figure_2 <- tmap_arrange(chill_map_median, change_map)

figure_2

# Save figure 2 with tmap save
tmap_save(figure_2, "figures/final_figures/figure_2C.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 3 will show the change in SWC for the RCP4.5 relative to the historic simulated median

figure_3 <- tmap_arrange(chill_cange_plot[["rcp45_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp45_2085_intermediate_CP"]])

figure_3

# Save figure 3 with tmap save
tmap_save(figure_3, "figures/final_figures/figure_3C.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


# Figure 4 is similar to figure 3 but showing the change for the RCP8.5 scenario

figure_4 <- tmap_arrange(chill_cange_plot[["rcp85_2050_intermediate_CP"]],
                         chill_cange_plot[["rcp85_2085_intermediate_CP"]])

figure_4

# Save the figure to the folder
tmap_save(figure_4, "figures/final_figures/figure_4C.png", width = 17.6, height = 23.4, units = "cm", dpi = 600)


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

