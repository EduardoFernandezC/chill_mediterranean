library(tidyverse)
library(patchwork)
library(ggrepel)

# This script is a preliminary code for developing some figures to send out to the partners 

# Load the data file from the folder

data <- read.csv("data/all_temp_response_data.csv")

# Load the data for the weather stations

weather_station <- read.csv("data/weather_stations.csv")

morocco <- weather_station[weather_station$CTRY == "MO" & 
                             weather_station$STATION.NAME %in% unique(data$Weather_station), ]

spain <- weather_station[weather_station$CTRY == "SP" & 
                           weather_station$STATION.NAME %in% unique(data$Weather_station), ]

# Maps

countries <- borders("world",
                     fill = "goldenrod", colour = "grey40",
                     size = 0.3)

# Morocco map

morocco_map <- ggplot() + countries + 
  geom_point(aes(Long, Lat), data = morocco[morocco$STATION.NAME %in% c("ANGADS", "MENARA", "BASSATINE",
                                                                        "MOULAY ALI CHERIF"), ],
             color = "black", size = 1) +
  geom_label_repel(data = weather_station[weather_station$STATION.NAME %in% c("ANGADS", "MENARA", "BASSATINE",
                                                                              "MOULAY ALI CHERIF"), ], 
                   aes(Long, Lat, label = STATION.NAME), size = 1.75, fontface = "bold", hjust = 0, nudge_y = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-14, -1),
              ylim = c(29, 37))

# Spain map

spain_map <- ggplot() + countries + 
  geom_point(aes(Long, Lat), data = spain[spain$STATION.NAME %in% c("ZARAGOZA AB", "MURCIA SAN JAVIER",
                                                                    "CACERES", "OVIEDO"), ],
             color = "black", size = 1) +
  geom_label_repel(data = weather_station[weather_station$STATION.NAME %in% c("ZARAGOZA AB", "MURCIA SAN JAVIER",
                                                                              "CACERES", "OVIEDO"), ], 
                   aes(Long, Lat, label = STATION.NAME), size = 1.75, fontface = "bold", hjust = 1, nudge_y = 0.5) +
  labs(x = "Longitude", y = "Latitude") +
  coord_equal(xlim = c(-11, 5),
              ylim = c(36, 44))


# Select key weather stations from the partner countries to get their feedback. These preliminary weather stations
# are: "ZARAGOZA AB", "MURCIA SAN JAVIER", "ROTA NAVAL AIR STATION", "GRANADA" in Spain and
# "ANGADS", "MENARA", "BASSATINE", "MOULAY ALI CHERIF" in Morocco

data_morocco <- data[data$Weather_station %in% c("ANGADS", "MENARA", "BASSATINE", "MOULAY ALI CHERIF"), ]

data_spain <- data[data$Weather_station %in% c("ZARAGOZA AB", "MURCIA SAN JAVIER", "OVIEDO", "CACERES"), ]

# Horizontal line

y_line_mo <- data_morocco %>% filter(Scenario == "Past_simulated") %>% group_by(Weather_station) %>% 
  
  summarize(y = median(Chill_Portions))

y_line_sp <- data_spain %>% filter(Scenario == "Past_simulated") %>% group_by(Weather_station) %>% 
  
  summarize(y = median(Chill_Portions))

# Plot for morocco

mo_past <- ggplot(filter(data_morocco, Scenario == "Past_simulated"),
       aes(as.character(Ref_year), Chill_Portions)) +
  geom_boxplot(outlier.shape = NA, size = 0.15, fill = "skyblue") +
  geom_hline(aes(yintercept = y), data = y_line_mo, linetype = "dashed", size = 0.5) +
  geom_point(data = filter(data_morocco, Scenario == "Past_observed"),
             aes(as.character(End_year), Chill_Portions), size = 0.25, color = "red") +
  scale_x_discrete(breaks = c(1975, 1980, 1985, 1990, 1995, 2000,
                              2005, 2010, 2015, 2019)) +
  labs(x = "Year", y = "Chill Portions") +
  facet_grid(factor(Weather_station, levels = c("ANGADS", "BASSATINE", "MOULAY ALI CHERIF", "MENARA")) ~ "Historic") +
  theme_bw() +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 60, size = 7, hjust = 1, vjust = 1))
   

mo_future <- ggplot(filter(data_morocco, !(Scenario %in% c("Past_simulated", "Past_observed"))),
       aes(Climate_model, Chill_Portions, fill = Climate_model)) +
  geom_boxplot(outlier.shape = NA, size = 0.25) +
  geom_hline(aes(yintercept = y), data = y_line_mo, linetype = "dashed", size = 0.5) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(fill = "Climate model") +
  facet_grid(factor(Weather_station, levels = c("ANGADS", "BASSATINE", "MOULAY ALI CHERIF", "MENARA")) ~ 
               Scenario, labeller = labeller(Scenario = c(rcp45_2050 = "RCP4.5 - 2050", 
                                                          rcp45_2085 = "RCP4.5 - 2085",
                                                          rcp85_2050 = "RCP8.5 - 2050",
                                                          rcp85_2085 = "RCP8.5 - 2085"))) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size = 8),
        legend.box.spacing = unit(0, "cm"))

(morocco_map & theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank())) / ((mo_past + mo_future) + plot_layout(guides = "collect",
                                    widths = c(1, 4)) &
  scale_y_continuous(limits = c(0, 90), expand = expansion(add = 0)) &
  theme(legend.position = "bottom")) + plot_layout(heights = c(1, 4))

ggsave("figures/boxplots_morocco_prel.png", width = 17.4, height = 22.4, units =  "cm",dpi = 600)



# Plot for spain

sp_past <- ggplot(filter(data_spain, Scenario == "Past_simulated"),
                  aes(as.character(Ref_year), Chill_Portions)) +
  geom_boxplot(outlier.shape = NA, size = 0.15, fill = "skyblue") +
  geom_hline(aes(yintercept = y), data = y_line_sp, linetype = "dashed", size = 0.5) +
  geom_point(data = filter(data_spain, Scenario == "Past_observed"),
             aes(as.character(End_year), Chill_Portions), size = 0.25, color = "red") +
  scale_x_discrete(breaks = c(1975, 1980, 1985, 1990, 1995, 2000,
                              2005, 2010, 2015, 2019)) +
  labs(x = "Year", y = "Chill Portions") +
  facet_grid(factor(Weather_station, levels = c("OVIEDO", "ZARAGOZA AB", "CACERES", "MURCIA SAN JAVIER")) ~ "Historic") +
  theme_bw() +
  theme(strip.text.y = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 60, size = 7, hjust = 1, vjust = 1))


sp_future <- ggplot(filter(data_spain, !(Scenario %in% c("Past_simulated", "Past_observed"))),
                    aes(Climate_model, Chill_Portions, fill = Climate_model)) +
  geom_boxplot(outlier.shape = NA, size = 0.25) +
  geom_hline(aes(yintercept = y), data = y_line_sp, linetype = "dashed", size = 0.5) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(fill = "Climate model") +
  facet_grid(factor(Weather_station, levels = c("OVIEDO", "ZARAGOZA AB", "CACERES", "MURCIA SAN JAVIER")) ~ 
               Scenario, labeller = labeller(Scenario = c(rcp45_2050 = "RCP4.5 - 2050", 
                                                          rcp45_2085 = "RCP4.5 - 2085",
                                                          rcp85_2050 = "RCP8.5 - 2050",
                                                          rcp85_2085 = "RCP8.5 - 2085"))) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size = 8),
        legend.box.spacing = unit(0, "cm"))

(spain_map & theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank())) / ((sp_past + sp_future) + plot_layout(guides = "collect",
                                                                                           widths = c(1, 4)) &
                                                         scale_y_continuous(limits = c(0, 100), expand = expansion(add = 0)) &
                                                         theme(legend.position = "bottom")) + plot_layout(heights = c(1, 4))

ggsave("figures/boxplots_spain_prel.png", width = 17.4, height = 22.4, units =  "cm",dpi = 600)

