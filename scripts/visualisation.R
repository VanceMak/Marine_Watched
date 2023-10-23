#initial package setup, install pacman package if not already present

#install.packages("pacman")
library(pacman)
pacman:: p_load(here, dplyr, tidyverse, sf, ggplot2, gganimate, gifski, terra, tidyterra)


#read data <===== change acccording to visualisation 
  #GPS TRACK OF BOAT TRIP
trip <- read.csv("raw_data/2023_09_22/waypoints.csv",row.names=NULL) %>%
  st_as_sf(coords = c("X", "Y"), crs=4326, remove = FALSE) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") 

  #GPS POINTS OF SIGHTINGS
sightings <- read.csv("raw_data/2023_09_22/points.csv",row.names=NULL) %>%
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Latitude", "Longitude"), crs=4326, remove = FALSE)


#read spatial files <=== do not change
cornwall <- st_read("spatial_files/cornwall/cornwall.shp")
falbay <- st_read("spatial_files/fal bay poly/falmouth_bay.shp")
falmouth <- st_read("spatial_files/fal poly/57583-polygon.shp")

bathy = rast("spatial_files/bathy/mtf5050010050.asc")
bathy2 = rast("spatial_files/bathy/mtf5050010055.asc")


#plot <=== do not change
plot <- ggplot() + 
  geom_spatraster(data=bathy) +
  geom_spatraster(data=bathy2) +
  scale_fill_gradient(high = "#cedce3", low = "#0073ad",
                      breaks = c(0, -10, -50, -60),guide="none") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(linetype = "solid", colour = "grey30",size=0.075),
        panel.ontop = TRUE,
        panel.border = element_rect(colour="transparent", fill = "transparent")) +
  geom_sf(data=falbay, colour = "#cedce3", fill = "grey75",lwd=0.5) +  
  geom_sf(data=falmouth, colour = "grey60", fill = "grey60") + 
  geom_sf(data=trip, size = 0.2, colour="red",alpha = 0.5) +
  geom_point(data=sightings, size = 3, aes (x = Longitude, y = Latitude,
                                            colour = Group)) +
  coord_sf(xlim = c(-5.15,-4.75), ylim = c(50.01, 50.2)) +
  scale_y_continuous(breaks = seq(50.07, 50.19, by = 0.1)) +
  NULL

plot

