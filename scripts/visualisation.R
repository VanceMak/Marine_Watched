#initial package setup, install pacman package if not already present

#install.packages("pacman")
library(pacman)
pacman:: p_load(here, dplyr, tidyverse, sf, ggplot2, gganimate, gifski, terra, tidyterra)

#read data <===== change acccording to visualisation 
  #GPS TRACK OF BOAT TRIP
trip <- read.csv(here::here("raw_data","2023_09_22","waypoints.csv"),row.names=NULL) %>%
  st_as_sf(coords = c("X", "Y"), crs=4326, remove = FALSE) %>%
  mutate(order = row_number()) #%>%
  #summarize(do_union = FALSE) %>%
  #st_cast("LINESTRING") 

  #GPS POINTS OF SIGHTINGS
sightings <- read.csv(here::here("raw_data","2023_09_22","points.csv"),row.names=NULL) %>%
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Latitude", "Longitude"), crs=4326, remove = FALSE) %>%
  mutate(species = as.factor(Group))


#read spatial files <=== do not change
cornwall <- st_read(here::here("spatial_files","cornwall","cornwall.shp"))
falbay <- st_read(here::here("spatial_files","fal bay poly","falmouth_bay.shp"))
falmouth <- st_read(here::here("spatial_files","fal poly","57583-polygon.shp"))

bathy = rast(here::here("spatial_files","bathy","mtf5050010050.asc"))
bathy2 = rast(here::here("spatial_files","bathy","mtf5050010055.asc"))


#plot <=== do not change
plot <- ggplot() + 
  theme(panel.background = element_blank(), panel.ontop = TRUE,
        panel.grid.major = element_line(linetype = "solid", 
                                        colour = "grey30", linewidth =0.075),
        panel.border = element_rect(colour="transparent", fill = "transparent"),
        legend.position = "none") +
  geom_spatraster(data=bathy) +
  geom_spatraster(data=bathy2) +
  scale_fill_gradient(high = "#cedce3", low = "#0073ad",
                      breaks = c(0, -10, -50, -60)) +
  geom_sf(data=falbay, colour = "#cedce3", fill = "grey75",lwd=0.5) +  
  geom_sf(data=falmouth, colour = "grey60", fill = "grey60") + 
  geom_path(data=trip, linewidth = 0.75, aes(x = X, y = Y, colour = order)) + 
  scale_colour_gradient2(high = "darkgreen", mid = "white", low = "darkgoldenrod1", 
                         midpoint = max(trip$order)/2) +
  ggnewscale::new_scale_colour() +  
  geom_point(data=sightings, aes(x = Longitude, y = Latitude, colour = species), 
             size = 3,alpha = 0.8) +
  ggnewscale::new_scale_colour() + 
  coord_sf(xlim = c(-5.15,-4.75), ylim = c(50.01, 50.2)) +
  scale_y_continuous(breaks = seq(50.07, 50.19, by = 0.1)) +
  NULL

plot
