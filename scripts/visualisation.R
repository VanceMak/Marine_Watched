#initial package setup, install pacman package if not already present

#install.packages("pacman")
library(pacman)
pacman:: p_load(here, dplyr, tidyverse, sf, ggplot2, gganimate, gifski, terra, tidyterra, ggnewscale, cowplot)

#read data <===== change according to visualization 
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
uk <- st_read(here::here("spatial_files","uk poly","LAD_MAY_2022_UK_BFE_V3.shp"))

bathy = rast(here::here("spatial_files","bathy","mtf5050010050.asc"))
bathy2 = rast(here::here("spatial_files","bathy","mtf5050010055.asc"))


#plot <=== do not change
trip.map <- ggplot() + 
  theme(panel.background = element_blank(), panel.ontop = TRUE,
        panel.grid.major = element_line(linetype = "solid", 
                                        colour = "#4d4d4d50", linewidth =0.075),
        panel.border = element_rect(colour="transparent", fill = "transparent"),
        legend.position = "none") +
  geom_spatraster(data=bathy) +
  geom_spatraster(data=bathy2) +
  scale_fill_gradient(high = "#cedce3", low = "#0073ad",
                      breaks = c(0, -10, -50, -60)) +
  geom_sf(data=falbay, colour = "#cedce3", fill = "grey75",lwd=0.5) +  
  geom_sf(data=falmouth, colour = "grey60", fill = "grey60") + 
  geom_path(data=trip, linewidth = 0.75, aes(x = X, y = Y, colour = order)) + 
  scale_colour_gradient2(high = "darkgreen", mid = "white", low = "magenta4", 
                         midpoint = max(trip$order)/2,
                         breaks = c(0, 124, 126, 250)) +
  ggnewscale::new_scale_colour() +  
  geom_point(data=sightings, alpha = 0.8, aes(x = Longitude, y = Latitude, 
                        colour = species, shape= species, size= species)) +
  scale_colour_manual(values = c("Bird" = "#E68613", "Cetacean" = "red", 
                      "Seal" = "sienna", "Fish" = "aquamarine")) +
  scale_shape_manual(values = c("Bird" = "\u25C4", "Cetacean" = "\u25B2", 
                                "Seal" = "\u25BC", "Fish" = "\u25BA")) +
  scale_size_manual(values = c("Bird" = 3, "Cetacean" = 5, 
                               "Seal" = 4, "Fish" = 4))+
  coord_sf(xlim = c(-5.15,-4.75), ylim = c(50.01, 50.2), expand = FALSE) +
  scale_y_continuous(breaks = seq(50.07, 50.19, by = 0.1)) +
  NULL

trip.map

#inset maps
inset.base <- ggplot() + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  geom_sf(data=uk, colour = "grey75", fill = "grey75") + 
  geom_sf(data=cornwall, colour = "grey60", fill = "grey60") +
  geom_sf(data=falmouth, colour = "red", fill = "red") +
  geom_rect(xmin = 131950.40, xmax = 206071.14, 
            ymin = 11179.37, ymax = 113556.30,
            fill = NA, colour = "black", linewidth = 0.6)+
  NULL

inset.base

inset.map <- ggdraw(inset.base) + 
  draw_plot({inset.base + coord_sf(xlim = c(131950.40, 206071.14),
                                   ylim = c(11179.37, 113556.30 ),
                                   expand = FALSE)})
  
  draw_plot({inset.base + coord_sf(xlim = c(131950.40, 206071.14), 
                                 ylim = c(11179.37, 113556.30 ), expand = FALSE)},
            x = 0, y = 0,
            width = 0.3, height = 0.3)

inset.map

inset.base + coord_sf(xlim = c(131950.40, 206071.14), ylim = c(11179.37, 113556.30 ), expand = FALSE)


