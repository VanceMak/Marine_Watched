#initial package setup, install pacman package if not already present
#install.packages("pacman")
library(pacman)
pacman:: p_load(here, dplyr, tidyverse, sf, ggplot2, gganimate, gifski, 
                terra, tidyterra, ggnewscale, cowplot, extrafont, 
                elementalistm, ggtext, rstudioapi) #elementalistm may fail

run_lines <- function(x) {
  editor <- rstudioapi::getSourceEditorContext()
  start <- rstudioapi::document_position(min(x), 1)
  end <- rstudioapi::document_position(max(x) + 1, 1)
  range <- rstudioapi::document_range(start, end)
  rstudioapi::setSelectionRanges(range, editor$id)
  selection <- rstudioapi::primary_selection(
    rstudioapi::getSourceEditorContext())$text
  source(textConnection(selection))}

                #=======================================================
#=========================================================================================
#====================================== CHANGE THIS ========================================
#=========================================================================================
                #=======================================================


  #GPS TRACK OF BOAT TRIP
trip <- read.csv(here::here("raw_data","2023_09_22","waypoints.csv"), #change this line
                 row.names=NULL) %>% 
  st_as_sf(coords = c("X", "Y"), crs=4326, remove = FALSE) %>% #change this if needed
  mutate(order = row_number()) #%>%
  #summarize(do_union = FALSE) %>%
  #st_cast("LINESTRING") 

  #GPS POINTS OF SIGHTINGS
sightings <- read.csv(here::here("raw_data","2023_09_22","points.csv"), #change this line
                      row.names=NULL) %>%
  filter(!is.na(Latitude)) %>% 
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Latitude", "Longitude"), crs=4326, remove = FALSE) %>%
  mutate(species = as.factor(Group))

                  #=======================================================
#=========================================================================================
#======================================= RUN THIS ========================================
#=========================================================================================
                  #=======================================================

run_lines(58:156) 

                #=======================================================
#=========================================================================================
#================================ DON'T CHANGE THIS ======================================
#=========================================================================================
                #=======================================================

#read spatial files <=== do not change
cornwall <- st_read(here::here("spatial_files","cornwall","corn.shp"))
falbay <- st_read(here::here("spatial_files","fal bay poly","falmouth_bay.shp"))
falmouth <- st_read(here::here("spatial_files","fal poly","57583-polygon.shp"))
uk <- st_read(here::here("spatial_files","uk poly","uk.shp"))

bathy = rast(here::here("spatial_files","bathy","mtf5050010050.asc"))
bathy2 = rast(here::here("spatial_files","bathy","mtf5050010055.asc"))

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
  NULL

inset.cornwall <- ggplot() + 
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  geom_sf(data=cornwall, colour = "grey60", fill = "grey60") +
  geom_sf(data=falmouth, colour = "red", fill = "red") + 
  geom_path(data=trip, linewidth = 0.75, aes(x = X, y = Y, colour = "red")) + 
  NULL

inset.map <- ggdraw(inset.base) + 
  draw_plot({inset.cornwall +  coord_sf(xlim = c(-5.75, -4.7), 
                                        ylim = c(49.96116, 50.5),expand = FALSE)},
            x = 0.14, y = 0.07, width = 0.3, height = 0.3) +
  theme(panel.background = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank())

#plot <=== do not change
trip.map <- ggplot() + 
  geom_spatraster(data=bathy) +
  geom_spatraster(data=bathy2) +
  scale_fill_gradient(high = "#cedce3", low = "#0073ad",
                      breaks = c(0, -10, -50, -60), na.value = "#cedce3", guide = "none") +
  geom_sf(data=falbay, colour = "#cedce3", fill = "grey75",lwd=0.5) +  
  geom_sf(data=falmouth, colour = "grey60", fill = "grey60", lwd = 0.1) + 
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
  coord_sf(xlim = c(-5.15,-4.75), ylim = c(50.01, 50.203), expand = FALSE) +
  scale_y_continuous(breaks = seq(50.07, 50.19, by = 0.1)) +
  theme(panel.background = element_blank(), panel.ontop = TRUE,
        panel.grid.major = element_line(linetype = "solid", 
                                        colour = "#4d4d4d26", linewidth =0.075),
        panel.border = element_rect(colour="transparent", fill = "transparent"),
        legend.direction = "horizontal", 
        legend.margin = margin(t = 0, unit = "cm"),
        legend.title = element_blank(), 
        legend.text = element_blank(),
        legend.key.width = unit(1.2, "cm"),
        #legend.key = elementalist::element_rect_round(radius = unit(0.5, "snpc")),
        legend.key.height = unit(0.5, "cm"),
        legend.background = element_rect(colour="transparent", fill = "transparent"),
        legend.key = element_rect(colour="transparent", fill = "transparent"),
        legend.key.size = unit(10, 'cm'),
        axis.title = element_blank()) +
  annotate("rect", xmin = -4.875, xmax = -4.755, ymin = 50.107, ymax = 50.2, 
           alpha = 0.5, colour = "grey50", fill = "#0073ad01") +
  annotate("richtext", x = -4.9, y = 50.027, #family = "Karla", 
           label = "**Coordinate Reference System:** ESPG:7030 <br> **Datum:** 
           WGS:1984, **Coded By:** @VanceMak",
           color = "grey30", size = 3, hjust = 0, fill = "#FFFFFF50", label.color = NA) +
  annotate("richtext", x = -4.91, y = 50.035, label = "**START**" ,
           color = "white", size = 4, hjust = 0, fill = NA, label.color = NA) +
  annotate("richtext", x = -4.79, y = 50.035, label = "**END**" ,
           color = "white", size = 4, hjust = 0, fill = NA, label.color = NA) +
  NULL

final.map <- ggdraw(trip.map) + 
  draw_plot(inset.map, x = 0.61, y = 0.50, width = 0.5, height = 0.5)

#http://www.sthda.com/english/wiki/ggplot2-texts-add-text-annotations-to-a-graph-in-r-software

                    #=======================================================
#=========================================================================================
#====================================== FINAL MAPS ========================================
#=========================================================================================
                      #=======================================================

final.map #final map, this is a really big render and may fail a few times
trip.map #has everything except inset, should load better hopefully
