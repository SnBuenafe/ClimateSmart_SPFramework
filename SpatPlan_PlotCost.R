# Example script to examine cost

# Jason Everett (UQ/UNSW/CSIRO) 
# Last Updated: 27 May 2021

library(sf)
library(tidyverse)
library(rnaturalearth)
library(raster)
source("fConvert2PacificRobinson.R")

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Best to define these first so you don't make mistakes below
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

world <- ne_countries(scale = "medium", returnclass = "sf")

## Do cost first
cost <- raster("/Users/jason/GitHub/SpatialPlanning/Data/Cost/Cost_Raster_Sum.grd") %>% 
  rasterToPolygons(dissolve = FALSE, na.rm = FALSE) %>% 
  st_as_sf()

cost_cat <- cost %>% 
  mutate(category = cut(layer, breaks=c(0, 1, 1e1, 1e2, 1e3, 1e4, 1e5, 609303)))

gg <- ggplot() + 
  geom_sf(data = world, fill = "grey30", colour = "red", size = 0.1) + 
  geom_sf(data = cost_cat, aes(fill = category, colour = category), size = 0.000001) + 
  viridis::scale_fill_viridis(discrete = TRUE) + 
  viridis::scale_colour_viridis(discrete = TRUE)
ggsave("CategoricalCost.pdf", plot = gg, width = 30, height = 15, units = "cm")


## Do catch
catch <- stack("/Users/jason/GitHub/SpatialPlanning/Data/Catch/Total_Catch_RasterBrick.grd")
catch <- mean(catch[[61:65]], na.rm = TRUE) %>% 
  rasterToPolygons(dissolve = FALSE, na.rm = FALSE) %>% 
  st_as_sf()

catch_cat <- catch %>% 
  mutate(category = cut(layer, breaks=c(0, 0.1, 1, 2, 10, 50, 200, 1e6)))

gg <- ggplot() + 
  geom_sf(data = world, fill = "grey30", colour = "red", size = 0.1) + 
  geom_sf(data = catch_cat, aes(fill = category), colour = NA, size = 0.0001) + 
  viridis::scale_fill_viridis(discrete = TRUE)
ggsave("CategoricalCatch.pdf", plot = gg, width = 30, height = 15, units = "cm")

