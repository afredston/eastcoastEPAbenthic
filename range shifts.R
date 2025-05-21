
rm(list=ls())

library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)  # Optional: for scale bar, north arrow

data<-read_csv("processed_benthic_data_all_years.csv")
data

data<-st_as_sf(data, coords = c("lon", "lat"), crs = 4326) #converting to spatial point data


#Now trying to look at range shifts:
#note: to calculate centroids need to use an appropiate projection!
# e.g. reproject to a suitable CRS for the US/eastern US coast (NAD83 / 
# Conus Albers)
# Balances distortion across the lower 48 states, very good for area and 
# distance over wide regions like the East Coast.
#need to group by year and species
#bit slow... not bad
centroids<-data %>% 
  st_transform(crs=5070) %>% #transforming to projection to preserve distances
  group_by(year, full_name) %>% 
  summarise(geometry = st_centroid(st_union(geometry))) %>%
  st_transform(crs=4326) #back transforming to the map projection
centroids


#by default, when you use rnaturalearth and sf, the spatial data is in the WGS84 (same as our data :) but may want to re-project if want more accurate distances or areas, etc. for the region)
# Get world country boundaries
world <- ne_countries(scale = "medium", returnclass = "sf")
# Subset just the USA
usa <- subset(world, name == "United States of America")
# Define bounding box: min lon, min lat, max lon, max lat
# Covers roughly from Florida to Maine
east_coast_bbox <- st_bbox(c(xmin = -85, xmax = -65, ymin = 25, ymax = 47), crs = st_crs(usa))
# Crop the USA map to the East Coast bounding box
east_coast <- st_crop(usa, east_coast_bbox)
ggplot(data = east_coast) +
  geom_sf(fill = "#C7DABB", color = "#5AAB71") +
  coord_sf(xlim = c(-85, -65), ylim = c(25, 47), expand = FALSE) +
  theme_minimal() +
  labs(title = "East Coast of the United States") +
  annotation_scale(location = "br", width_hint = 0.4) + 
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


#if want states shown:
states <- ne_states(country = "United States of America", returnclass = "sf")
east_states <- st_crop(states, east_coast_bbox)
ggplot() +
  geom_sf(data = east_states, fill = "#C7DABB", color = "#5AAB71") +
  geom_sf(data = centroids, aes(color = full_name, alpha=((year-2000)/100)), size = 2) + 
  coord_sf(xlim = c(-85, -65), ylim = c(25, 47), expand = FALSE) +
  theme_minimal() +
  labs(title = "U.S. East Coast States",
       colour = "Species") +
  annotation_scale(location = "br", width_hint = 0.4) + 
  theme(legend.position = "none") +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)
