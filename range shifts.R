
rm(list=ls())

library(tidyverse)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)  # Optional: for scale bar, north arrow

data<-read_csv("processed_benthic_data_all_years.csv")
data

#How many species total in our data?
length(unique(data$species)) #1277

#sites -lat lon of records
#year -year visited
#species occurence


# count how many years each species was observed at each site
sdata<-data %>%
  group_by(siteID, species) %>%
  summarise(yearsobs = n(), .groups="drop") %>%
  filter(!is.na(yearsobs)) %>%
  filter(!is.na(species))
summary(sdata$yearsobs) #can't use siteIDs because they are unique each year!

#making my own siteIDs
usites<-data %>% mutate(latlon = paste(lat,lon)) %>% 
  select("latlon") %>% 
  unique()
summary(usites)
summary(data$siteID)
usitesIDs<-as.character(1:length(usites))
usites<-data_frame(usites,usitesIDs)
data<-data %>% mutate(latlon = paste(lat,lon))
data<-merge(data, usites, by="latlon", all.x = TRUE)
head(data)

# count how many years each species was observed at each usite
sdata<-data %>%
  group_by(usitesIDs, latlon, species) %>%
  summarise(yearsobs = n(), .groups="drop") %>%
  filter(!is.na(yearsobs)) %>%
  filter(!is.na(species))
summary(sdata$yearsobs) #can't use siteIDs because they are unique each year!
sdata[sdata$yearsobs>4,]

#replace all _'s with -'s in data for more consistency
data <- data %>%
  mutate(across(where(is.character), ~ gsub("_", "-", .)))
boop<-str_split(data$siteID, "-")
# removing the year code from the siteID code
for (i in 1:length(boop)){
  boop[[i]][1]<-stri_reverse(substring(stri_reverse(boop[[i]][1]),3))
}
boop<-unlist(lapply(boop, paste, collapse="-"))
data$siteIDnew<-boop
data %>% select(siteID, siteIDnew)

#count how many species observations came from each site each year
sdata<-data %>% group_by(siteIDnew, year) %>%
  summarise(totobs = n(), .groups="drop") %>%
  arrange(desc(totobs)) #ordered by most commonly observed to least 
sdata

ggplot(sdata, aes(x = siteIDnew, y = year, fill = totobs)) +
  geom_tile() +#color = "white") +
  scale_fill_gradient(low = "#1789FC", high = "#FF521B") +
  labs(
    title = "Number of Years Each Species Observed at Each Site",
    x = "SiteID",
    y = "Year",
    fill = "Species Observed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#*****************************************************************************
# based on this figure it doesn't look like sites were revisited often...
# need to go back to the data and the metadata to figure out each site is ID'd
# across years
#*****************************************************************************

#how common are the species across sites?
sdata<-data %>% group_by(species) %>%
  summarise(totobs = n(), .groups="drop") %>%
  filter(!is.na(species)) %>%
  arrange(desc(totobs)) #ordered by most commonly observed to least 
sdata
ggplot(data, aes(species)) +
  geom_histogram(stat="count")
#50 most common species?
top50<-sdata$species[1:50]

# count how many years each species was observed at each site
sdata<-data %>%
  group_by(siteIDnew, species) %>%
  summarise(yearsobs = n(), .groups="drop") %>%
  filter(!is.na(yearsobs)) %>%
  filter(!is.na(species))
summary(sdata$yearsobs)
sdata[sdata$yearsobs>4,] #should be none...yes

sdatatop50<-sdata %>% filter(species %in% top50)
#plot
ggplot(sdatatop50, aes(x = siteIDnew, y = species, fill = yearsobs)) +
  geom_tile() +#color = "white") +
  scale_fill_gradient(low = "#1789FC", high = "#FF521B") +
  labs(
    title = "Number of Years Each Species Observed at Each Site",
    x = "SiteID",
    y = "Species",
    fill = "Years Observed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#lets look at only the records for which a species was observed a seccond time 
#at least once at a given site
sdata<-sdata[sdata$yearsobs>=2,]

#plot
ggplot(sdata, aes(x = siteIDnew, y = species, fill = yearsobs)) +
  geom_tile() +#color = "white") +
  scale_fill_gradient(low = "#1789FC", high = "#FF521B") +
  labs(
    title = "Number of Years Each Species Observed at Each Site",
    x = "SiteID",
    y = "Species",
    fill = "Years Observed"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
  group_by(year, species) %>% 
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
  geom_sf(data = centroids, aes(color = species, alpha=((year-2000)/100)), size = 2) + 
  coord_sf(xlim = c(-85, -65), ylim = c(25, 47), expand = FALSE) +
  theme_minimal() +
  labs(title = "U.S. East Coast States",
       colour = "Species") +
  annotation_scale(location = "br", width_hint = 0.4) + 
  theme(legend.position = "none") +
  annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_fancy_orienteering)


