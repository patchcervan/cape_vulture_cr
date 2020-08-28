# 16/07/2020

# This script is used to load the base map elements needed for processing
# the telemetry data

library(tidyverse)
library(sf)
library(rnaturalearth)


# Base map elements -------------------------------------------------------

# Download South Africa map with Lesotho and Swaziland
sa_map <- rbind(rnaturalearth::ne_states(country = "south africa", returnclass = "sf"),
                rnaturalearth::ne_states(country = "lesotho", returnclass = "sf"),
                rnaturalearth::ne_states(country = "swaziland", returnclass = "sf"))

# Remove the Prince Edward Islands
sa_map <- st_crop(sa_map, 
                  xmin = 15, xmax = 35,
                  ymin = -35, ymax = -20)

# Read in UTM grid
utm <- st_read("data/working/utmzone.shp")

# Crop UTM to South Africa
utm <- st_crop(utm, 
               xmin = 15, xmax = 35,
               ymin = -35, ymax = -20)

# Calculate polygon centroids
utm <- utm %>% 
    mutate(lon = st_coordinates(st_centroid(utm))[,1],
           lat = st_coordinates(st_centroid(utm))[,2])