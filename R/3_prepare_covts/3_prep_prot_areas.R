# 02-09-2020

# In this script we prepare the shapefile with protected areas

# All countries come in different shapefiles but birds fly between countries.
# It makes it easier to work with a single spatial object

library(tidyverse)
library(sf)
library(furrr)

rm(list = ls())

# Define path to files
filepath <- "data/working/WDPA_protected_areas/"

# List files
shpfiles <- list.files(filepath, pattern = ".shp")

# Create a data frame to store all the protected areas
pa <- vector("list", length = length(shpfiles))
             
pa <- paste0(filepath, shpfiles) %>% 
    future_map(~st_read(.x))

pa <- do.call("rbind", pa)

plot(st_geometry(pa))

# Save the result
st_write(pa, dsn = paste0(filepath, "prot_areas.shp"), driver = "ESRI Shapefile" )

# Dissolve in a single polygon (very slow)
pa <- pa %>% 
    mutate(prot_area = "yes") %>% 
    group_by(prot_area) %>% 
    summarize()


# Save the dissoved result (single polygon)
st_write(pa, dsn = paste0(filepath, "prot_areas_single.shp"), driver = "ESRI Shapefile" )
