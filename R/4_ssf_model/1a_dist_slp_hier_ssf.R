# 18-10-2020

# In this script add the variable distance to steep slopes to the
# vulture data

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)
library(Rcpp)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- read_rds("data/working/data_ssf_ready.rds")

# To load slope rasters that intersect with the tracking data we need
source("R/functions/loadCovtsRasters.R")

# We will also need to reproject the tracks to lat-lon coordinates
source("R/functions/makeTmerProj.R")

# We will need the Rcpp function to calculate minimum distances
sourceCpp("R/functions/minDist_cpp.cpp")


# Create new variable to fill in ------------------------------------------

vults <- vults %>% 
      mutate(dist_slp = NA)

# Extract bird and create spatial object ----------------------------------

# Extract bird ids
bird_ids <- unique(vults$bird_id)

for(i in 1:length(bird_ids)){
      
      id_sel <- bird_ids[i]
      
      # For each bird create a spatial object
      trk <- vults %>% 
            filter(bird_id == id_sel) %>% 
            st_as_sf(coords = c("lon1_", "lat1_"), crs = 4326, remove = F)
      
      # Define projected coordinate system
      tmerproj <- makeTmerProj(trk)
      
      # We are interested in the end of the steps and the current coordinates
      # refer to the beginning of steps
      trk <- trk %>% 
            st_drop_geometry() %>% 
            st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = F) %>% 
            st_transform(crs = 4326)
      
      
      # Load raster and extract coordinates --------------------------------------
      
      # Load rasters      
      rr <- loadCovts(trk, covt = "slope")
      
      # Extract coordinates for all rasters
      cc <- map(rr, ~as.data.frame(.x, xy = T))
      
      rm(rr)
      
      # Filter those slopes that are greater than 0.3
      cc <- map(cc, ~rename(.x, layer = 3) %>% filter(layer > 0.3))
      
      # Bind in single object
      cc <- do.call("rbind", cc)
      
      # Make a spatial object
      cc <- st_as_sf(cc, coords = c("x", "y"), crs = 4326)
      
      # Project
      cc <- st_transform(cc, crs = tmerproj)
      
      # Extract coordinates
      cc <- st_coordinates(cc)
      
      
      # Calculate distances -----------------------------------------------------
      
      # Find closest distance to the trk points
      mindist <- minDist_cpp(as.matrix(st_drop_geometry(trk)[,c("x2_", "y2_")]), cc)
      
      # Fill in variable in original data frame
      vults$dist_slp[which(vults$bird_id == id_sel)] <- mindist
      
}

# Save data ---------------------------------------------------------------

write_rds(vults, "data/working/data_ssf_ready.rds")
