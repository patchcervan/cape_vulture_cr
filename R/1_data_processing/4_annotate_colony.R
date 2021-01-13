# 21-12-2020

# In this script we annotate bird tracks with information regarding
# distance to its focal colony, any other colonies and supplementary
# feeding sites.

# We must have ran the script "R/2_colony_finding/1_colony_finding.R" to
# create the colony database before running this script

rm(list = ls())

library(tidyverse)
library(sf)
library(lubridate)
library(Rcpp)


# Read in data ------------------------------------------------------------

# Read in colony data
colony_db <- read_csv("data/working/colony_db.csv") 

# Read in roost and colony data
colony_all <- read_csv("data/working/colony_data_join.csv")

# Load restaurant data
sfs <- read_csv("data/working/restaurants.csv")

# Function to make projection
source("R/functions/makeTmerProj.R")

# Function to calculate minimum distance
Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")



# Read in track ----------------------------------------------------------

# List files that at the keep folder
trkfiles <- list.files("data/working/bird_tracks/in_process/", pattern = "fine.rds")

for(i in 1:length(trkfiles)){
      
      # Read in original files from these birds
      trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trkfiles[i]))
      
      # Idenfify bird
      id_sel <- unique(trk$bird_id)
      
      # Find projection for data
      tmerproj <- makeTmerProj(st_as_sf(trk, coords = c("lon", "lat"), crs = 4326, remove = F))
      
      # Find colonies used by the bird, make spatial object, and nest by year
      colony <- colony_db %>% 
            filter(bird_id == id_sel) %>% 
            st_as_sf(coords = c("dens_lon", "dens_lat"), crs = 4326, remove = F) %>% 
            st_transform(tmerproj) %>% 
            nest(data = -year)
      
      years <- unique(colony$year)
      

      # for each year's colony find distance to tracking locations
      trk <- trk %>% 
            mutate(year = lubridate::year(datetime)) %>% 
            st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
            st_transform(tmerproj) %>% 
            nest(data = -year)
      
      trk <- trk %>%
            mutate(dist_col = map2(.$data, colony$data, ~as.numeric(st_distance(.x, .y))))
      
      
      # Find distance to any colony or roost ------------------------------------
      
      # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
      colony_all <- st_as_sf(colony_all, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
            st_transform(tmerproj)
      
      # Then, we need to remove any colony or roost that is less than 10km away from the central colony
      col_sel <- map(colony$data, ~st_distance(.x, colony_all)) %>% 
            # add distance from central colony (for each year)
            map(~mutate(colony_all,
                        dist_from_central = .x[1,])) %>% 
            # remove those that are closer than 10km
            map( ~filter(.x, as.numeric(dist_from_central) > 1e4))
      
      # For each year calculate distance between locations and any colony or roost
      # considering only those selected roosts and colonies that are further than 10km
      # from central colony
      trk <- trk %>% 
            mutate(dist_col_any = map2(.$data, col_sel,
                                       ~minDist_cpp(st_coordinates(.x),
                                                    st_coordinates(.y))))
      
      # Find distance to restaurants --------------------------------------------
      
      # To create a matrix of coordinates from the sfs layer, we need to create a spatial object
      sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
            st_transform(tmerproj)
      
      # Then, we need to remove any sfs that is less than 10km away from the central colony
      sfs_sel <- map(colony$data, ~st_distance(.x, sfs)) %>% 
            # add distance from central colony (for each year)
            map(~mutate(sfs,
                        dist_from_central = .x[1,])) %>% 
            # remove those that are closer than 10km
            map( ~filter(.x, as.numeric(dist_from_central) > 1e4))
      
      # For each year calculate distance between locations and any sfs
      # considering only those selected sfs that are further than 10km
      # from central colony
      trk <- trk %>% 
            mutate(dist_sfs = map2(.$data, sfs_sel,
                                   ~minDist_cpp(st_coordinates(.x),
                                                st_coordinates(.y))))
      
      # Unnest
      trk <- unnest(trk,
                    cols = -year)
      
      # Remove geometry and save
      trk %>% 
            dplyr::select(-geometry) %>% 
            saveRDS(paste0("data/working/bird_tracks/in_process/", id_sel, "_col.rds"))
      
}