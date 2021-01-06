# 13-07-2020

# We then explore bird tracking data and eliminate non-sensical locations
# i.e locations with physically impossible travelling speeds
# Finally, birds are added to the data bases

# UTM proj4string (crs) we might want to use:
# WGS84 Latitude Longitude
# "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" (4326)
# UTM 34S:
# "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" (32734)
# UTM 35S:
# "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" (32735)


rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(rnaturalearth)
library(sf)


# Read in bird data base template
bird_db <- read_rds("data/working/bird_db_template.rds")


# Read in base map elements -----------------------------------------------

source("R/functions/load_basemap.R")

# Ignore warnings because these are auxiliary maps - precision is not important now.


# Load functions ----------------------------------------------------------

# Ignore warnings because these are auxiliary maps - precision is not important now.
source("R/functions/saveBirdToDB.R")

# Function to make UTM projection
source("R/functions/makeTmerProj.R")

# This function takes the bird data and removes points with speed greater than cut_speed.
# the cut speed is modified depending on the time between relocations - for longer times
# the cut speed tends to be smaller.
# The speed used is computed from the location data, not the one recorded by the tag.
# Some diagnostic plots are exported to png files that can be found at
# output/data_process

source("R/functions/removeSpeedOutliers.R")


# Process birds -----------------------------------------------------------

# If the loop is ran all birds are processed. If only one bird wants to be processed
# set i <- <desired bird number> and run the body of the loop.

for(i in c(1:2)){
    
    # Read in bird data templates and new data --------------------------------
    
    # Fill in bird id
    bird_id <- paste0("ew0", i)
    
    # Read in new pre-processed track
    new_trk <- readRDS(paste("data/working/pre_proc_data/trk_", bird_id, "_pp.rds", sep = ""))
    
    # Read in new pre-processed data base record
    new_db <- readRDS(paste("data/working/pre_proc_data/db_", bird_id, "_pp.rds", sep = ""))
    

    # Remove empty locations and repeated records -----------------------------
    
    # Remove empty or 0 coordinates as well as repeated time stamps
    new_trk <- new_trk %>%
        filter(!is.na(lat), !is.na(lon)) %>% 
        filter(lat != 0, lon != 0) %>% 
        distinct(datetime, .keep_all = T) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")))
    

    # Remove speed outliers ---------------------------------------------------
    
    # Plot geographical location
    print(
        ggplot() +
            geom_sf(data = sa_map) +
            geom_sf(data = utm, fill = NA) + 
            geom_text(data = utm, aes(x = lon, y = lat, label = ZONE)) +
            geom_point(data = new_trk, aes(x = lon, y = lat))
    )
    
    # Make spatial object
    new_trk <- st_as_sf(new_trk, coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE)
    
    # Project bird to UTM (make sure the crs code is correct for the bird)
    proj <- makeTmerProj(new_trk)
    
    # Make sf object and project data to UTM
    new_trk <- new_trk %>% 
        st_transform(crs = proj) %>% 
        mutate(x = as.double(st_coordinates(.)[,1]),
               y = as.double(st_coordinates(.)[,2]))
    
    # Remove outliers
    trk_proc <- removeSpeedOutliers(bird = new_trk, country_map = sa_map,
                                   cut_speed = 200,
                                   filepath = "output/diag_data_process/")
    
    
    # Fill in track template --------------------------------------------------
    
    # Fill in missing fields in pre-processed data base record
    new_db
    
    new_db <- new_db %>% 
        mutate(
            # date of last location
            date_end = date(new_trk %>% group_by(bird_id) %>% slice_tail() %>% pull(datetime)),
            # number of locations in processed data
            nloc_post = as.double(nrow(trk_proc)),
            # mean sampling rate (hours)
            avg_dt = as.double(mean(trk_proc$dt, na.rm = T)),
            # standard deviation of sampling rate (hours)
            sd_dt = as.double(sd(trk_proc$dt, na.rm = T)) ) %>% 
        select(colnames(bird_db))
    
    
    # Add bird to data base ---------------------------------------------------
    
    # If the bird id is present in the DB the record will not be overwritten
    # unless overwrite is set to TRUE
    saveBirdToDB(new_db, overwrite = T)
    
    # Save bird track ---------------------------------------------------------
    
    saveRDS(trk_proc, file = paste("data/working/bird_tracks/", bird_id, ".rds", sep = ""))
    
}
