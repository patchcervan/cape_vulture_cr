# 13-07-2020

# With this script we first pre-process bird data to math the database.
# We then explore bird tracking data and eliminate non-sensical locations
# - Locations in the ocean
# - Locations with physically impossible travelling speeds
# Finally, birds are added to the data bases

# UTM proj4string (crs) we might want to use:
# WGS84 Latitude Longitude
# "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" (4326)
# UTM 34S:
# "+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" (32734)
# UTM 35S:
# "+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs" (32735)


rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)
library(rnaturalearth)
library(sf)

# If the loop is ran all birds are processed. If only one bird wants to be processed
# set i <- <desired bird number> and run the body of the loop.

for(i in c(1:2,4:9)){
    
# Read in bird data templates and new data --------------------------------

# Fill in bird id
bird_id <- paste("ez0", i, sep = "")

# Read in bird data base template
bird_db <- read_rds("data/working/bird_db_template.rds")

# Read in raw data summary
dat_summary <- read_excel("data/raw/Cape vulture data_Ezemvelo_Feb19_datecorrected.xls", sheet = "Summary")

# Read in new pre-processed track
new_trk <- read_csv(paste("data/working/pre_proc_data/trk_", bird_id, "_pp.csv", sep = ""))

# Read in new pre-processed data base record
new_db <- read_csv(paste("data/working/pre_proc_data/db_", bird_id, "_pp.csv", sep = ""))


# Read in base map elements -----------------------------------------------

source("R/functions/load_basemap.R")

# Ignore warnings because these are auxiliary maps - precision is not important now.


# Process SA land and speed -----------------------------------------------

# Plot geographical location
ggplot() +
    geom_sf(data = sa_map) +
    geom_sf(data = utm, fill = NA) + 
    geom_text(data = utm, aes(x = lon, y = lat, label = ZONE)) +
    geom_point(data = new_trk, aes(x = lon, y = lat))

source("R/functions/processLandSpeed.R")

# This function takes the bird data and removes points that fall ca.5km (0.5 degrees)
# outside of SA land as well as those locations with speed greater than cut_speed.
# The speed used is computed from the location data, not the one recorded by the tag.
# Some diagnostic plots are exported to a PDF file that can be found at
# output/data_process

trk_proc <- processLandSpeed(bird = new_trk, country_map = sa_map,
                                 proj = 32735, cut_speed = 200)



# Fill in track template --------------------------------------------------

# Fill in missing fields in pre-processed data base record
new_db

new_db <- new_db %>% 
    mutate(
        # date of last location
        date_end = as.POSIXct(as.character(date(trk_proc$datetime[nrow(trk_proc)]))),
        # number of locations in processed data
        nloc_post = as.double(nrow(trk_proc)),
        # mean sampling rate (hours)
        avg_dt = as.double(mean(trk_proc$dt, na.rm = T)),
        # standard deviation of sampling rate (hours)
        sd_dt = as.double(sd(trk_proc$dt, na.rm = T)) ) %>% 
    select(colnames(bird_db))


# Add bird to data base ---------------------------------------------------

source("R/functions/saveBirdToDB.R")

# If the bird id is present in the DB the record will not be overwritten
# unless overwrite is set to TRUE
saveBirdToDB(new_db, overwrite = F)

# Save bird track ---------------------------------------------------------

write_csv(trk_proc, path = paste("data/working/bird_tracks/", bird_id, ".csv", sep = ""))

}