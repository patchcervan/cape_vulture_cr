# 16-07-2020

# With this script we pre-process bird data to match the database.

rm(list = ls())

library(tidyverse)
library(lubridate)
library(readxl)



# Read in bird data templates and new data --------------------------------

# Read in bird data base template
bird_db <- read_rds("data/working/bird_db_template.rds")

# Read in bird track template
bird_trk <- read_rds("data/working/bird_trk_template.rds")

# Read in new data
dat_summary <- read_excel("data/raw/vp/VulPro2/Movebank/movebank_metadata.xlsx", sheet = 1)
dat_summary

for(i in 1:7){
    
    # Set bird id
    bird_id <- paste0("mb0", i)
    
    # Set tag id
    tag_id <- dat_summary$`Unit number`[i]
    ring_id <- dat_summary$SAFRING[i]
    
    new_trk <- read_csv(paste0("data/raw/vp/VulPro2/Movebank/", ring_id,".csv"))
    
    
    # Pre-processing ----------------------------------------------------------
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    # new_trk$Date <- if(i > 6) new_trk$`GPS_date_YYYY-MM-DD` else dmy(new_trk$`GPS_date_YYYY-MM-DD`)
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = tag_id,
               datetime = timestamp) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(`location-long`),
               lat = as.double(`location-lat`),
               x = NA, y = NA,      # These will be filled-in later
               alt = as.double(`height-above-ellipsoid`),
               heading = heading,
               spd_h = `ground-speed` * 3.6, # ASSUMING THAT SPEED IS IN 2D? Converting to km/h
               spd_v = NA, spd_3d = NA, 
               error_h = NA, 
               error_v = NA, 
               error_3d = NA) %>% 
        select(colnames(bird_trk))
    
    write_csv(new_trk, path = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.csv", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    
    # Fix name, age and sex
    names <- c(NA, NA, NA, NA, NA, NA, NA)
    
    age <- if_else(i %in% c(4,6), "ad",
                   if_else(i == 1, "subad",
                           "juv"))
    
    sex <- "unknown"
    
    
    new_db <- dat_summary %>% 
        filter(SAFRING == ring_id) %>% 
        mutate(
            # trasmitter id
            tag_id = tag_id,
            # tag model
            tag_type = "GPS",
            # Speed units
            spd_units = as.character("km/h"),
            # unique bird identifier - 2 first letters of provider, plus 2 numbers
            bird_id = as.character(bird_id),
            # ring id
            ring_id = ring_id,
            # capture date
            date_start = as.POSIXct(strftime(new_trk$datetime[1], format = "%m/%d/%y"), tz = "GMT", format = "%m/%d/%y"),
            # date of last location
            date_end = NA,
            # name of the bird
            name = names[i],
            
            # bird age when caught - factor with levels:juvenile, sub-adult, adult and unknown
            age = factor(age, levels = c("juv", "subad", "ad", "unknown")),
            # bird sex - factor with levels:male, female and unknown
            sex = factor(sex,levels = c("male", "female", "unknown")),
            
            # number of locations in raw data
            nloc_pre = as.double(nrow(new_trk)),
            # number of locations in processed data
            nloc_post = NA,
            # mean sampling rate (hours)
            avg_dt = NA,
            # standard deviation of sampling rate (hours)
            sd_dt = NA ) %>% 
        select(colnames(bird_db))
    
    write_csv(new_db, path = paste("data/working/pre_proc_data/db_", bird_id,"_pp.csv", sep = ""))
    
    
}
