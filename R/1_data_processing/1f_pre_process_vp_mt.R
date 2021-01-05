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
dat_summary <- read_excel("data/raw/vp/VulPro2/MTI/MTI metadata.xlsx", sheet = 1)
dat_summary



for(i in 1:2){
    
    # The database is a bit messy for these birds so we need additional indexes
    id <- i + 2
    
    
    # Set bird id
    bird_id <- paste0("mt0", i)
    
    # Set tag id
    ring_id <- dat_summary$SAFRING[id]
    tag_id <- dat_summary$`Unit number`[id]
    
    
    new_trk <- if(tag_id == "#126459"){
        read_excel(paste0("data/raw/vp/VulPro2/MTI/Adelaide Master.xlsx"))
    } else {
        read_excel(paste0("data/raw/vp/VulPro2/MTI/", ring_id,".xlsx"))
        }
        
    
    
    # Pre-processing ----------------------------------------------------------
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    new_trk$Date <- ymd(new_trk$Date)
    new_trk$Time <- strftime(new_trk$Time, format="%H:%M:%S", tz = "GMT")
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = tag_id,
               datetime = as.POSIXct(paste(new_trk$Date, new_trk$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(`Longitude(E)`),
               lat = as.double(`Latitude(N)`),
               x = NA, y = NA,      # These will be filled-in later
               alt = as.double(`Altitude(m)`),
               heading = Course,
               spd_h = Speed, # ASSUMING THAT SPEED IS IN 2D?
               spd_v = NA, spd_3d = NA, 
               error_h = NA, 
               error_v = NA, 
               error_3d = NA) %>% 
        select(colnames(bird_trk))
    
    saveRDS(new_trk, file = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.rds", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    
    # Fix name, age and sex
    names <- c("Seizure", "Sir Adelaide")
    
    age <- c("juv", "subad")
    
    sex <- "unknown"
    
    
    new_db <- dat_summary %>% 
        filter(SAFRING == ring_id) %>% 
        mutate(
            # trasmitter id
            tag_id = tag_id,
            # tag model
            tag_type = "Argos/GPS(?)",
            # Speed units
            spd_units = as.character("km/h"),
            # unique bird identifier - 2 first letters of provider, plus 2 numbers
            bird_id = as.character(bird_id),
            # ring id
            ring_id = ring_id,
            # capture date
            date_start = date(new_trk$datetime[1]),
            # date of last location
            date_end = NA,
            # name of the bird
            name = names[i],
            
            # bird age when caught - factor with levels:juvenile, sub-adult, adult and unknown
            age = factor(age[i], levels = c("juv", "subad", "ad", "unknown")),
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
    
    saveRDS(new_db, file = paste("data/working/pre_proc_data/db_", bird_id,"_pp.rds", sep = ""))
    
    
}
