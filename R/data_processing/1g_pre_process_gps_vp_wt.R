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
dat_summary <- read_excel("data/raw/vp/VulPro2/Wildlife Tracking/Wildlife_tracking_metadata.xlsx", sheet = 1)
dat_summary



for(i in 1:22){

    # Set bird id
    bird_id <- if(i < 10) paste0("wt0", i) else paste0("wt", i)
    
    # Set tag id
    ring_id <- dat_summary$SAFRING[i]
    tag_id <- dat_summary$`Unit number`[i]
    
    
    new_trk <- if(i %in% c(2, 17, 18, 20:22)){
        read.csv(paste0("data/raw/vp/VulPro2/Wildlife Tracking/", ring_id,".csv"))
    } else if(i %in% c(1,3,19)){
        read_delim(paste0("data/raw/vp/VulPro2/Wildlife Tracking/", ring_id,".csv"), delim = ";")
    } else {
        read_delim(paste0("data/raw/vp/VulPro2/Wildlife Tracking/", ring_id,".csv"), delim = ";",
                   locale = locale(decimal_mark = ","))
    }
    
    
    
    # Pre-processing ----------------------------------------------------------
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    if(i %in% c(1,3)){
        new_trk$Date <- as.POSIXct(new_trk$Date, format="%m/%d/%Y", tz = "GMT")
        new_trk$Time <- strftime(new_trk$GMT, format = "%H:%M:%S", tz = "GMT")
    }else if(i %in% c(2, 17, 18, 20:22)){
        new_trk$Date <- as.POSIXct(new_trk$Date, format="%m/%d/%Y", tz = "GMT")
        new_trk$Time <- as.POSIXct(new_trk$GMT, format = "%H:%M:%S", tz = "GMT") %>% 
            strftime(format = "%H:%M:%S", tz = "GMT")
    }else if (i == 19){
        new_trk$Date <- as.POSIXct(new_trk$`Date GMT + 0 (MM/DD/YYYY)`, format="%m/%d/%Y %H:%M:%S", tz = "GMT")
        new_trk$Time <- strftime(new_trk$Date, format = "%H:%M:%S", tz = "GMT")
        new_trk$Date <- as.POSIXct(new_trk$Date, format="%m/%d/%Y", tz = "GMT")
    }else{
        new_trk$Date <- as.POSIXct(new_trk$`Date GMT + 2 (MM/DD/YYYY)`, format="%m/%d/%Y %H:%M:%S", tz = "Africa/Johannesburg")
        new_trk$Time <- strftime(new_trk$Date, format = "%H:%M:%S", tz = "Africa/Johannesburg")
        new_trk$Date <- as.POSIXct(new_trk$Date, format="%m/%d/%Y", tz = "Africa/Johannesburg")
    }
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = tag_id,
               datetime = as.POSIXct(paste(new_trk$Date, new_trk$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(`Longitude`),
               lat = as.double(`Latitude`),
               x = NA, y = NA,      # These will be filled-in later
               alt = as.double(if(i %in% c(1:3,17,18,20:22)) new_trk$`Altitude` else new_trk$`Altitude (m)`),
               heading = Direction,
               spd_h = as.double(if(i %in% c(1:3,17,18,20:22)) new_trk$`Speed` else new_trk$`Speed (km/h)`), # ASSUMING THAT SPEED IS IN 2D?
               spd_v = NA, spd_3d = NA, 
               error_h = NA, 
               error_v = NA, 
               error_3d = as.double(if(i %in% c(1:3,17,18,20:22)) new_trk$PDOP else NA),) %>% 
        select(colnames(bird_trk))
    
    write_csv(new_trk, path = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.csv", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    
    # Fix age and sex
    age <- if_else(bird_id %in% paste0("wt", c("04", "09", "15")), "subad",
                   if_else(bird_id %in% paste0("wt", c("01", "18", "19", "20", "21")), "juv",
                           "ad"))
    
    sex <- "unknown"
    
    
    new_db <- dat_summary %>% 
        filter(SAFRING == ring_id) %>% 
        mutate(
            # trasmitter id
            tag_id = tag_id,
            # tag model
            tag_type = "GSM/GPS(?)",
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
            name = NA,
            
            # bird age when caught - factor with levels:juvenile, sub-adult, adult
            age = factor(age, levels = c("juv", "subad", "ad")),
            # bird sex - factor with levels:male, female
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
