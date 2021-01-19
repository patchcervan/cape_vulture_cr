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
dat_summary <- read_csv("data/raw/na/Summary of data.csv")
dat_summary

# Remove three last records because they are not Cape Vultures
dat_summary <- head(dat_summary, 9)

for(i in 1:9){
    
    # Set bird id
    bird_id <- paste0("na0", i)
    
    new_trk <- read_csv(paste0("data/raw/na/cv",i,".csv"))
    
    
    # Pre-processing ----------------------------------------------------------
    
    # Set tag id
    tag_id <- str_sub(dat_summary$`Bird - PTT`[i], - 5, -1)
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    if(i %in% c(1)){
        new_trk <- new_trk %>% 
            mutate(Date = mdy(new_trk$DATE))
        
    } else {
        new_trk$Date<- mdy(new_trk$Date)
    }
    
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = tag_id,
               datetime = Date + hms(new_trk$`GMT Time`)) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(Longitude),
               lat = as.double(Latitude),
               alt = as.double(Altitude),
               heading = as.double(Course),
               spd_h = Speed, # ASSUMING THAT SPEED IS IN 2D? Also for tag 87988 data said to multiply by 1852
               spd_v = NA, spd_3d = NA, error_h = NA, error_v = NA, 
               error_3d = NA) %>% 
        select(colnames(bird_trk))
    
    saveRDS(new_trk, file = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.rds", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    
    # Fix age and sex
    age <- if_else(bird_id %in% paste0("na0", c(1,2,3,4,5)), "ad",
                   if_else(bird_id %in% paste0("na0", 6), "juv",
                           "unknown"))
    
    sex <- if_else(bird_id %in% paste0("na0", 1:5), "male",
                   "unknown")
    
    
    new_db <- dat_summary %>% 
        filter(paste0("na0",str_sub(dat_summary$`Bird - PTT`, 3, 3)) == bird_id) %>% 
        mutate(
            # trasmitter id
            tag_id = tag_id,
            # tag model
            tag_type = "GMS/GPS(?)",
            # Speed units
            spd_units = as.character("km/h"),
            # unique bird identifier - 2 first letters of provider, plus 2 numbers
            bird_id = as.character(bird_id),
            # ring id
            ring_id = NA,
            # capture date
            date_start = date(new_trk$datetime[1]),
            # date of last location
            date_end = NA,
            # name of the bird
            name = NA,
            
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
    
    saveRDS(new_db, file = paste("data/working/pre_proc_data/db_", bird_id,"_pp.rds", sep = ""))
    
    
}
