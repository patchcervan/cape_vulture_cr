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
excel_sheets("data/raw/ez/Cape vulture data_Ezemvelo_Feb19.xls")

dat_summary <- read_excel("data/raw/ez/Cape vulture data_Ezemvelo_Feb19_datecorrected.xls", sheet = "Summary")
dat_summary

for(i in 2:10){
    
    new_trk <- read_excel("data/raw/ez/Cape vulture data_Ezemvelo_Feb19.xls", sheet = i)
    
    
    # Pre-processing ----------------------------------------------------------
    
    # Set bird id
    unique(new_trk$Id)
    
    bird_id <- paste("ez0", dat_summary$`Bird #`[which(dat_summary$`PTT id` == unique(new_trk$Id))], sep = "")
    
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    new_trk$Date <- as.POSIXct(new_trk$Date, format = "%m/%d/%Y")
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = as.character(Id),
               datetime = as.POSIXct(paste(Date, format(ymd_hms(Time), "%H:%M:%S")))) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(Longitude),
               lat = as.double(Latitude),
               x = NA, y = NA,      # These will be filled-in later
               alt = as.double(Altitude),
               heading = as.double(Course),
               spd_h = if_else(tag_id == 87988, as.double(Speed) * 1852, as.double(Speed)), # ASSUMING THAT SPEED IS IN 2D? Also for tag 87988 data said to multiply by 1852
               spd_v = NA, spd_3d = NA, error_h = NA, error_v = NA, 
               error_3d = if('PDOP' %in% names(.)) PDOP else NA) %>% 
        select(colnames(bird_trk))
    
    write_csv(new_trk, path = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.csv", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    
    # Fix age and sex
    age <- if_else(bird_id %in% paste("ez0", c(2,5), sep = ""), "ad",
                   if_else(bird_id %in% paste("ez0", 1, sep = ""), "juv",
                           "subad"))
    
    sex <- if_else(bird_id %in% paste("ez0", c(1,2), sep = ""), "male",
                   if_else(bird_id %in% paste("ez0", 4, sep = ""), "female",
                           "unknown"))
    
    
    new_db <- dat_summary %>% 
        filter(`PTT id` == unique(new_trk$tag_id)) %>% 
        mutate(
            # trasmitter id
            tag_id = as.character(`PTT id`),
            # tag model
            tag_type = as.character(`Type of data`),
            # Speed units
            spd_units = as.character("km/h"),
            # unique bird identifier - 2 first letters of provider, plus 2 numbers
            bird_id = as.character(bird_id),
            # capture date
            date_cap = as.POSIXct(`Date caught`),
            # date of last location
            date_end = NA,
            # name of the bird
            name = as.character(Name),
            
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
