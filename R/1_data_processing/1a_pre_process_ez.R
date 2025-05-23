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
    
    new_trk <- read_excel("data/raw/ez/Cape vulture data_Ezemvelo_Feb19_datecorrected.xls", sheet = i)
    
    # Pre-processing ----------------------------------------------------------
    
    # Set bird id
    tag_id <- unique(new_trk$Id)
    
    if(tag_id == 57355) next
    
    bird_id <- paste("ez0", dat_summary$`Bird #`[which(dat_summary$`PTT id` == unique(new_trk$Id))], sep = "")
    
    # Check template columns and new bird columns
    colnames(bird_trk)
    colnames(new_trk)
    
    # Fix Date column to be POSIXct variable
    new_trk$Date <- as.POSIXct(new_trk$Date, format = "%m/%d/%Y", tz = "GMT")
    
    # Create variables to match template.
    new_trk <- new_trk %>% 
        mutate(bird_id = bird_id,    # create identifier for the bird
               tag_id = as.character(tag_id),
               datetime = as.POSIXct(paste(Date, format(ymd_hms(Time), "%H:%M:%S")), tz = "GMT")) %>% 
        arrange(datetime) %>%       # Sort data by date before computing dt
        mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
               lon = as.double(Longitude),
               lat = as.double(Latitude),
               alt = as.double(Altitude),
               heading = as.double(Course),
               spd_h = if_else(tag_id == 87988, as.double(Speed) * 1.852, as.double(Speed)), # ASSUMING THAT SPEED IS IN 2D? Also for tag 87988 data said to multiply by 1.852
               spd_v = NA, spd_3d = NA, error_h = NA, error_v = NA, 
               error_3d = if('PDOP' %in% names(.)) PDOP else NA) %>% 
        select(colnames(bird_trk))
    
    saveRDS(new_trk, file = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.rds", sep = ""))
    
    
    # Fill in track template --------------------------------------------------
    
    unique(new_trk$tag_id)
    dat_summary
    index <- which(dat_summary$`PTT id` == tag_id)
    
    # Fix age and sex
    age <- dat_summary$`Age when caught`[index]
    
    age <- if_else(age == "Adult", "ad",
                   if_else(age == "Juvenile", "juv",
                           "subad"))
    
    sex <- dat_summary$Sex[index] %>% tolower()
    
    tag_type_sel <- dplyr::case_when(bird_id == "ez02" ~ "GPS-GSM Africa Wildlife Tracking",
                                     bird_id == "ez05" ~ "GPS North Star",
                                     TRUE ~ "Argos-GPS PTT-100 Microwave Telemetry")
    
    accu_sel <- dplyr::case_when(bird_id == "ez02" ~ 10,
                                 bird_id == "ez05" ~ 15,
                                 TRUE ~ 18)
    
    
    new_db <- dat_summary %>% 
        filter(`PTT id` == unique(new_trk$tag_id)) %>% 
        mutate(
            # trasmitter id
            tag_id = as.character(`PTT id`),
            # tag model
            tag_type = tag_type_sel,
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
            name = as.character(Name),
            
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
            sd_dt = NA,
            # Wild/rehab bird
            rehab = 0,
            # Accuracy as per manufacturer (m)
            accu = accu_sel) %>% 
        select(colnames(bird_db))
    
    saveRDS(new_db, file = paste("data/working/pre_proc_data/db_", bird_id,"_pp.rds", sep = ""))
    
}
