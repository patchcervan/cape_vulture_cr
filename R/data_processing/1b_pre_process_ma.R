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
dat_summary <- read_csv("data/raw/ma/CapesMarburg_DataOverview_Megan_13122018.csv")
dat_summary

# THIS IS A VERY LARGE FILE. RUN THE FIRST TIME TO DIVIDE IT IN TEMPORARY INDIVIDUAL
# BIRD FILES. THEN, SKIP DIRECTLY TO PRE-PROCESSING.
# new_trk <- read_csv("data/raw/ma/E-obs GSM Kapgeier Southafrica.csv")


# Create temporary individual files ---------------------------------------

# This dataset is too big to be handled at once. We will divide it into birds
# and create temporary individual files.

# This takes a while so RUN ONLY IF NECESSARY!!!
# source("R/functions/create_ma_temp_files.R")




# Pre-processing ----------------------------------------------------------

# Load one bird
dir("data/working/pre_proc_data/temp_ma")

# Set bird ids. These ids match the order of the birds in the summary file and
# those in the temporary files directory
# ids <-c("07", "09", "05", "11", "03", "13", "10", "06", "04", "15", "01", "02", "12", "16", "08", "14") 

# The loop will process all birds. If only one bird wants to be processed
# set i <- <desired bird number> and run the body of the loop.

for(i in 1:length(dir("data/working/pre_proc_data/temp_ma"))){
    
new_trk <- read_csv(paste("data/working/pre_proc_data/temp_ma/",
                          dir("data/working/pre_proc_data/temp_ma")[i], sep = ""))

bird_name <- unique(new_trk$bird_name)
dat_summary

index <- which(str_detect(dat_summary$AnimalID, bird_name))

bird_id <- if(index < 10) paste0("ma0", index) else paste0("ma", index)


# Check template columns and new bird columns
colnames(bird_trk)
colnames(new_trk)

# Fix Date column to be POSIXct variable
new_trk$`study-local-timestamp` <- as.POSIXct(new_trk$`study-local-timestamp`, format = "%Y/%m/%d")

# Create variables to match template.
new_trk <- new_trk %>% 
    mutate(bird_id = bird_id,    # create identifier for the bird
           tag_id = as.character(`tag-local-identifier`),
           datetime = as.POSIXct(`study-local-timestamp`)) %>% 
    arrange(datetime) %>%       # Sort data by date before computing dt
    mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
           lon = as.double(`location-long`),
           lat = as.double(`location-lat`),
           x = NA, y = NA,      # These will be filled-in later
           alt = as.double(`height-above-ellipsoid`),
           heading = as.double(heading),
           spd_h = as.double(`ground-speed`), # ASSUMING THAT SPEED IS IN 2D?
           spd_v = NA, spd_3d = NA, 
           error_h = as.double(`eobs:horizontal-accuracy-estimate`),
           error_v = NA, 
           error_3d = as.double(`gps:dop`)) %>% 
    select(colnames(bird_trk))

write_csv(new_trk, path = paste("data/working/pre_proc_data/trk_", bird_id,"_pp.csv", sep = ""))


# Fill in track template --------------------------------------------------

unique(new_trk$tag_id)
dat_summary

# Fix age and sex
age <- dat_summary$Age[index]
age <- if_else(age == "First year", "juv","ad")

sex <- if_else(is.na(dat_summary$SexMDS[index]), 
               dat_summary$SuspectedSex[index], 
               dat_summary$SexMDS[index]) %>% 
    tolower()

sex <- if(is.na(sex)) "unknown" else sex


new_db <- dat_summary %>% 
    filter(str_detect(dat_summary$AnimalID, bird_name)) %>% 
    mutate(
        # trasmitter id
        tag_id = as.character(TransmitterID),
        # tag model
        tag_type = "e-obs_GPS/GSM",
        # Speed units
        spd_units = as.character("km/h"),
        # unique bird identifier - 2 first letters of provider, plus 2 numbers
        bird_id = as.character(bird_id),
        # ring id
        ring_id = RingNo,
        # capture date
        date_start = as.POSIXct(strftime(new_trk$datetime[1], format = "%m/%d/%y"), tz = "GMT", format = "%m/%d/%y"),
        # date of last location
        date_end = NA,
        # name of the bird
        name = as.character(bird_name),
        
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