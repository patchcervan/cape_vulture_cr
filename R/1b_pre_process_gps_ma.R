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
dat_summary <- read_csv("data/raw/E-obs GSM Kapgeier Southafrica-reference-data.csv")
dat_summary

# THIS IS A VERY LARGE FILE. RUN THE FIRST TIME TO DIVIDE IT IN TEMPORARY INDIVIDUAL
# BIRD FILES. THEN, SKIP DIRECTLY TO PRE-PROCESSING.
new_trk <- read_csv("data/raw/E-obs GSM Kapgeier Southafrica.csv")


# Create temporary individual files ---------------------------------------

# This dataset is too big to be handled at once. We will divide it into birds
# and create temporary individual files.

# This takes a while so RUN ONLY IF NECESSARY!!!
source("R/functions/create_ma_temp_files.R")




# Pre-processing ----------------------------------------------------------

# Load one bird
dir("data/working/pre_proc_data/temp_ma")

# Set bird ids. These ids match the order of the birds in the summary file and
# those in the temporary files directory
ids <-c("07", "09", "05", "11", "03", "13", "10", "06", "04", "15", "01", "02", "12", "16", "08", "14") 

# The loopwill process all birds. If only one bird wants to be processed
# set i <- <desired bird number> and run the body of the loop.

for(i in 1:length(dir("data/working/pre_proc_data/temp_ma"))){
    
new_trk <- read_csv(paste("data/working/pre_proc_data/temp_ma/",
                          dir("data/working/pre_proc_data/temp_ma")[i], sep = ""))

unique(new_trk$bird_name)
dat_summary

bird_id <- paste("ma", ids[i], sep = "")


# Check template columns and new bird columns
colnames(bird_trk)
colnames(new_trk)

# Fix Date column to be POSIXct variable (using SA standard time - SAST)
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
age <- if_else(bird_id %in% paste("ma", c("09", "11", "12", "14"), sep = ""), "ad",
               "juv")

sex <- if_else(bird_id %in% paste("ma", c("01", "03", "05"), sep = ""), "female",
               if_else(bird_id %in% paste("ma", c("02", "04"), sep = ""), "male",
                       "unknown"))


new_db <- dat_summary %>% 
    filter(str_detect(`animal-id`, str_sub(dir("data/working/pre_proc_data/temp_ma")[i], 1, -10))) %>% 
    mutate(
        # trasmitter id
        tag_id = as.character(`tag-id`),
        # tag model
        tag_type = "e-obs_GPS/GSM",
        # Speed units
        spd_units = as.character("km/h"),
        # unique bird identifier - 2 first letters of provider, plus 2 numbers
        bird_id = as.character(bird_id),
        # capture date
        date_cap = as.POSIXct(`deploy-on-date`),
        # date of last location
        date_end = NA,
        # name of the bird
        name = as.character(str_sub(dir("data/working/pre_proc_data/temp_ma")[i], 1, -10)),
        
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