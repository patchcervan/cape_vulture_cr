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

# There is no data summary sheet for this vulture so I will just create one



# Pre-processing ----------------------------------------------------------

new_trk <- read_excel("data/raw/kr/MASTERcape.xlsx", sheet = 1)

# There is a comment on one cell of column I. I will ignore this column

bird_name <- unique(new_trk$Name)

# Set bird id
tag_id <- unique(new_trk$ID)

bird_id <- paste0("kr0", 1:2)

# Check template columns and new bird columns
colnames(bird_trk)
colnames(new_trk)

# Fix Date column to be POSIXct variable
new_trk$Date_Time <- as.POSIXct(new_trk$Date_Time, format = "%Y/%m/%d")

# Create variables to match template.
new_trk <- new_trk %>% 
    mutate(bird_id = if_else(Name == bird_name[1], bird_id[1], bird_id[2]),    # create identifier for the birds
           tag_id = as.character(ID),
           datetime = Date_Time) %>% 
    arrange(datetime) %>%       # Sort data by date before computing dt
    mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
           lon = as.double(Long),
           lat = as.double(Lat),
           x = NA, y = NA,      # These will be filled-in later
           alt = as.double(Alt),
           heading = as.double(Course),
           spd_h = Speed, # ASSUMING THAT SPEED IS IN 2D? Also for tag 87988 data said to multiply by 1852
           spd_v = NA, spd_3d = NA, error_h = NA, error_v = NA, 
           error_3d = NA) %>% 
    select(colnames(bird_trk))

for(i in 1:length(bird_id)){
    new_trk %>% 
        filter(bird_id == unique(bird_id)[i]) %>% 
        write_csv(path = paste0("data/working/pre_proc_data/trk_", bird_id[i],"_pp.csv"))
}



# Fill in track template --------------------------------------------------

unique(new_trk$tag_id)

new_db <- tibble(
    # trasmitter id
    tag_id = tag_id,
    # tag model
    tag_type = "GPS_Microwave_Telemetry",
    # Speed units
    spd_units = "km/h",
    # unique bird identifier - 2 first letters of provider, plus 2 numbers
    bird_id = bird_id,
    # ring id if the bird was ringed (SAFRING)
    ring_id = NA,
    # capture date
    date_start = format(new_trk %>% group_by(bird_id) %>% slice_head() %>% pull(datetime), format = "%m/%d/%Y"),
    # date of last location
    date_end = format(new_trk %>% group_by(bird_id) %>% slice_tail() %>% pull(datetime), format = "%m/%d/%Y"),
    # name of the bird
    name = bird_name,
    # bird age when caught - factor with levels:juvenile, sub-adult, adult
    age = factor(rep("ad", length(bird_id)), levels = c("juv", "subad", "ad")),
    # bird sex - factor with levels:male, female
    sex = factor(c("male", "female"), levels = c("male", "female", "unknown")),
    # number of locations in raw data
    nloc_pre = new_trk %>% group_by(bird_id) %>% tally() %>% pull(n),
    # number of locations in processed data
    nloc_post = NA,
    # mean sampling rate (hours)
    avg_dt = NA,
    # standard deviation of sampling rate (hours)
    sd_dt = NA
)

for(i in 1:length(bird_id)){
    new_db %>% 
        filter(bird_id == unique(bird_id)[i]) %>% 
        write_csv(path = paste("data/working/pre_proc_data/db_", bird_id[i],"_pp.csv", sep = ""))
}
