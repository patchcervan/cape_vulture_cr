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

new_trk <- read_csv("data/raw/ew/EWT_DRUID_RAPTORS.csv")

range(new_trk$timestamp)

unique(new_trk$`individual-local-identifier`)

new_trk %>% 
    group_by(`individual-local-identifier`) %>% 
    summarize(range_time = range(timestamp))



# Keep only the Cape vultures, which are identified by the code CV
new_trk <- new_trk %>% 
    filter(str_detect(`individual-local-identifier`, "CV"))

unique(new_trk$`individual-local-identifier`)

bird_name <- unique(new_trk$`individual-local-identifier`)

new_trk %>% 
    group_by(`individual-local-identifier`) %>% 
    mutate(dt = as.double(difftime(lead(timestamp), timestamp, units = "hour"))) %>% 
    summarize(avg_dt = median(dt, na.rm = T),
              sd_dt = sd(dt, na.rm = T))

# These data comes in very high definition. I will subset to every  15 min
require(amt)

# Nest by individual
new_trk <- new_trk %>% 
    nest(data = c(-`individual-local-identifier`))

# Make amt object
new_trk$data <- new_trk$data %>% 
    purrr::map(~make_track(.x, `location-long`, `location-lat`, timestamp, all_cols = T))

# Subset to 15 mins
new_trk$data <- new_trk$data %>% 
    purrr::map(~track_resample(.x, rate = minutes(15),
                                    tolerance = minutes(5)))
# Unnest
new_trk <- new_trk %>% 
    unnest(cols = c(`individual-local-identifier`, data))


# Set bird id
tag_id <- unique(new_trk$`tag-local-identifier`)

bird_id <- paste0("ew0", 1:2)

# Check template columns and new bird columns
colnames(bird_trk)
colnames(new_trk)

# Create variables to match template.
new_trk <- new_trk %>% 
    mutate(bird_id = if_else(`individual-local-identifier` == bird_name[1], bird_id[1], bird_id[2]),    # create identifier for the birds
           tag_id = as.character(`tag-local-identifier`),
           datetime = t_) %>% 
    arrange(datetime) %>%       # Sort data by date before computing dt
    mutate(dt = as.double(difftime(lead(datetime), datetime, units = "hour")),
           lon = as.double(x_),
           lat = as.double(y_),
           alt = as.double(`height-above-msl`),
           heading = as.double(heading),
           spd_h = `ground-speed`, # ASSUMING THAT SPEED IS IN 2D?
           spd_v = NA, spd_3d = NA, error_h = NA, error_v = NA, 
           error_3d = NA) %>% 
    select(colnames(bird_trk))

for(i in 1:length(bird_id)){
    new_trk %>% 
        filter(bird_id == unique(bird_id)[i]) %>% 
        saveRDS(file = paste0("data/working/pre_proc_data/trk_", bird_id[i],"_pp.rds"))
}



# Fill in track template --------------------------------------------------

unique(new_trk$tag_id)

new_db <- tibble(
    # trasmitter id
    tag_id = tag_id,
    # tag model
    tag_type = "GPS-GSM Druid",
    # Speed units
    spd_units = "km/h",
    # unique bird identifier - 2 first letters of provider, plus 2 numbers
    bird_id = bird_id,
    # ring id if the bird was ringed (SAFRING)
    ring_id = NA,
    # capture date
    date_start = date(new_trk %>% group_by(bird_id) %>% slice_head() %>% pull(datetime)),
    # date of last location
    date_end = date(new_trk %>% group_by(bird_id) %>% slice_tail() %>% pull(datetime)),
    # name of the bird
    name = bird_name,
    # bird age when caught - factor with levels:juvenile, sub-adult, adult
    age = factor(c("juv", "juv"), levels = c("juv", "subad", "ad", "unknown")),
    # bird sex - factor with levels:male, female
    sex = factor(c("male", "female"), levels = c("male", "female", "unknown")),
    # number of locations in raw data
    nloc_pre = new_trk %>% group_by(bird_id) %>% tally() %>% pull(n),
    # number of locations in processed data
    nloc_post = NA,
    # mean sampling rate (hours)
    avg_dt = NA,
    # standard deviation of sampling rate (hours)
    sd_dt = NA,
    # Wild/rehab bird
    rehab = 0,
    # Accuracy as per manufacturer (m)
    accu = 15
)

for(i in 1:length(bird_id)){
    new_db %>% 
        filter(bird_id == unique(bird_id)[i]) %>% 
        saveRDS(file = paste("data/working/pre_proc_data/db_", bird_id[i],"_pp.rds", sep = ""))
}