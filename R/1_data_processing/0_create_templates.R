# 13-07-2020

# In this script we create template for the storage of
# information of tagged birds. There are two templates:
# - bird_db - contains information about birds and their tags
# - bird_trk - contains tracking locations and associated info

# We will use the Ezemvelo summary sheet as a starting point

rm(list = ls())

library(tidyverse)
library(lubridate)


# Bird data base ----------------------------------------------------------

bird_db <- tibble(
    # trasmitter id
    tag_id = character(),
    # tag model
    tag_type = character(),
    # Speed units
    spd_units = character(),
    # unique bird identifier - 2 first letters of provider, plus 2 numbers
    bird_id = character(),
    # ring id if the bird was ringed (SAFRING)
    ring_id = character(),
    # capture date
    date_start = POSIXct(),
    # date of last location
    date_end = POSIXct(),
    # name of the bird
    name = character(),
    # bird age when caught - factor with levels:juvenile, sub-adult, adult
    age = factor(levels = c("juv", "subad", "ad")),
    # bird sex - factor with levels:male, female
    sex = factor(levels = c("male", "female", "unknown")),
    # number of locations in raw data
    nloc_pre = double(),
    # number of locations in processed data
    nloc_post = double(),
    # mean sampling rate (hours)
    avg_dt = double(),
    # standard deviation of sampling rate (hours)
    sd_dt = double()
)

# Save template
saveRDS(bird_db, file = "data/working/bird_db_template.rds")

# Create bird data base to store bird data
if("bird_db.csv" %in% dir("data/working")){
    print("Database present, if you are sure you want to overwrite, run the write_csv alone.")
} else {
    write_csv(bird_db, file = "data/working/bird_db.csv")
}


# Bird tracking data ------------------------------------------------------

bird_trk <- tibble(
    # unique bird identifier - 2 first letters of provider, plus 2 numbers
    bird_id = character(),
    # trasmitter id
    tag_id = character(),
    # Timestamp
    datetime = ymd_hms(),
    # Time increment (hours)
    dt = double(),
    # longitude
    lon = double(),
    # latitude
    lat = double(),
    # Altitude a.s.l (m)
    alt = double(),
    # course (heading?)
    heading = double(),
    # horizontal speed (km/h)
    spd_h = double(),
    # vertical speed
    spd_v = double(),
    # 3D speed
    spd_3d = double(),
    # Horizontal error
    error_h = double(),
    # Vertical error
    error_v = double(),
    # Position error (3D)
    error_3d = double()
)

# Save template
saveRDS(bird_trk, file = "data/working/bird_trk_template.rds")