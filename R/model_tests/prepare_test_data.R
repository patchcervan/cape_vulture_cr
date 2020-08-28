# 07-08-2020

# In this script I will select a few of the vultures with best data
# to test a few candidate models

rm(list = ls())

library(tidyverse)
library(lubridate)

# Read in database --------------------------------------------------------

db <- read_csv("data/working/bird_db.csv")


# Select birds ------------------------------------------------------------

# Filter birds with at least one year of tracking
db_sel <- db %>% 
    mutate(date_start = as.POSIXct(date_start, format = "%m/%d/%Y"),
           date_end = as.POSIXct(date_end, format = "%m/%d/%Y"),
           dur = date_end - date_start) %>% 
    filter(dur > 300) %>% 
    print(n = Inf)

# Explore timeline
db_sel %>% 
    ggplot() + 
    geom_segment(aes(x = date_start, xend = date_end, 
                     y = bird_id, yend = bird_id, col = age), size = 2) + 
    geom_vline(xintercept = lubridate::mdy(paste0("01/01/", 2003:2020)), 
               colour = "grey") +
    xlab("") +
    # scale_x_date(breaks = lubridate::y(paste0("01/01/", 2003:2020))) +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 5),
          axis.text.x = element_text(size = 8))

# I will manually select birds from different areas, age classes and tracking resolution
bird_ids <- c("wt18", "wt07", "na03", "na06", "ma14", "ma15")


# Load tracking files -----------------------------------------------------

# Extract track files
trkfiles <- list.files("data/working/bird_tracks", pattern = ".csv")

# Keep only those files that correspond to the selected bird ids
trkfiles <- trkfiles[trkfiles %in% paste0(bird_ids, ".csv")]


# Select tracking period --------------------------------------------------

# Create data frame to store test data
trk_all <- data.frame()

# Loop through files and keep the periods that we want
study_period <- list(wt18 = c("2010-01-01","2011-01-01"),
                     wt07 = c("2008-01-01","2009-01-01"),
                     na03 = c("2006-01-01","2007-01-01"),
                     na06 = c("2006-01-01","2007-01-01"),
                     ma14 = c("2017-01-01","2018-01-01"),
                     ma15 = c("2017-01-01","2018-01-01"))

for(i in seq_along(trkfiles)){
    
    # Load track
    trk <- read_csv(paste0("data/working/bird_tracks/", trkfiles[i]))
    
    # identify bird and tracking period
    print(unique(trk$bird_id))
    tp <- date(study_period[unique(trk$bird_id)][[1]])
    
    trk <- trk %>% 
        filter(datetime > tp[1] & datetime < tp[2])
    
    trk_all <- rbind(trk_all, trk)
}


unique(trk_all$bird_id)

# save test data
write_csv(trk_all, path = "data/working/test_data/test_data.csv")
