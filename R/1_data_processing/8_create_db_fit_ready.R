# 10-01-2020

# In this script we create a data base summary of the data that is ready
# for model fit.

rm(list = ls())

library(tidyverse)
library(lubridate)


# Read in data ------------------------------------------------------------

# Read in database with all birds
bird_db_all <- read_csv("data/working/bird_db.csv")

# List all tracking file names that are model ready
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")

# Read in problems with bird locations
loc_issues <- read_csv("data/working/problem_birds_locs.csv")

# Read in birds that are excluded because of movement modes
mov_issues <- read_csv("data/working/birds_mxt_exclude.csv")

# Read in height issues
hgt_issues <- read_csv("data/working/problem_birds_height.csv")

# Create new data base to store birds that are model fit ready.
# We will add the median of dt because it is usually more representative than the mean.
new_db <- bird_db_all %>% 
    slice(0) %>% 
    mutate(med_dt = double())


# Fill in data base -------------------------------------------------------

for(i in 1:length(trk_files)){
    
    # Read in track file
    trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i]))
    
    # Define bird id
    id_sel <- unique(trk$bird_id)
    
    # Find problems with locs
    l <- loc_issues %>% 
        filter(bird_id == id_sel)
    
    # Find problems with movement modes
    m <- mov_issues %>% 
        filter(bird_id == id_sel)
    
    # Find problems with height
    h <- hgt_issues %>% 
        filter(bird_id == id_sel)
    
    # Extract bird from database with all birds
    new_record <- bird_db_all %>% 
        filter(bird_id == id_sel)
    
    
    # Change fields that have changed when making the bird ready for modelling
    new_record <- new_record %>% 
        mutate(
            date_start = head(trk$datetime, 1),
            date_end = tail(trk$datetime, 1),
            age = age,
            nloc_post = nrow(trk),
            avg_dt = mean(trk$dt, na.rm = T),
            sd_dt = sd(trk$dt, na.rm = T),
            med_dt = median(trk$dt, na.rm = T),
            keep_loc = ifelse(!id_sel %in% loc_issues$bird_id, 1,
                               case_when(str_detect(l$status, "Included") ~ 1,
                                         str_detect(l$status, "Excluded") ~ 0)),
            keep_mov = ifelse(length(m$keep) == 0, NA, m$keep),
            keep_hgt = ifelse(length(h$keep) == 0, NA, h$keep),
            keep_all = ifelse(sum(keep_mov, keep_hgt, keep_hgt, na.rm = T) == 3, 1, 0)
    )
    
    new_db <- rbind(new_db, new_record)
    
}


# Save new database
write_csv(new_db, "data/working/bird_db_fit_ready.csv")
