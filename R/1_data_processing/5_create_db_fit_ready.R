# 1-10-2020

# In this script we create a data base summary of the data that is ready
# for model fit.

rm(list = ls())

library(tidyverse)
library(lubridate)


# Read in data ------------------------------------------------------------

# Read in database with all birds
bird_db_all <- read_csv("data/working/bird_db.csv")

# List all tracking file names that are model ready
trk_files <- list.files("data/working/bird_tracks/fit_ready", pattern = ".csv")

# Create new data base to store birds that are model fit ready.
# We will add the median of dt because it is usually more representative than the mean.
new_db <- bird_db_all %>% 
    slice(0) %>% 
    mutate(med_dt = double())



# Fill in data base -------------------------------------------------------

for(i in 1:length(trk_files)){
    
    # i <- 1
    
    # Read in track file
    trk <- read_csv(paste0("data/working/bird_tracks/fit_ready/", trk_files[i]))
    
    # Define bird id
    id_sel <- unique(trk$bird_id)
    
    # Compute time increments between locations
    trk <- trk %>% 
        mutate(dt = as.numeric(difftime(lead(t_), t_, units = "hours")))
    
    # Extract bird from database with all birds
    new_record <- bird_db_all %>% 
        filter(bird_id == id_sel)
    
    
    # Change fields that have changed when making the bird ready for modelling
    new_record <- new_record %>% 
        mutate(
            date_start = head(trk$t_, 1),
            date_end = tail(trk$t_, 1),
            age = paste(unique(trk$age), collapse = ","),
            nloc_post = nrow(trk),
            avg_dt = mean(trk$dt, na.rm = T),
            sd_dt = sd(trk$dt, na.rm = T),
            med_dt = median(trk$dt, na.rm = T)
    )
    
    new_db <- rbind(new_db, new_record)
    
}


# Save new database
write_csv(new_db, "data/working/bird_db_fit_ready.csv")
