# 19-01-2021

# In this script we annotate bird tracks with different covariates

# Note that this script must be ran only after covariates have been prepared in
# "R/3_prepare_covts"

rm(list = ls())

library(tidyverse)
library(lubridate)


# Read in data ------------------------------------------------------------

# Read in database with all birds
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# List all tracking file names that are model ready
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")


# Fill in data base -------------------------------------------------------

for(i in 1:length(trk_files)){
      
      # Read in track file
      trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i]))
      
      # Define bird id
      id_sel <- unique(trk$bird_id)
      
      # Check if track has been processed or it was excluded early on
      if(length(attr(trk, "height")) == 0){
            print("No height information?")
            next
      }
      
      # If the track has been processed annotate wit covariates
      
}
