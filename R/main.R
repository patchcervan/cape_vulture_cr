# This script runs all the processing scripts
# This takes quite a while so run only if completely necessary
# and go find something to do.

rm(list = ls())

library(tidyverse)

# Load scripts
scripts <- dir("R/1_data_processing")

# remove the creation of templates. If you want to overwrite templates you will
# have to run the scripts directly
scripts <- scripts[str_detect(scripts, "0", negate = T)]

# Run the processing scripts for specific birds

# Run processing scripts for specific birds -------------------------------

proc <- scripts[1:18]

for(f in 1:length(proc)){
    
    # Need to reload because of the rm functions in the scripts
    scripts <- dir("R/1_data_processing")
    scripts <- scripts[str_detect(scripts, "0", negate = T)]
    proc <- scripts[1:18]
    
    print(f)
    
    source(paste0("R/1_data_processing/", proc[f]))
}



# Prepare colony data -----------------------------------------------------

# Load scripts
scripts <- dir("R/2_colony_processing")

# Run the processing scripts for colonies
for(s in seq_along(scripts)){
    print(f)
    source(paste0("R/2_colony_processing/", proc[f]))
}


# Run other general scripts -----------------------------------------------

# Run fine processing: exclude certain birds, cut out certain periods, change ages for
# long term tracking, re-sample very high resolution tracks
source("R/1_data_processing/3_fine_process.R")

# Run the colony process scripts in which we find the central places of the 
# vultures (WE SHOULD HAVE RAN "R/2_colony_finding/1_colony_finding.R" ALREADY)
source("R/1_data_processing/4_colony_process.R")

# Run script to classify movement modes. This takes really long, so consider
# running in the UCT HPC. If ran in the HPC, there is a second part in the script
# (from "Explore state proportions") that must be ran in the local script. Do it manually.
source("R/1_data_processing/5_move_mode_process.R")

# Run script to process heights and correct for heights that wrap to zero when
# going over 2042 and other known issues
source("R/1_data_processing/6_height_process.R")

# Create a database with the new information extracted from the birds
source("R/1_data_processing/8_create_db_fit_ready.R")
