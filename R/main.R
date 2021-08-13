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

# Also run fine processing: exclude certain birds, cut out certain periods, change ages for
# long term tracking, re-sample very high resolution tracks. Do before any colony processing
source("R/1_data_processing/3_fine_process.R")


# Prepare colony data -----------------------------------------------------

# Load scripts
scripts <- dir("R/2_colony_processing")

# Run the processing scripts for colonies
for(s in seq_along(scripts)){
    print(s)
    # Load scripts
    scripts <- dir("R/2_colony_processing")
    source(paste0("R/2_colony_processing/", scripts[s]))
}


# Run other processing scripts -----------------------------------------------

# Run the colony process scripts in which we find the central places of the 
# vultures (WE SHOULD HAVE RAN "R/2_colony_finding/1_colony_finding.R" ALREADY)
source("R/1_data_processing/4_colony_process.R")

# Run script to classify movement modes. This takes really long, so consider
# running in the UCT HPC. If ran in the HPC, there is a second part in the script
# (from "Explore state proportions") that must be ran in the local script if one
# wants to explore state proportions and things like that. Do it manually.
source("R/1_data_processing/5_move_mode_process.R")


# Prepare raster covariates -----------------------------------------------

# Create slope and ruggedness rasters from elevation. Also set sea level = 0
# Takes a while
source("R/3_prepare_covts/1_prep_covts_topo_rasters.R")

# Re-classify habitats and create a csv file with the codes
source("R/3_prepare_covts/2_prep_land_cover.R")

# Create a single shapefile with protected areas
source("R/3_prepare_covts/3_prep_prot_areas.R")

# Identify steep slopes to calculate distance to them later
# Might have to unload certain packages for it to run properly (best just re-start R)
source("R/3_prepare_covts/4_prep_steep_slopes.R")

# Create rasters with distance to steep slopes to use as covariates
# This takes very long, so don't run unless completely necessary
source("R/3_prepare_covts/5_prep_dist_slp.R")

# Increase the resolution of distance to slopes and tranform to meters
source("R/3_prepare_covts/5a_red_dist_slp_m.R")

# Prepare the supplementary feeding sites file for processing
source("R/3_prepare_covts/6_prep_covts_restaurants.R")


# Run the last processing scripts -----------------------------------------------

# Run script to process heights and correct for heights that wrap to zero when
# going over 2042 and other known issues
source("R/1_data_processing/6_height_process.R")

# Analyze distance traveled per day to try to detect anomalies
source("R/1_data_processing/7_dist_process.R")

# Create a database with the new information extracted from the birds
source("R/1_data_processing/8_create_db_fit_ready.R")


# Fit SSF model -----------------------------------------------------------

# Prepare data regularizing trajectories and annotating with covariates
source("R/4_ssf_model/1a_prep_ssf_data.R")

