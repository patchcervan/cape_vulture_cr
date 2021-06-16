# 15-06-2021

# This scripts runs all the necessarty scripts for the preparation of the colony data

rm(list = ls())


# Prepare colony locations ------------------------------------------------

source("R/2_colony_processing/1_prep_colony_locs.R")


# Prepare colony counts ---------------------------------------------------

source("R/2_colony_processing/2_prep_colony_counts.R")


# Integrate multiple data sources -----------------------------------------

source("R/2_colony_processing/3_integrate_col.R")

source("R/2_colony_processing/4_DA_colonies.R")


# Find colonies as areas of highest use intensity -------------------------

# This script can run in multicore for that run the following line. Choose the righ number of cores
# future::plan("multisession", workers = 4)
source("R/2_colony_processing/5_colony_finding.R")

