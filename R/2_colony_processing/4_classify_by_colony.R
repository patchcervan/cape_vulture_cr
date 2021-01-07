# 1-10-2020

# In this script we classify birds according to the colony they spend the
# most time around

rm(list = ls())

library(tidyverse)
library(sf)


# Read in data ------------------------------------------------------------

# Read in colony data
colony <- read_csv("data/working/colony_db.csv")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Load basemap
source("R/functions/load_basemap.R")

# Add age to colony data
colony <- colony %>% 
    left_join(dplyr::select(bird_db, bird_id, age), by = "bird_id")

# Plot colonies -----------------------------------------------------------

# Plot observed centres of activity
colony %>% 
    ggplot() +
    geom_sf(data = sa_map) +
    geom_point(aes(x = dens_lon, y = dens_lat, colour = age)) +
    geom_hline(aes(yintercept = -27.5)) +
    geom_vline(aes(xintercept = 24))



# Classify birds in 4 sectors ---------------------------------------------

colony <- colony %>% 
    mutate(zone = case_when(dens_lon > 24 & dens_lat > -27.5 ~ 1,
                            dens_lon > 24 & dens_lat < -27.5 ~ 2,
                            dens_lon < 24 & dens_lat < -27.5 ~ 3,
                            dens_lon < 24 & dens_lat > -27.5 ~ 4))
colony %>% 
    ggplot() +
    geom_sf(data = sa_map) +
    geom_text(aes(x = dens_lon, y = dens_lat, label = zone)) +
    geom_hline(aes(yintercept = -27.5)) +
    geom_vline(aes(xintercept = 24))


# Save colony db ----------------------------------------------------------


write_csv(colony, "data/working/colony_db.csv")
