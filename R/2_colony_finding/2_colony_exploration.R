# 21-09-2020

# In this script we explore the distance between colonies
# calculated by looking at the maximum density of points
# and the closest known colonies. Also at how colonies change
# over the years.

rm(list = ls())

library(tidyverse)



# Read in data ------------------------------------------------------------

# Read in colony data
colony <- read_csv("data/working/colony_db.csv")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")


# Process colony data -----------------------------------------------------

# Add age to colony data
colony <- colony %>% 
    left_join(dplyr::select(bird_db, bird_id, age), by = "bird_id")

# Create variables of distance between observed colonies over the years
colony <- colony %>% 
    group_by(bird_id) %>% 
    mutate(dist_year_lon = lag(dens_lon) - dens_lon,
           dist_year_lat = lag(dens_lat) - dens_lat,
           dist_year = sqrt(dist_year_lon^2 + dist_year_lat^2)) %>% 
    ungroup()

# Plot distance between observed and known colonies -----------------------

colony %>% 
    ggplot() +
    geom_histogram(aes(x = dist))

colony %>% 
    filter(dist > 100) %>% 
    pull(bird_id) %>% 
    unique()

# Plot change in colony as function of age --------------------------------

colony %>% 
    ggplot() +
    geom_boxplot(aes(x = age, y = dist_year))



# Plot change in colony for each juvenile bird ----------------------------

colony %>% 
    filter(age == "juv") %>% 
    group_by(bird_id) %>% 
    mutate(t = row_number()) %>% 
    ungroup() %>% 
    ggplot() +
    geom_line(aes(x = t, y = dist_year)) +
    geom_point(aes(x = t, y = dist_year)) +
    facet_wrap(~bird_id)
