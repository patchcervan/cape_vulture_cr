# 21-09-2020

# In this script we create template for the storage of
# information pertaining the colonies that are central to
# the different birds.

rm(list = ls())

library(tidyverse)


# Colony data base ----------------------------------------------------------

colony_db <- tibble(
    # unique bird identifier - 2 first letters of provider, plus 2 numbers
    bird_id = character(),
    # year of tracking associated with the colony
    year = integer(),
    # longitude of computed colony location (highest density of points)
    dens_lon = double(),
    # latitude of computed colony location (highest density of points)
    dens_lat = double(),
    # longitude of closest known colony
    known_lon = double(),
    # latitude of closest known colony
    known_lat = double(),
    # distance between computed and known closest colony (in kilometres)
    dist = double(),
    # code of closest known colony to computed colony
    known_id = character()
)

# Save template
write_rds(colony_db, path = "data/working/colony_db_template.rds")
