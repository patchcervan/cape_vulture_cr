# 14-09-2020

# In this script we fix errors in individual birds that were picked up
# during the analysis

library(tidyverse)
library(lubridate)

rm(list = ls())


# Read in database --------------------------------------------------------

db <- read_csv("data/working/bird_db.csv")


# Select birds ------------------------------------------------------------

bird <- "ct11"

trk <- read_csv(paste0("data/working/bird_tracks/", bird, ".csv"))

trk %>% 
    ggplot() +
    geom_point(aes(x = lon, y = lat))

trk %>% 
    dplyr::select(lon, lat, datetime) %>% 
    gather(dim, coord, -datetime) %>% 
    ggplot() +
    geom_point(aes(x = datetime, y = coord)) +
    facet_grid(dim ~., scales = "free")
