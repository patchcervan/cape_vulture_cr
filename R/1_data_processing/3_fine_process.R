# 28-09-2020

# In this script we make fine scale exploration and processing of birds
# The objective is to leave the birds ready for model fitting

rm(list = ls())

library(tidyverse)
library(lubridate)
library(sf)


# Read in data ------------------------------------------------------------

# Read in bird data base
bird_db <- read_csv("data/working/bird_db.csv")

# List all tracking files
trk_files <- list.files("data/working/bird_tracks", pattern = ".rds")

# Load base maps
source("R/functions/load_basemap.R")


# Birds excluded ----------------------------------------------------------

# Create an indicator variable capturing whether birds were selected to
# be included in the analysis based on previous data processing scripts.
# See the file "data/working/problem_birds.txt"
excluded <- c("ct01", "ct11", "ew02", "mb07", "wt05", "wt11", "wt16", "wt19", "wt22")

bird_db <- bird_db %>% 
    mutate(included = case_when(bird_id %in% excluded ~ 0L,
                                TRUE ~ 1L))

# Save updated version of database
write_csv(bird_db, "data/working/bird_db.csv")


# PROCESS BIRDS -----------------------------------------------------------

# Load a bird -----------------------------------------------------------

for(i in 1:79){
    
# choose one track
trk_sel <- read_rds(paste0("data/working/bird_tracks/", trk_files[i])) %>% 
    arrange(datetime)

# bird ID
id_sel <- unique(trk_sel$bird_id)

# Database entry
db_sel <- bird_db %>% 
    filter(bird_id == id_sel)



# General exploration -----------------------------------------------------

db_sel

# Median tracking temporal resolution. This might be very different from the mean
# and probably more appropiate for regularizing data
median(trk_sel$dt, na.rm = T)



# General plots -----------------------------------------------------------

# General map by year
trk_sel %>%
    mutate(year = year(datetime)) %>% 
    group_by(year) %>% 
    ggplot() +
    geom_sf(data = sa_map) +
    geom_point(aes(x = lon, y = lat), alpha = 0.2) +
    facet_wrap(~year)


# Longitude - Latitude profiles
trk_sel %>% 
    dplyr::select(datetime, lat, lon) %>% 
    gather(dim, coord, -datetime) %>% 
    ggplot() +
    geom_point(aes(x = datetime, y = coord)) +
    facet_wrap(~ dim, nrow = 2, scales = "free")

# Go to next if bird is excluded
if(id_sel %in% excluded) next 


# Cut track segment ----------------------------------------------------------

# Define track period.
dx <- lead(trk_sel$lon) - trk_sel$lon

# First date to use in analysis
date_start <- case_when(id_sel == "ct04" ~ trk_sel$datetime[which(dx > 0.01)][1],  # The last index 1 is necessary to force the result to be of length 1
                        id_sel == "ct08" ~ trk_sel$datetime[which(year(trk_sel$datetime) == 2018)][1],
                        id_sel == "ew01" ~ trk_sel$datetime[which(trk_sel$lat > -28)][1],
                        id_sel == "ez02" ~ trk_sel$datetime[which(dx > 0.1)][1],
                        id_sel == "ma07" ~ trk_sel$datetime[which(month(trk_sel$datetime) == 12)][1],
                        id_sel == "mb03" ~ trk_sel$datetime[which(trk_sel$lon > 29.3)][1],
                        id_sel == "na07" ~ trk_sel$datetime[which(year(trk_sel$datetime) == 2006)][1],
                        id_sel == "wt06" ~ trk_sel$datetime[which(trk_sel$lat > -26.4)][1],
                        id_sel == "wt07" ~ trk_sel$datetime[which(trk_sel$lat > -26.4)][1],
                        id_sel == "wt08" ~ as.POSIXct("2008-01-01", format = "%Y-%m-%d"),
                        id_sel == "wt09" ~ as.POSIXct("2007-11-01", format = "%Y-%m-%d"),
                        id_sel == "wt10" ~ trk_sel$datetime[which(trk_sel$lat > -26)][1],
                        id_sel == "wt14" ~ as.POSIXct("2007-03-14", format = "%Y-%m-%d"),
                        id_sel == "wt15" ~ trk_sel$datetime[which(trk_sel$lat > -26)][1],
                        TRUE ~ head(trk_sel$datetime,1))

# Last date to use in analysis
date_end <- case_when(id_sel == "ct04" ~ trk_sel$datetime[which(trk_sel$dt > 500)][1],   # The last index 1 is necessary to force the result to be of length 1
                      id_sel == "ct10" ~ trk_sel$datetime[which(trk_sel$lat > -26) - 1][1],
                      id_sel == "ct11" ~ trk_sel$datetime[which(trk_sel$lat > 0) - 1][1],
                      id_sel == "ez09" ~ as.POSIXct("2017-11-08", format = "%Y-%m-%d"),
                      id_sel == "ma15" ~ as.POSIXct("2017-11-01", format = "%Y-%m-%d"),
                      id_sel == "wt06" ~ trk_sel$datetime[800],
                      id_sel == "wt12" ~ as.POSIXct("2008-02-15", format = "%Y-%m-%d"),
                      id_sel == "wt13" ~ as.POSIXct("2007-08-01", format = "%Y-%m-%d"),
                      id_sel == "wt14" ~ as.POSIXct("2007-08-01", format = "%Y-%m-%d"),
                      TRUE ~ tail(trk_sel$datetime, n = 1L))


# Subset dates. Remove UTM coordinates to avoid confusion
trk_sel <- trk_sel %>% 
    filter(datetime >= date_start, datetime <= date_end) %>%
    dplyr::select(-c("x", "y"))


# Redefine age ------------------------------------------------------------

# If a bird has been tracked for a long time, locations in later years might need to be
# re-classified to older age classes
trk_sel <- trk_sel %>% 
    mutate(age = db_sel$age)

if(id_sel == "ct06"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2018 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ez01"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) %in% c(2015:2016) ~ "subad",
                               year(datetime) %in% c(2017:2018) ~ "ad",
                               TRUE ~ .$age))
} else if(id_sel == "ez06"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2015 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ez08"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2019 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ma03"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2018 ~ "subad", TRUE ~ .$age))
} else if(id_sel == "ma16"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2019 ~ "subad", TRUE ~ .$age))
} else if(id_sel == "mb01"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) == 2019 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "na06"){
    trk_sel <- trk_sel %>% 
        mutate(age = case_when(year(datetime) %in% c(2008,2009) ~ "subad",
                               year(datetime) == 2010 ~ "ad",
                               TRUE ~ .$age))
}

# Save resampled track ----------------------------------------------------

saveRDS(trk_sel, paste0("data/working/bird_tracks/keep/", id_sel, "_fine.rds"))

}
