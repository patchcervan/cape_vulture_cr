# 28-09-2020

# In this script we make fine scale exploration and processing of birds
# The objective is to leave the birds ready for model fitting

# Individual corrections to birds are made in the script ind_corrections.R

rm(list = ls())

library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(amt)


# Read in one bird --------------------------------------------------------

# Read in bird data base
bird_db <- read_csv("data/working/bird_db.csv")

# Fix dates. We should consider doing this in the original processing scripts
bird_db <- bird_db %>% 
    mutate(date_start = as.POSIXct(date_start, format = "%m/%d/%Y"),
           date_end = as.POSIXct(date_end, format = "%m/%d/%Y"))

# List all tracking files
trk_files <- list.files("data/working/bird_tracks", pattern = ".csv")

for(i in 1:22){
    
# choose one track
i <- 79

trk_sel <- read_csv(paste0("data/working/bird_tracks/", trk_files[i])) %>% 
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

source("R/functions/load_basemap.R")

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


# Birds excluded ----------------------------------------------------------

# Create an indicator variable capturing whether birds were selected to
# be included in the analysis based on previous data processing scripts
excluded <- c("ct01", "ct11", "ew02", "mb07", "wt05", "wt11", "wt16", "wt19", "wt22")

if(id_sel %in% excluded){
    
    bird_db <- bird_db %>% 
        mutate(included = case_when(bird_id %in% excluded ~ 0L,
                                    TRUE ~ 1L))
    
    # Save updated version of database
    write_csv(bird_db, "data/working/bird_db.csv")
    
    # Go to next bird in the loop
    next
} 


# Resample track ----------------------------------------------------------

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


# Define tracking resolution (sampling rate) and tolerance in hours and minutes respectively
trk_res <- case_when(id_sel == "ez02" ~ 24,
                     id_sel == "ez05" ~ 8,
                     id_sel == "kr01" ~ 2,
                     id_sel == "ma07" ~ 3,
                     id_sel %in% c("mt01", "mt02", "na07", "na08", "na09") ~ 2,
                     id_sel %in% c("wt01", "wt02", "wt03", "wt17", "wt18", "wt20", "wt21") ~ 4,
                     id_sel %in% c("wt04", "wt06", "wt07", "wt08", "wt09", "wt10", "wt12", "wt13", "wt14", "wt15") ~ 24,
                     TRUE ~ 1)

trk_tol <- case_when(id_sel == "ez02" ~ 120,
                     id_sel == "ez05" ~ 40,
                     id_sel == "kr01" ~ 15,
                     id_sel == "ma07" ~ 20,
                     id_sel %in% c("mt01", "mt02", "na07", "na08", "na09") ~ 15,
                     id_sel %in% c("wt01", "wt02", "wt03", "wt17", "wt18", "wt20", "wt21") ~ 20,
                     id_sel %in% c("wt04", "wt06", "wt07", "wt08", "wt09", "wt10", "wt12", "wt13", "wt14", "wt15") ~ 120,
                     TRUE ~ 10)

# Subset dates and make amt object. Remove UTM coordinates and dt to avoid confusion
trk_xyt <- trk_sel %>% 
    filter(datetime >= date_start, datetime <= date_end) %>% 
    make_track(lon, lat, datetime, all_cols = T, crs = CRS("+proj=longlat +datum=WGS84 +no_defs")) %>% 
    dplyr::select(-c("x", "y", "dt"))

# Resample and keep only those bursts that allow step and angle calculation
new_trk <- trk_xyt %>% 
    track_resample(rate = hours(trk_res), tolerance = minutes(trk_tol)) %>% 
    filter_min_n_burst(min_n = 3)

# Basic checks
any(duplicated(new_trk$t_))

# General map
new_trk %>%
    mutate(year = year(t_)) %>% 
    group_by(year) %>% 
    ggplot() +
    geom_sf(data = sa_map) +
    geom_point(aes(x = x_, y = y_), alpha = 0.2) +
    facet_wrap(~year)

# Longitude - Latitude profiles
new_trk %>% 
    dplyr::select(t_, x_, y_) %>% 
    gather(dim, coord, -t_) %>% 
    ggplot() +
    geom_point(aes(x = t_, y = coord)) +
    facet_wrap(~ dim, nrow = 2, scales = "free")


# Redefine age ------------------------------------------------------------

# If a bird has been tracked for a long time, locations in later years might need to be
# re-classified to older age classes
new_trk <- new_trk %>% 
    mutate(age = db_sel$age)

if(id_sel == "ct06"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2018 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ez01"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) %in% c(2015:2016) ~ "subad",
                               year(t_) %in% c(2017:2018) ~ "ad",
                               TRUE ~ .$age))
} else if(id_sel == "ez06"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2015 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ez08"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2019 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "ma03"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2018 ~ "subad", TRUE ~ .$age))
} else if(id_sel == "ma16"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2019 ~ "subad", TRUE ~ .$age))
} else if(id_sel == "mb01"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) == 2019 ~ "ad", TRUE ~ .$age))
} else if(id_sel == "na06"){
    new_trk <- new_trk %>% 
        mutate(age = case_when(year(t_) %in% c(2008,2009) ~ "subad",
                               year(t_) == 2010 ~ "ad",
                               TRUE ~ .$age))
}

# Save resampled track ----------------------------------------------------

write_csv(new_trk, paste0("data/working/bird_tracks/fit_ready/", id_sel, "_mod.csv"))

}
