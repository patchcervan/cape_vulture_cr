# 17-01-2021

rm(list = ls())

library(tidyverse)

# In this script we prepare the data for the visualization app

# Load data
col_counts <- read_csv("data/working/col_count_summary.csv")
col_locs_all <- read_csv("data/working/colony_data_join.csv")

# Remove counts without coordinates
col_counts <- col_counts %>% 
      filter(!is.na(lon))

# Remove locations that have been counted from the locs data 
# (alredy present in the count data)
col_locs <- col_locs_all %>% 
      filter(counted == 0)

# Join both locations and locations with counts
col_join <- rbind(
      col_locs %>% 
            mutate(avg_ad = NA, avg_juv = NA, names_old = NA, counted = 0,
                   type = if_else(type %in% c("Roost", "Roosting"), "roost", "breed")) %>% 
            dplyr::select(name, lon, lat, avg_ad, avg_juv, names_old, counted, type),
      col_counts %>% 
            dplyr::select(name = name_new, lon, lat, avg_ad, avg_juv, names_old) %>% 
            mutate(counted = 1, type = "breed")
)

# Replace any non UTF-8 in colony names by '#'
col_join <- col_join %>% 
      mutate(name = iconv(name, "UTF-8", "UTF-8", sub = '#'))

# Save data
saveRDS(col_join, "R/show/colony_viz/col_viz_data.rds")
