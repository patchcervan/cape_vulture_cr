# 15-01-2021

# In this script we will prepare an interactive map to show and navigate colony data

rm(list = ls())

library(tidyverse)
library(leaflet)


# Load colony data --------------------------------------------------------

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

# Create a palette that maps factor levels to colors
pal <- colorFactor(c("red", "purple"), domain = c(0, 1))

col_join %>% 
      leaflet() %>% 
      addProviderTiles(providers$Stamen.Terrain) %>% 
      addCircleMarkers(label = col_join$name,
                       popup = paste("Name: ",col_join$name, "<br>",
                                     "Type: ", col_join$type, "<br>",
                                     "Avg_adults: ", round(col_join$avg_ad), "<br>",
                                     "Old names: ", col_join$names_old, "<br>"),
                       radius = ~case_when(avg_ad <= 20 ~ 4,
                                           avg_ad > 20 & avg_ad <= 100 ~ 6,
                                           avg_ad > 100 & avg_ad <= 1000 ~ 8,
                                           avg_ad > 1000 ~ 10,
                                           TRUE ~ 2),
                       color = pal(col_join$counted), fillOpacity = 0.5,
                       weight = 1, stroke = T, opacity = 1
      )
