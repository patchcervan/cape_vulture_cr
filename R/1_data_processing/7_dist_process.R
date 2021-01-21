library(tidyverse)
library(sf)
library(lubridate)

rm(list= ls())


# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/in_process", pattern = ".rds")

# Load data base
bird_db <- read_csv("data/working/bird_db.csv")

# Load function to create UTM projection
source("R/functions/makeTmerProj.R")


# Process tracks ----------------------------------------------------------

# Create a data frame with traveled distances for all birds
dist_df <- data.frame()


# for each bird
for(i in 1:length(trk_files)){
      
      trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i]))
      
      id_sel <- unique(trk$bird_id)
      print(id_sel)
      
      # Check previous processing steps
      if(length(attr(trk, "height")) == 0){
            print("No height information?")
            next
      }
      
      # Make spatial object and reproject
      trk <- st_as_sf(trk, coords = c("lon", "lat"), crs = 4326, remove = F)
      
      tmerproj <- makeTmerProj(trk)
      
      trk <- st_transform(trk, crs = tmerproj)
      
      # calculate distance traveled
      cc <- st_coordinates(trk)
      
      trk <- trk %>% 
            mutate(dx = lead(cc[,1]) - cc[,1],
                   dy = lead(cc[,2]) - cc[,2],
                   dist = sqrt(dx^2 + dy^2))
      
      dist_df <- rbind(dist_df,
                       trk %>% 
                             st_drop_geometry() %>% 
                             dplyr::select(bird_id, datetime, dt, age, dist))
      
}      

dist_summ <- dist_df %>%
      mutate(day = date(datetime)) %>% 
      group_by(bird_id, age, day) %>% 
      summarize(sum_dist = sum(dist, na.rm = T)) %>% 
      group_by(bird_id, age) %>% 
      summarize(avg_dist = mean(sum_dist, na.rm = T)/1000)

dist_summ %>% 
      ggplot() +
      geom_point(aes(x = avg_dist, y = bird_id, colour = age)) +
      theme(axis.text.y = element_text(size = 8))

dist_summ %>% 
   ggplot() +
   geom_histogram(aes(avg_dist, ..density..), colour = "black", fill = "white") +
   geom_density(aes(x = avg_dist))

dist_summ %>% 
      ggplot() +
      geom_histogram(aes(avg_dist, ..density..), colour = "black", fill = "white") +
      geom_density(aes(x = avg_dist)) +
      facet_wrap("age", nrow = 2)
      

