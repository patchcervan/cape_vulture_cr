library(tidyverse)
library(sf)
library(lubridate)
library(amt)

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
      
      # Make spatial object
      trk <- st_as_sf(trk, coords = c("lon", "lat"), crs = 4326, remove = F)
      
      # Define projection
      tmerproj <- makeTmerProj(trk)
      
      # Transform to projected coordinate system
      trk <- st_transform(trk, crs = tmerproj)
      
      # Make amt track
      trk <- trk %>% 
         mutate(x = st_coordinates(.)[,1],
                y = st_coordinates(.)[,2]) %>% 
         make_track(x, y, datetime, all_cols = T, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
      
      # Resample and keep only those bursts that allow step and angle calculation
      new_trk <- trk %>% 
         track_resample(rate = hours(24), tolerance = minutes(120)) %>% 
         filter_min_n_burst(min_n = 3)
      
      # Create steps object
      new_trk <- new_trk %>% 
         dplyr::select(x_, y_, t_, bird_id, age, burst_) %>% 
         steps_by_burst(keep_cols = 'end')
      
      # Summarize step-lengths
      dist_df <- rbind(dist_df,
                       new_trk %>% 
                          group_by(bird_id,age) %>% 
                          summarize(avg_km = mean(sl_/1000, na.rm = T),
                                    sd_km = sd(sl_/1000, na.rm = T),
                                    avg_dt = mean(dt_, na.rm = T),
                                    sd_dt = sd(dt_, na.rm = T)))
      
}      


dist_df %>% 
      ggplot() +
      geom_point(aes(x = avg_km, y = bird_id, colour = age)) +
      theme(axis.text.y = element_text(size = 8))


dist_df %>% 
   ggplot() +
   geom_histogram(aes(avg_km, ..density..), colour = "black", fill = "white") +
   geom_density(aes(x = avg_km))
ggsave("output/avg_dist_day_hist.png")

dist_df %>% 
      ggplot() +
      geom_histogram(aes(avg_km, ..density..), colour = "black", fill = "white") +
      geom_density(aes(x = avg_km)) +
      facet_wrap("age", nrow = 2)
ggsave("output/avg_dist_day_age_hist.png")      

dist_df %>% 
   arrange(avg_km) %>%
   mutate(bird_id = factor(bird_id)) %>% 
   ggplot() +
   geom_pointrange(aes(x = avg_km, y = reorder(bird_id, avg_km),
                       xmin = avg_km - sd_km, xmax = avg_km + sd_km,
                       colour = age)) +
   ylab("Bird ID") + xlab("Avg km per day") + 
   theme(axis.text.y = element_text(size = 8))
ggsave("output/avg_dist_day_pt.png") 

dist_df %>% 
   filter(avg_km < 10)
