# 16-10-2020

# In this script we process height data from all vultures selected from the data base to
# fit a hierarchical Ornstein-Uhlenbeck process model to the height readings

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)


# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/fit_ready", pattern = ".csv")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Function to extract covariates from rasters
source("R/functions/extractCovts.R")

# Process tracks ----------------------------------------------------------

# for each bird
for(i in 1:length(trk_files)){
   
   # i = 1
   
   trk <- read_csv(paste0("data/working/bird_tracks/fit_ready/", trk_files[i]))
   
   id_sel <- unique(trk$bird_id)
   print(id_sel)
   
   # Plot altitude data
   trk %>% 
      ggplot(aes(x = t_, y = alt)) +
      geom_point()
   
   trk %>% 
      ggplot(aes(x = alt)) +
      geom_histogram()
   
   
   # Create sf object and reproject
   trk <- st_as_sf(trk, coords = c("x_", "y_"), crs = 4326, remove = F)
   
   # Extract topographical covariates
   trk <- trk %>% 
      extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R",
                   extractCovtPath = "R/functions/extractCovt.R",
                   covts_path = "data/working/covts_rasters",
                   covts_names = "srtm0",
                   return_sf = FALSE, extract_method = "merge")
   
   # Substract GPS altitude from ground altitude
   trk <- trk %>% 
      mutate(height = alt - srtm0)
   
   # Plot height data
   ppt <- trk %>% 
      ggplot(aes(x = t_, y = height)) +
      geom_point(alpha = 0.5) +
      ggtitle(id_sel)
   
   hst <- trk %>% 
      ggplot(aes(x = height)) +
      geom_histogram() +
      ggtitle(id_sel)

   ggsave(filename = paste0("figures/height_plots/hgt_ppt_", id_sel,".png"), ppt)
   ggsave(filename = paste0("figures/height_plots/hgt_hst_", id_sel,".png"), hst)
}