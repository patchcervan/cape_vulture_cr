# 16-10-2020

# In this script we plot and process height data from all vultures

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)


# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/in_process", pattern = ".rds")

# Function to extract covariates from rasters
source("R/functions/extractCovts.R")


# Process tracks ----------------------------------------------------------

# for each bird
for(i in 1:length(trk_files)){
   
   trk <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i]))
   
   id_sel <- unique(trk$bird_id)
   print(id_sel)
   
   # Check previous processing steps
   if(length(attr(trk, "move")) == 0){
      print("No move information?")
      next
   }
   
   # Plot altitude data
   trk %>% 
      ggplot(aes(x = datetime, y = alt)) +
      geom_point()
   
   trk %>% 
      ggplot(aes(x = alt)) +
      geom_histogram()
   
   # Create sf object and reproject
   trk <- st_as_sf(trk, coords = c("lon", "lat"), crs = 4326, remove = F)
   
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
      ggplot(aes(x = datetime, y = height)) +
      geom_point(alpha = 0.5) +
      ggtitle(id_sel)
   
   hst <- trk %>% 
      ggplot(aes(x = height)) +
      geom_histogram() +
      ggtitle(id_sel)

   ggsave(filename = paste0("output/height_plots/hgt_ppt_", id_sel,".png"), ppt)
   ggsave(filename = paste0("output/height_plots/hgt_hst_", id_sel,".png"), hst)
   

   # Make corrections --------------------------------------------------------

   # Plot altitude and height
   trk %>% 
      dplyr::select(datetime, alt, height) %>% 
      gather(measure, value, -datetime) %>% 
      ggplot(aes(x = datetime, y = value)) +
      geom_point() +
      facet_wrap("measure", nrow = 2, scales = "free")
   
   # Plot altitude and height
   trk %>% 
      dplyr::select(datetime, alt, height) %>% 
      gather(measure, value, -datetime) %>% 
      ggplot(aes(x = value)) +
      geom_histogram(aes(y = ..density..)) +
      geom_density(colour = "red") +
      facet_wrap("measure", nrow = 2, scales = "free")
   
   # Plot change in height vs. height after the change
   trk %>% 
      mutate(dalt = alt - lag(alt)) %>% 
      ggplot(aes(x = alt, y = dalt)) +
      geom_point()
   
   trk %>% 
      mutate(dheight = height - lag(height)) %>% 
      ggplot(aes(x = height, y = dheight)) +
      geom_point()
   
   trk %>% 
      mutate(dheight = height - lag(height),
             ddheight = dheight - lag(dheight)) %>% 
      ggplot(aes(x = height, y = ddheight)) +
      geom_point()
   
   # Based on the previous plots correct altitude
   trk <- trk %>% 
      mutate(alt_c = case_when(id_sel == "ez01" & height < -500 ~ alt + 2042,
                               id_sel == "ez04" & alt < 900 ~ alt + 2042,
                               id_sel == "ez06" & height < -200 ~ alt + 2042,
                               id_sel == "ez07" & height < -200 ~ alt + 2042,
                               id_sel == "na01" & height < -200 ~ alt + 2042,
                               id_sel == "na02" & height < -200 ~ alt + 2042,
                               id_sel == "na03" & height < -250 ~ alt + 2042,
                               id_sel == "na04" & height < -200 ~ alt + 2042,
                               id_sel == "na05" & height < -200 ~ alt + 2042,
                               id_sel == "na06" & height < -200 ~ alt + 2042,
                               id_sel == "na07" & height < -200 ~ alt + 2042,
                               id_sel == "na08" & alt < 850 ~ alt + 2042,
                               id_sel == "na09" & height < -200 ~ alt + 2042,
                               TRUE ~ alt))
   
   trk %>% 
      ggplot(aes(x = datetime, y = alt_c)) +
      geom_point()
   
   # Substract GPS altitude from ground altitude
   trk <- trk %>% 
      mutate(height = alt_c - srtm0)
   
   # if height was corrected plot the corrected height
   if(id_sel %in% c("ez01", "ez04", "ez06", "ez07", "na01", "na02", "na03", "na04", "na05", "na06", "na07", "na08", "na09")){
      
      ppt <- trk %>% 
         ggplot(aes(x = datetime, y = height)) +
         geom_point(alpha = 0.5) +
         ggtitle(id_sel)
      
      hst <- trk %>% 
         ggplot(aes(x = height)) +
         geom_histogram() +
         ggtitle(id_sel)
      
      ggsave(filename = paste0("output/height_plots/hgt_ppt_", id_sel,"_fix.png"), ppt)
      ggsave(filename = paste0("output/height_plots/hgt_hst_", id_sel,"_fix.png"), hst)
      
   }
   
   # Add attribute height
   attr(trk, "height") <- 1
   
   # Save bird
   saveRDS(trk, file = paste0("data/working/bird_tracks/in_process/", id_sel, ".rds"))
}


# DO NOT RUN:

# Process tracks ----------------------------------------------------------

# for each bird
for(i in 1:length(trk_files)){
   
   trk <- read_csv(paste0("data/working/bird_tracks/fit_ready/", trk_files[i]))
   
   id_sel <- unique(trk$bird_id)
   print(id_sel)
   
   # Check previous processing steps
   if(length(attr(trk, "height")) == 0){
      print("No height information?")
      next
   }
   
   # Plot altitude data
   trk %>% 
      ggplot(aes(x = t_, y = height)) +
      geom_point()
   
   if(id_sel == "ez01"){
      
      trk <- trk %>% 
         mutate(hat_hgt = if_else(height < -300, height + 2042, height))
      
      trk %>% 
         ggplot(aes(x = t_, y = hat_hgt)) +
         geom_point()
      
      
   } else {
      next
   }
   
   
   
   
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
   trk %>% 
      ggplot(aes(x = t_, y = height)) +
      geom_point()
   
   if(id_sel == "ez01"){
      
   }
   
   
   
   # Save bird
   write_csv(trk, file = paste0("data/working/bird_tracks/fit_ready/", id_sel, "_height.csv"))
   
}