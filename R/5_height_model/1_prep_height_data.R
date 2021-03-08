# 20-01-2021

# In this script we annotate tracking data with covariates to fit a model to height

# Note that distance to colonies and supplementary feeding sites have already
# been calculated in previous scripts

rm(list = ls())

library(tidyverse)
library(sf)
library(lubridate)
library(furrr)
library(raster)


# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Load protected areas
pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")

# Read in colony data
colony_db <- read_csv("data/working/colony_db.csv")


# Load functions ----------------------------------------------------------

# Function to extract covariates from rasters
source("R/functions/extractCovts.R")


# Process birds -----------------------------------------------------------

for(i in 1:length(trk_files)){
   
   # Read in one bird
   trk_sel <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i])) %>% 
      arrange(datetime)
   
   # bird ID
   id_sel <- unique(trk_sel$bird_id)
   
   print(c(i, id_sel))
   
   # Check if track has been processed or it was excluded early on
   if(bird_db[bird_db$bird_id == id_sel, "keep_loc"] == 0 |
      bird_db[bird_db$bird_id == id_sel, "keep_hgt"] == 0){
      print("This bird was excluded")
      next
   }
   
   # Take only locations that correspond to a moving state
   trk_sel <- trk_sel %>% 
      filter(state == 2)
   
   # Make spatial object and find UTM projection
   trk_sel <- st_as_sf(trk_sel, coords = c("lon", "lat"), crs = 4326, remove = F)

   
   # Colony zone -------------------------------------------------------------
   
   # Classify birds according to where their colony for the year is
   trk_sel <- trk_sel %>% 
      mutate(year = lubridate::year(datetime)) %>% 
      left_join(dplyr::select(colony_db, bird_id, year, zone), by = c("bird_id", "year"))
   
   
   # Extract covariates from rasters -----------------------------------------
   
   years <- unique(trk_sel$year)
   
   # Nest and extract land use class for the different years
   trk_sel <- trk_sel  %>%
      nest(data = -year) %>% 
      mutate(year = case_when(year < 2015 ~ 2015,
                              year > 2019 ~ 2019,
                              TRUE ~ year)) %>%     # land use maps only start in 2015 and ends in 2019
      mutate(data = future_map2(.$data, .$year, ~ extractCovts(.x,
                                                               loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                                               extractCovtPath = "R/functions/extractCovt.R",
                                                               covts_path = "data/working/covts_rasters/copernicus",
                                                               covts_names = paste0(c("LC100_global_v3.0.1_"), .y),
                                                               return_sf = FALSE, extract_method = "merge"))) %>% 
      mutate(data = future_map(.$data, ~rename(.x, land_use = which(str_detect(names(.), "LC100"))))) %>% 
      mutate(year = years) %>%    # Return years back to their original values
      unnest(cols = c(year, data)) 
   
   
   # Nest and extract NDVI for the different years
   trk_sel <- trk_sel  %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
      nest(data = -year) %>% 
      mutate(data = future_map2(.$data, .$year, ~ extractCovts(.x,
                                                               loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                                               extractCovtPath = "R/functions/extractCovt.R",
                                                               covts_path = "data/working/covts_rasters/modis",
                                                               covts_names = paste0(c("NDVI_doy"), .y),
                                                               return_sf = FALSE, extract_method = "stack")))
   
   # Calculate mean NDVI for the year and remove monthly measurements
   # (Careful, means computed for each year separately!)
   trk_sel <- trk_sel  %>%
      mutate(NDVI_mean = future_map(.$data, ~dplyr::select(.x, which(str_detect(names(.x), "NDVI"))) %>%
                                       transmute(., NDVI_mean = rowMeans(., na.rm = T)))) %>% 
      unnest(cols = c(year, data, NDVI_mean )) %>% 
      dplyr::select(which(!str_detect(names(.), "NDVI_doy")))
   
   # Extract topographical covariates
   trk_sel <- trk_sel %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>% 
      extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R",
                   extractCovtPath = "R/functions/extractCovt.R",
                   covts_path = "data/working/covts_rasters",
                   covts_names = c("srtm0", "slope", "vrm3", "dist_slp_m"),
                   return_sf = FALSE, extract_method = "merge")
   
   
   # Extract protected areas -------------------------------------------------
   
   # Calculate intersection between locations and protected areas
   pa_int <- trk_sel %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
      st_intersects(pa, sparse = FALSE)
   
   # Create a protected areas covariate
   trk_sel <- trk_sel %>%
      mutate(prot_area = if_else(pa_int == FALSE, 0, 1))
   
   # Remove intersection
   rm(pa_int)
   
   
   # Create time of day variable ---------------------------------------------
   
   trk_sel <- trk_sel %>% 
      mutate(hourday = lubridate::hour(datetime),
             ttnoon = hourday - 10,
             ttnoon_sq = (ttnoon)^2)
   
   # Create also a time resolution variable
   # Recalculate dt in hours
   trk_sel <- trk_sel %>% 
      mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours")))
   
   meandt <- mean(trk_sel$dt, na.rm = T)
   
   trk_sel <- trk_sel %>% 
      mutate(res = case_when(meandt > 0.5 & meandt <= 1.5 ~ 1,
                             meandt > 1.5 & meandt <= 2.5 ~ 2,
                             meandt > 2.5 & meandt <= 3.5 ~ 3,
                             meandt > 3.5 & meandt <= 5 ~ 4,
                             meandt > 5 & meandt <= 12 ~ 8,
                             TRUE ~ 24))
   
   # Export bird ------------------------------------------------------------
   
   saveRDS(trk_sel, paste0("data/working/bird_tracks/fit_ready/height/", id_sel,".rds"))
   
}


# Create a unique data frame with all data --------------------------------

rm(list = ls())

# create a data frame to store results
model_data <- data.frame()

trkfiles <- list.files("data/working/bird_tracks/fit_ready/height")

# Exclude test files
trkfiles <- trkfiles[!str_detect(trkfiles, "test")]


for(i in 1:length(trkfiles)){
   trk <- readRDS(paste0("data/working/bird_tracks/fit_ready/height/", trkfiles[i]))
   model_data <- rbind(model_data, trk)
   print(i)
}


# Prepare variables -------------------------------------------------------

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv")

# Change habitat codes
model_data <- model_data %>%
   left_join(dplyr::select(hab_meta, "Map code", "class_code"), by = c("land_use" = "Map code")) %>%
   rename(land_cov = class_code)

# There are a few NA NDVI corresponding mostly to locations over the ocean.
# I will assign the mean NDVI value
model_data %>%
   filter(is.na(NDVI_mean)) %>%
   group_by(land_cov) %>%
   summarize(n = n())

model_data <- model_data %>%
   mutate(NDVI_mean = if_else(is.na(NDVI_mean), mean(NDVI_mean, na.rm = T), NDVI_mean))

# Make dummy variables from factors (I also keep the factors)
model_data <- model_data %>%
   mutate(i = 1,
          land_cov_fct = land_cov) %>%
   spread(land_cov, i, fill = 0) %>%
   mutate(i = 1,
          zone = factor(zone, levels = 1:4, labels = paste("z", 1:4, sep = "_")),
          zone_fct = zone) %>%
   spread(zone, i, fill = 0) %>%
   mutate(i = 1,
          res = factor(res, levels = c(1, 2, 3, 4, 8, 24), labels = paste("res", c(1, 2, 3, 4, 8, 24), sep = "_")),
          res_fct = res) %>%
   spread(res, i, fill = 0) %>%
   mutate(i = 1,
          age = factor(age, levels = c("juv", "subad", "ad")),
          age_fct = age) %>%
   spread(age, i, fill = 0)


# Save data ---------------------------------------------------------------

saveRDS(model_data, "data/working/data_height_ready.rds")
