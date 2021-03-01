# 23-02-2021

# In this script we regularize tracking data to prepare a test data set for the
# step selection function model

rm(list = ls())

library(tidyverse)
library(sf)
library(amt)
library(lubridate)
library(furrr)
library(Rcpp)


# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Read in colony data
colony_db <- read_csv("data/working/colony_db.csv")

# Read in roost and colony data
colony_orig <- read_csv("data/working/colony_data_all_upt.csv")

# Extract roosts and fix names
roost <- colony_orig %>% 
   filter(str_detect(type, "roost"))

# Load protected areas
pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")

# Load restaurant data
sfs <- read_csv("data/working/restaurants.csv")


# Load functions ----------------------------------------------------------

# Function to define projection for each data frame
source("R/functions/makeTmerProj.R")

# Function to calculate minimum distance
sourceCpp("R/functions/minDist_cpp.cpp")

# Function to extract covariates from rasters
source("R/functions/extractCovts.R")

# Read in base map elements
source("R/functions/load_basemap.R")

# Ignore warnings because these are auxiliary maps - precision is not important now.


# Process birds -----------------------------------------------------------

for(i in 1:length(trk_files)){
   
   # Read in one bird
   trk_sel <- readRDS(paste0("data/working/bird_tracks/in_process/", trk_files[i])) %>% 
      arrange(datetime)
   
   # bird ID
   id_sel <- unique(trk_sel$bird_id)
   
   print(c(i, id_sel))
   
   # Check if track has been processed or it was excluded early on
   if(bird_db[bird_db$bird_id == id_sel, "keep_loc"] == 0){
      print("This bird was excluded")
      next
   }
   
   # Define the variables that we will use
   trk_sel <- trk_sel %>% 
      dplyr::select(1:19)
   
   # Define tracking resolution (sampling rate) and tolerance in hours and minutes respectively
   # We set a very coarse resolution for the validation set to avoid autocorrelation
   trk_res <- case_when(id_sel == "ez02" ~ 24,
                        TRUE ~ 8)
   
   trk_tol <- case_when(id_sel == "ez02" ~ 120,
                        TRUE ~ 40)
   
   # Make amt object
   trk_xyt <- trk_sel %>% 
      make_track(lon, lat, datetime, all_cols = T, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
   
   # Resample
   new_trk <- trk_xyt %>% 
      track_resample(rate = hours(trk_res), tolerance = minutes(trk_tol))
   
   unique(year(new_trk$t_))
   
   # # Remove repeated observations if there were
   # if(nrow(distinct(new_trk, x_, y_)) != nrow(new_trk)){
   #    warning(paste("Removing repeated observations! In", unique(new_trk$bird_id)))
   #    new_trk <- distinc(new_trk, x_, y_, .keep_all = TRUE)
   # }
   
   # Nest by year and produce random points for each year
   use_rdm <- new_trk %>% 
      # as.data.frame() %>% 
      mutate(year = year(t_)) %>% 
      nest(data = -c(bird_id, year)) %>%  
      mutate(rp = map(.$data, ~random_points(.x)))
   

   
   # Add random points to the main data frame
   use_rdm <- use_rdm %>% 
      mutate(data = map2(use_rdm$data, use_rdm$rp, ~left_join(.y, .x, by = c("x_", "y_")))) %>% 
      dplyr::select(-rp)
   
   # Define projected coordinate system
   tmerproj <- makeTmerProj(st_as_sf(new_trk, coords = c("x_", "y_"), crs = 4326))
   
   # At this point I need to save the attributes so that they are not lost later in the process
   atts <- attributes(new_trk)
   
   
   # Find distance to colony -------------------------------------------------
   
   # Find colonies used by the bird, make spatial object, and nest by year
   colony <- colony_db %>% 
      filter(bird_id == id_sel) %>% 
      st_as_sf(coords = c("dens_lon", "dens_lat"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj) %>% 
      nest(data = -year)
   
   # Classify birds according to where their colony for the year is
   use_rdm <- use_rdm %>% 
      left_join(dplyr::select(colony_db, bird_id, year, zone), by = c("bird_id", "year"))
   
   # filtering bursts might result in colony years and track years not matching
   years <- unique(use_rdm$year)
   
   colony <- colony %>% 
      filter(year %in% years)
   
   # for each year's colony find distance to tracking locations
   use_rdm <- use_rdm %>% 
      mutate(data = map(.$data, ~st_as_sf(.x, coords = c("x_", "y_"), crs = tmerproj, remove = F))) 
   
   use_rdm <- use_rdm %>%
      mutate(dist_col = future_map2(.$data, colony$data, ~as.numeric(st_distance(.x, .y))))
   
   
   # Find distance to any colony or roost ------------------------------------
   
   # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
   colony_orig <- st_as_sf(colony_orig, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj)
   
   # Then, we need to remove any colony or roost that is less than 10km away from the central colony
   col_sel <- future_map(colony$data, ~st_distance(.x, colony_orig)) %>% 
      # add distance from central colony (for each year)
      future_map(~mutate(colony_orig,
                         dist_from_central = .x[1,])) %>% 
      # remove those that are closer than 10km
      future_map( ~filter(.x, as.numeric(dist_from_central) > 1e4))
   
   # For each year calculate distance between locations and any colony or roost
   # considering only those selected roosts and colonies that are further than 10km
   # from central colony
   use_rdm <- use_rdm %>% 
      mutate(dist_col_any = future_map2(.$data, col_sel,
                                        ~minDist_cpp(st_coordinates(st_as_sf(.)),
                                                     st_coordinates(.y))))
   

   # Find distance to restaurants --------------------------------------------
   
   # To create a matrix of coordinates from the sfs layer, we need to create a spatial object
   sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj)
   
   # Then, we need to remove any sfs that is less than 10km away from the central colony
   sfs_sel <- future_map(colony$data, ~st_distance(.x, sfs)) %>% 
      # add distance from central colony (for each year)
      future_map(~mutate(sfs,
                         dist_from_central = .x[1,])) %>% 
      # remove those that are closer than 10km
      future_map( ~filter(.x, as.numeric(dist_from_central) > 1e4))
   
   # For each year calculate distance between locations and any sfs
   # considering only those selected sfs that are further than 10km
   # from central colony
   use_rdm <- use_rdm %>% 
      mutate(dist_sfs = future_map2(.$data, sfs_sel,
                                    ~minDist_cpp(st_coordinates(st_as_sf(.)),
                                                 st_coordinates(.y))))
   
   
   # Extract covariates from rasters -----------------------------------------
   
   # Unnest data
   use_rdm <- unnest(use_rdm, cols = names(use_rdm))
   
   # For whatever reason unnest is not producing a dataframe. Fix
   reassemble <- function(l){
      out <- as.tibble(l[[1]])
      for(i in 2:length(l)){
         out <- cbind(out,l[[i]])
      }
      names(out) <- names(l)
      return(out)
   }
   
   use_rdm <- reassemble(use_rdm)

   # Recover spatial object and transform back to geographic coordinates
   use_rdm <- use_rdm %>% 
      st_as_sf(coords = c("x_", "y_"), crs = 4326, remove = F)
   
   # Nest and extract land use class for the different years
   use_rdm <- use_rdm  %>%
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
   use_rdm <- use_rdm  %>%
      st_as_sf(coords = c("x_", "y_"), crs = 4326, remove = FALSE) %>% 
      nest(data = -year) %>% 
      mutate(data = future_map2(.$data, .$year, ~ extractCovts(.x,
                                                               loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                                               extractCovtPath = "R/functions/extractCovt.R",
                                                               covts_path = "data/working/covts_rasters/modis",
                                                               covts_names = paste0(c("NDVI_doy"), .y),
                                                               return_sf = FALSE, extract_method = "stack")))
   
   # Calculate mean NDVI for the year and remove monthly measurements
   # (Careful, means computed for each year separately!)
   use_rdm <- use_rdm  %>%
      mutate(NDVI_mean = future_map(.$data, ~dplyr::select(.x, which(str_detect(names(.x), "NDVI"))) %>%
                                       transmute(., NDVI_mean = rowMeans(., na.rm = T)))) %>% 
      unnest(cols = c(year, data, NDVI_mean )) %>% 
      dplyr::select(which(!str_detect(names(.), "NDVI_doy")))
   
   
   # Extract topographical covariates
   use_rdm <- use_rdm %>%
      st_as_sf(coords = c("x_", "y_"), crs = 4326, remove = FALSE) %>% 
      extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R",
                   extractCovtPath = "R/functions/extractCovt.R",
                   covts_path = "data/working/covts_rasters",
                   covts_names = c("srtm0", "slope", "vrm3", "dist_slp_m"),
                   return_sf = FALSE, extract_method = "merge")
   
   
   # Extract protected areas -------------------------------------------------
   
   # Calculate intersection between locations and protected areas
   pa_int <- use_rdm %>%
      st_as_sf(coords = c("x_", "y_"), crs = 4326, remove = FALSE) %>%
      st_intersects(pa, sparse = FALSE)
   
   # Create a protected areas covariate
   use_rdm <- use_rdm %>%
      mutate(prot_area = if_else(pa_int == FALSE, 0, 1))
   
   # Remove intersection
   rm(pa_int)
   
   
   # Add bird to data frame --------------------------------------------------
   
   # # Recover attributes (e.g. step-length and angle distributions)
   # atts$names <- attr(use_rdm, "names")
   # attributes(use_rdm) <- atts
   
   # Save track
   saveRDS(use_rdm, paste0("data/working/bird_tracks/fit_ready/ssf/test_", id_sel,".rds"))
   
}


# Create a unique data frame with all data --------------------------------

rm(list = ls())

# create a data frame to store results
model_data <- data.frame()

trkfiles <- list.files("data/working/bird_tracks/fit_ready/ssf", pattern = "test")

for(i in 1:length(trkfiles)){
   trk <- readRDS(paste0("data/working/bird_tracks/fit_ready/ssf/", trkfiles[i]))
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

# Need to correct age for random points
age_df <- model_data %>% 
   filter(case_ == TRUE) %>% 
   group_by(bird_id, year) %>% 
   summarize(age = unique(age))

age_false <- left_join(filter(model_data, case_ == FALSE),
                       age_df, by = c("bird_id", "year"))

age_false <- age_false %>% 
   mutate(age.x = age.y) %>% 
   rename(age = age.x) %>% 
   dplyr::select(-age.y)

model_data <- rbind(age_false,
                    filter(model_data, case_ == TRUE))

# Make dummy variables from factors (I also keep the factors)
model_data <- model_data %>%
   mutate(id = row_number()) %>% 
   mutate(i = 1,
          land_cov_fct = land_cov) %>%
   spread(land_cov, i, fill = 0) %>%
   mutate(i = 1,
          zone = factor(zone, levels = 1:4, labels = paste("z", 1:4, sep = "_")),
          zone_fct = zone) %>%
   spread(zone, i, fill = 0) %>%
   mutate(i = 1,
          age = factor(age, levels = c("juv", "subad", "ad")),
          age_fct = age) %>%
   spread(age, i, fill = 0)


# Save data ---------------------------------------------------------------

saveRDS(model_data, "data/working/data_ssf_test.rds")
