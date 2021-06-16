# 20-01-2021

# In this script we regularize tracking data to fit a step selection function model

rm(list = ls())

library(tidyverse)
library(sf)
library(amt)
library(lubridate)
library(furrr)
library(Rcpp)

sf_use_s2(FALSE) # s2 throws error when intersecting protected areas

# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/in_process/", pattern = ".rds")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Read in colony data
colony_db <- read_csv("data/working/colony_db.csv")

# Read in roost and colony data
colony_orig <- read_csv("data/working/colony_all_w_da.csv")

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
   
   # Certain birds were excluded
   if(id_sel %in% c("ct08", "ct09")){
      print("This bird was excluded")
      next
   }
   
   # We removed the sub-adult age therefore they should become juveniles
   trk_sel <- trk_sel %>% 
      mutate(age = if_else(age == "subad", "juv", age))
   
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
   
   # Make amt object
   trk_xyt <- trk_sel %>% 
      make_track(lon, lat, datetime, all_cols = T, crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
   
   # Resample and keep only those bursts that allow step and angle calculation
   new_trk <- trk_xyt %>% 
      track_resample(rate = hours(trk_res), tolerance = minutes(trk_tol)) %>% 
      filter_min_n_burst(min_n = 3)
   
   # Basic checks
   print(any(duplicated(new_trk$t_)))
   
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
   
   
   # Generate random steps --------------------------------------------------
   
   # Transform to projected coordinate system
   tmerproj <- makeTmerProj(st_as_sf(new_trk, coords = c("x_", "y_"), crs = 4326))
   
   new_trk <- new_trk %>% 
      transform_coords(crs_to = tmerproj)
   
   # Create steps object
   new_trk <- new_trk %>% 
      dplyr::select(x_, y_, t_, bird_id, age, burst_) %>% 
      steps_by_burst(keep_cols = 'end')
   
   use_rdm <- new_trk %>% 
      random_steps(n_control = 7)
   
   # At this point I need to save the attributes so that they are not lost later in the process
   atts <- attributes(use_rdm)
   
   
   # Find distance to colony -------------------------------------------------
   
   # Find colonies used by the bird, make spatial object, and nest by year
   colony <- colony_db %>% 
      filter(bird_id == id_sel) %>% 
      st_as_sf(coords = c("dens_lon", "dens_lat"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj) %>% 
      nest(data = -year)
   
   # Classify birds according to where their colony for the year is
   use_rdm <- use_rdm %>% 
      mutate(year = lubridate::year(t2_)) %>% 
      left_join(dplyr::select(colony_db, bird_id, year, zone), by = c("bird_id", "year"))
   
   # filtering bursts might result in colony years and track years not matching
   years <- unique(use_rdm$year)
   
   colony <- colony %>% 
      filter(year %in% years)
   
   # for each year's colony find distance to tracking locations
   use_rdm <- use_rdm %>% 
      st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = F) %>% 
      nest(data = -year)
   
   use_rdm <- use_rdm %>%
      mutate(dist_col = future_map2(.$data, colony$data, ~st_distance(.x, .y)))
   
   
   # Find distance to any colony or roost ------------------------------------
   
   # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
   colony_orig <- st_as_sf(colony_orig, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj)
   
   # Then, we need to remove any colony or roost that is less than 5km away from the central colony
   col_sel <- future_map(colony$data, ~st_distance(.x, colony_orig)) %>% 
      # add distance from central colony (for each year)
      future_map(~mutate(colony_orig,
                         dist_from_central = .x[1,])) %>% 
      # remove those that are closer than 5km
      future_map( ~filter(.x, as.numeric(dist_from_central) > 5e3))
   
   # For each year calculate distance between locations and any colony or roost
   # considering only those selected roosts and colonies that are further than 5km
   # from central colony
   use_rdm <- use_rdm %>% 
      mutate(dist_col_any = future_map2(.$data, col_sel,
                                        ~minDist_cpp(st_coordinates(st_as_sf(.)),
                                                     st_coordinates(.y))))
   
   
   # Find distance to restaurants --------------------------------------------
   
   # Unnest data
   use_rdm <- unnest(use_rdm, cols = c(-year))
   
   # To create a matrix of coordinates from the sfs layer, we need to create a spatial object
   sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj)
   
   # For each year calculate distance between locations and any sfs
   use_rdm <- use_rdm %>% 
      mutate(dist_sfs = minDist_cpp(st_coordinates(st_as_sf(.)), st_coordinates(sfs)))
   
   
   # Extract covariates from rasters -----------------------------------------
   
   # Recover spatial object and transform back to geographic coordinates
   use_rdm <- use_rdm %>% 
      st_as_sf() %>% 
      st_transform(crs = 4326)
   
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
      st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = FALSE) %>% 
      st_transform(crs = 4326) %>% 
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
      st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = FALSE) %>% 
      st_transform(crs = 4326) %>% 
      extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R",
                   extractCovtPath = "R/functions/extractCovt.R",
                   covts_path = "data/working/covts_rasters/topo_res01",
                   covts_names = c("res01_srtm0", "res01_slope", "res01_vrm3", "dist_slp"),
                   return_sf = FALSE, extract_method = "merge")
   
   
   # Extract protected areas -------------------------------------------------
   
   # Calculate intersection between locations and protected areas
   pa_int <- use_rdm %>%
      st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = FALSE) %>% 
      st_transform(crs = 4326) %>%
      st_intersects(pa, sparse = FALSE)
   
   # Create a protected areas covariate
   use_rdm <- use_rdm %>%
      mutate(prot_area = if_else(pa_int == FALSE, 0, 1))
   
   # Remove intersection
   rm(pa_int)
   
   
   # Create time of day variable ---------------------------------------------
   
   # Create log of step length and cosine of turning angle. These are used later in the mov. model
   use_rdm <- use_rdm %>% 
      mutate(hourday = lubridate::hour(t1_),
             ttnoon = hourday - 10,
             ttnoon_sq = (ttnoon)^2,
             log_sl = if_else(sl_ > 0, log(sl_), log(min(.$sl_[.$sl_ >0]))), # Otherwise model complains about infinite predictor
             cos_ta = cos(ta_))
   
   # Create also a time resolution variable
   # Recalculate dt in hours
   use_rdm <- use_rdm %>% 
      mutate(dt_ = as.numeric(dt_, units = "hours"))
   
   meandt <- mean(use_rdm$dt_, na.rm = T)
   
   use_rdm <- use_rdm %>% 
      mutate(res = case_when(meandt > 0.5 & meandt <= 1.5 ~ 1,
                             meandt > 1.5 & meandt <= 2.5 ~ 2,
                             meandt > 2.5 & meandt <= 3.5 ~ 3,
                             meandt > 3.5 & meandt <= 5 ~ 4,
                             meandt > 5 & meandt <= 12 ~ 8,
                             TRUE ~ 24))
   
   # Add bird to data frame --------------------------------------------------
   
   # Recover latitude -longitude coordinates
   use_rdm <- use_rdm %>% 
         st_as_sf(coords = c("x1_", "y1_"), crs = tmerproj, remove = F) %>% 
         st_transform(crs = 4326) %>% 
         mutate(lon1_ = st_coordinates(.)[,1],
                lat1_ = st_coordinates(.)[,2]) %>% 
         st_drop_geometry() %>% 
         st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = F) %>% 
         st_transform(crs = 4326) %>% 
         mutate(lon2_ = st_coordinates(.)[,1],
                lat2_ = st_coordinates(.)[,2]) %>% 
         st_drop_geometry()
   
   # Recover attributes (e.g. step-length and angle distributions)
   atts$names <- attr(use_rdm, "names")
   attributes(use_rdm) <- atts
   
   # Save track
   saveRDS(use_rdm, paste0("data/working/bird_tracks/fit_ready/ssf/", id_sel,".rds"))
   
}


# Create a unique data frame with all data --------------------------------

rm(list = ls())

# create a data frame to store results
model_data <- data.frame()

trkfiles <- list.files("data/working/bird_tracks/fit_ready/ssf")

# Keep only those files that will be used for training models not for testing
trkfiles <- trkfiles[!str_starts(trkfiles, "test")]

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

# There are a few NA NDVI corresponding mostly to locations over the ocean.
# I will assign the mean NDVI value
model_data %>%
   filter(is.na(NDVI_mean)) %>%
   group_by(land_cov) %>%
   summarize(n = n())

model_data <- model_data %>%
   mutate(NDVI_mean = if_else(is.na(NDVI_mean), mean(NDVI_mean, na.rm = T), NDVI_mean))

# Make variable names more understandable
model_data <- model_data %>% 
   rename(elev = res01_srtm0,
          slope = res01_slope,
          rugg = res01_vrm3)

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
          age = factor(age, levels = c("juv", "ad")),
          age_fct = age) %>%
   spread(age, i, fill = 0)


# Define trips from colony ------------------------------------------------

# Each day will be a trip unless the bird doesn't return to colony (dist_col < 5e3)

# Number of locations for each bird
model_data <- nest(model_data, data = -c(bird_id))

# Function to define trips from the central colony
f <- function(trk){
   
   trk <- trk %>% 
      mutate(date = lubridate::date(t2_))
   
   trips <- trk %>% 
      group_by(date) %>% 
      summarize(mindist = min(dist_col)) %>% 
      mutate(at_col = if_else(mindist <= 5e3, 1, 0))
   
   trips$trip <- trips$at_col
   
   # Initiate trips
   t <- 1
   trips$trip[1] <- t
   
   for(i in 2:nrow(trips)){
      if(trips$trip[i] == 0){
         trips$trip[i] <- t
      } else {
         t <- t + 1 
         trips$trip[i] <- t
      }
   }
   
   trips <- trips %>% 
      group_by(trip) %>% 
      mutate(days_away = difftime(date, lag(date), units = "day"),
             days_away = ifelse(is.na(days_away), 0, days_away),
             days_away = cumsum(days_away)) %>% 
      ungroup()
   
   trk <- trk %>% 
      left_join(trips %>% dplyr::select(date, trip, days_away),
                by = "date")
   
   return(trk)
   
}

model_data$data <- model_data$data %>% 
   map(~f(.x))

model_data <- unnest(model_data, cols = c(bird_id, data))

model_data <- model_data %>% 
   mutate(trip = paste(bird_id, trip, sep = "_"))

# model_data %>%
#    group_by(age_fct, trip) %>%
#    summarize(days_away = max(days_away)) %>%
#    filter(days_away < 25) %>%
#    ggplot() +
#    geom_histogram(aes(x = days_away)) +
#    facet_wrap("age_fct")


# Save data ---------------------------------------------------------------

saveRDS(model_data, "data/working/data_ssf_ready_7pp.rds")
