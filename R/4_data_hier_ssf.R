# 16-10-2020

# In this script I fit a hierarchical step-selection function model
# to all vultures selected from the data base

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)
library(amt)
library(furrr)
library(Rcpp)
library(lme4)

# Read in data ------------------------------------------------------------

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/fit_ready", pattern = ".csv")

# Read in bird data base
bird_db <- read_csv("data/working/bird_db_fit_ready.csv")

# Read in colony data
colony_db <- read_csv("data/working/colony_db.csv") 

# Read in roost and colony data
colony_orig <- read_csv("data/raw/CV_Colonies_20200601.csv")

# Extract roosts and fix names
roost <- colony_orig %>% 
    dplyr::select(ID_key, latitude, longitude, ColonyType) %>% 
    rename_all(tolower) %>% 
    filter(str_detect(colonytype, "Roost"))

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


# Annotate tracks ---------------------------------------------------------

# create a data frame to store results
model_data <- data.frame()


# for each bird
for(i in 1:length(trk_files)){
    
    # i = 2
    
    trk <- read_csv(paste0("data/working/bird_tracks/fit_ready/", trk_files[i]))
    
    id_sel <- unique(trk$bird_id)
    print(id_sel)
    
    # Create sf object and reproject
    trk <- st_as_sf(trk, coords = c("x_", "y_", "t_"), crs = 4326, remove = F)
    tmerproj <- makeTmerProj(trk)
    trk <- st_transform(trk, tmerproj)
    
    # Change coordinates and drop spatial geometry (amt doesn't like)
    trk <- trk %>% 
        mutate(lon = x_,
               lat = y_,
               x_ = st_coordinates(.)[,1],
               y_ = st_coordinates(.)[,2]) %>% 
        st_drop_geometry()
    
    
    # Generate random steps --------------------------------------------------
    
    # Create steps object
    trk <- trk %>% 
        dplyr::select(x_, y_, t_, bird_id, age, burst_) %>% 
        make_track(x_, y_, t_, all_cols = T) %>% 
        steps_by_burst(keep_cols = 'end')
    
    use_rdm <- trk %>% 
        random_steps(n_control = 5)
    
    
    # Find distance to colony -------------------------------------------------
    
    # Find colonies used by the bird, make spatial object, and nest by year
    colony <- colony_db %>% 
        filter(bird_id == id_sel) %>% 
        st_as_sf(coords = c("dens_lon", "dens_lat"), crs = 4326, remove = F) %>% 
        st_transform(tmerproj) %>% 
        nest(data = -year)
    
    years <- unique(colony$year)
    
    # Classify birds according to where their colony for the year is
    use_rdm <- use_rdm %>% 
        mutate(year = lubridate::year(t2_)) %>% 
        left_join(dplyr::select(colony_db, bird_id, year, zone), by = c("bird_id", "year"))
    
    # for each year's colony find distance to tracking locations
    use_rdm <- use_rdm %>% 
        st_as_sf(coords = c("x2_", "y2_"), crs = tmerproj, remove = F) %>% 
        nest(data = -year)
    
    use_rdm <- use_rdm %>%
        mutate(dist_col = future_map2(.$data, colony$data, ~st_distance(.x, .y)))
    

    # Find distance to any colony or roost ------------------------------------
    
    # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
    colony_orig <- st_as_sf(colony_orig, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
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
    
    # Find distance to roosts -------------------------------------------------
    
    # To create a matrix of coordinates from the roost layer, we need to create a spatial object
    roost <- st_as_sf(roost, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
        st_transform(tmerproj)
    
    # Then, we need to remove any roost that is less than 10km away from the central colony
    roost_sel <- future_map(colony$data, ~st_distance(.x, roost)) %>% 
        # add distance from central colony (for each year)
        future_map(~mutate(roost,
                           dist_from_central = .x[1,])) %>% 
        # remove those that are closer than 10km
        future_map( ~filter(.x, as.numeric(dist_from_central) > 1e4))
    
    # For each year calculate distance between locations and any roost
    # considering only those selected roosts that are further than 10km
    # from central colony
    use_rdm <- use_rdm %>% 
        mutate(dist_roost = future_map2(.$data, roost_sel,
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
    use_rdm <- unnest(use_rdm, cols = c(-year))
    
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
                                                                 covts_path = "data/working/copernicus",
                                                                 covts_names = paste0(c("LC100_global_v3.0.1_"), .y),
                                                                 return_sf = TRUE, extract_method = "merge"))) %>% 
        mutate(data = future_map(.$data, ~rename(.x, land_use = which(str_detect(names(.), "LC100"))))) %>% 
        mutate(year = years) %>%    # Return years back to their original values
        unnest(cols = c(year, data)) 
        
    
    # Nest and extract NDVI for the different years
    use_rdm <- use_rdm  %>%
        st_as_sf() %>% 
        nest(data = -year) %>% 
        mutate(data = future_map2(.$data, .$year, ~ extractCovts(.x,
                                                                 loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                                                 extractCovtPath = "R/functions/extractCovt.R",
                                                                 covts_path = "data/working/covts_rasters/modis",
                                                                 covts_names = paste0(c("NDVI_doy"), .y),
                                                                 return_sf = TRUE, extract_method = "stack"))) %>%
        unnest(cols = c(year, data))
    
    # Calculate mean NDVI for the year and remove monthly measurements
    use_rdm <- use_rdm  %>% 
        mutate(NDVI_mean = rowMeans(dplyr::select(., which(str_detect(names(.), "NDVI"))), na.rm = TRUE)) %>% 
        dplyr::select(which(!str_detect(names(.), "NDVI_doy")))
    
    # Extract topographical covariates
    use_rdm <- use_rdm %>% 
        st_as_sf() %>%
        extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R",
                     extractCovtPath = "R/functions/extractCovt.R",
                     covts_path = "data/working/covts_rasters",
                     covts_names = c("srtm", "slope", "vrm3"),
                     return_sf = TRUE, extract_method = "merge")
    
    
    # Extract protected areas -------------------------------------------------
    
    # Calculate intersection between locations and protected areas
    pa_int <- use_rdm %>%
        st_as_sf() %>%
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
               ttnoon = 12 - hourday,
               ttnoon_sq = (ttnoon)^2,
               log_sl = if_else(sl_ > 0, log(sl_), log(min(.$sl_[.$sl_ >0]))), # Otherwise model complains about infinite predictor
               cos_ta = cos(ta_))
    
    # Create also a time resolution variable
    use_rdm <- use_rdm %>% 
        mutate(res = case_when(dt_ > 0.5*60 & dt_ <= 1.5*60 ~ 1,
                               dt_ > 1.5*60 & dt_ <= 2.5*60 ~ 2,
                               dt_ > 2.5*60 & dt_ <= 3.5*60 ~ 3,
                               dt_ > 3.5*60 & dt_ <= 5*60 ~ 4,
                               dt_ > 5*60 & dt_ <= 12*60 ~ 8,
                               TRUE ~ 24))
    
    # Add bird to data frame --------------------------------------------------
    
    # Remove geometry and merge
    use_rdm <- st_drop_geometry(use_rdm)
    
    model_data <- rbind(model_data, use_rdm)
    
    print(c(i, id_sel))
    
}

# Save data for model fitting
write_rds(model_data, file = "data/working/data_ssf_ready.rds")
