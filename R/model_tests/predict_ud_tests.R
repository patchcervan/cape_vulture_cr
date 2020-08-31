# 30-08-202

# In this script we will produce resource selection maps to predict
# use by Cape Vultures from different models

# Load libraries
library(tidyverse)
library(sf)
library(raster)
library(furrr)
# library(lubridate)
library(amt)

# Clean workspace
rm(list = ls())

# Load test data
test_data <- read_csv("data/working/test_data/test_data.csv")

# Load bird data base
db <- read_csv("data/working/bird_db.csv")

# Load model fits
fits <- list(colony = read_rds("output/rsf_colony_fits_test.rds"),
             hr = read_rds("output/rsf_hr_fits_test.rds"),
             ssf = read_rds("output/ssf_fits_test.rds"))


# PREPARE DATA FOR MODEL FITTING ------------------------------------------

# Create a spatial object nested by bird
test_data <- test_data %>% 
    dplyr::select(bird_id, datetime, dt, lon, lat) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE) %>%
    nest(data = c(-bird_id))


# Project trajectories ----------------------------------------------------

# Define projection for each data frame
source("R/functions/makeTmerProj.R")

test_data <- test_data %>% 
    mutate(tmerproj = future_map(data, ~ makeTmerProj(.x)))

# Project trajectories
test_data <- test_data %>% 
    mutate(data = future_map2(test_data$data, test_data$tmerproj, ~ st_transform(.x, crs = .y)))

# Change x and y coordinates within data frames
test_data <- test_data %>% 
    mutate(data = future_map(data, ~ mutate(.x, 
                                            x = st_coordinates(.x)[,1],
                                            y = st_coordinates(.x)[,2])))


# Find colonies -----------------------------------------------------------

source("R/functions/findColony.R")

future::plan("multiprocess")
test_data <- test_data %>% 
    mutate(colony = future_map2(test_data$data, test_data$tmerproj, ~ findColony(.x, bw = 1000, sp_proj = .y)))
future::plan("sequential")


# Define available area ---------------------------------------------------

# I am going to define an area of 200 km around colonies

future::plan("multiprocess")
test_data <- test_data %>% 
    mutate(av_area = future_map(test_data$colony, ~ st_buffer(x = .x, dist = 200000)))
future::plan("sequential")

# Load rasters and predict ------------------------------------------------

source("R/functions/loadCovtsRasters.R")

# Create data frame to store results
cv_df <- tibble(
    target_id = character(),
    mod = character(),
    pred_id = character(),
    log_lik = double()
    )

# For each bird
for(i in 1:nrow(test_data)){
    # i <- 2
    
    target_id <- test_data$bird_id[i]
    
    a <- st_transform(test_data$av_area[[i]], crs = 4326)
    
    colony <- st_transform(test_data$colony[[i]], crs = 4326)
    
    srtm <- loadCovts(a, covt = "srtm")
    slope <- loadCovts(a, covt = "slope")
    vrm3 <- loadCovts(a, covt = "vrm3")
    
    # crop covariates
    future::plan("multiprocess")
    srtm <- future_map(srtm, ~crop(.x, a))
    slope <- future_map(slope, ~crop(.x, a))
    vrm3 <- future_map(vrm3, ~crop(.x, a))
    
    
    if(length(srtm) > 1){
        srtm <- do.call(raster::merge, srtm)
        slope <- do.call(raster::merge, slope)
        vrm3 <- do.call(raster::merge, vrm3)
    } else{
        srtm <- srtm[[1]]
        slope <- slope[[1]]
        vrm3 <- vrm3[[1]]
    }
    
    future::plan("sequential") # It's important to close multiprocess after merge otherwise temp files are lost
    
    # Calculate distance
    dist <- raster::distanceFromPoints(srtm, st_coordinates(colony))
    
    # For each of the different models calculate preference
    for(j in 1:length(fits)){
        # j <- 1
        
        mod <- names(fits)[j]
        
        fit <- fits[[j]]
        
        # Based on the models fit to each of the birds
        for(k in 1:nrow(fit)){
            # k <- 1
            
            pred_id <- fit$bird_id[k]
            
            fit_bird <- fit$fits[[k]]
            data_bird <- fit$data[[k]]
            
            center_factors <- data_bird %>% 
                dplyr::select(dist, srtm, slope, vrm3) %>% 
                map(~attr(.x, "scaled:center")) %>% 
                unlist()
            
            sd_factors <- data_bird %>% 
                dplyr::select(dist, srtm, slope, vrm3) %>% 
                map(~attr(.x, "scaled:scale")) %>% 
                unlist()
            
            coef_bird <- coefficients(fit_bird)
            
            log_pref <- coef_bird["srtm"] / sd_factors["srtm"] * srtm + 
                coef_bird["slope"] / sd_factors["slope"] * slope + 
                coef_bird["vrm3"] / sd_factors["vrm3"] * vrm3 + 
                coef_bird["dist"] / sd_factors["dist"] * dist
            
            # plot(log_pref)
            # plot(st_geometry(st_transform(test_data$data[[i]], crs = 4326)), add = T)
            
            rsf <- exp(log_pref) / cellStats(exp(log_pref), sum)
            
            lik <- raster::extract(rsf, st_coordinates(st_transform(test_data$data[[i]], crs = 4326)))
            
            log_lik <- sum(log(lik), na.rm = T)
            
            cv_df <- rbind(cv_df, tibble(target_id = target_id,
                                         mod = mod, 
                                         pred_id = pred_id,
                                         log_lik = log_lik))
        }
    }
}

cvplot <- ggplot(cv_df) +
    geom_boxplot(aes(x = mod, y = log_lik)) +
    geom_jitter(aes(x = mod, y = log_lik), height = 0, width = 0.05, col = "red", alpha = 0.5) +
    xlab("Model") + ylab("Predicted log-likelihood") +
    facet_grid(target_id ~., scales = "free")

ggsave(cvplot, file = "figures/cv_results_test.png", dpi = 700)

write_rds(cv_df, "output/cv_results_tests.rds")
