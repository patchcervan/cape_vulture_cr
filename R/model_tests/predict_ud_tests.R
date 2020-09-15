# 30-08-202

# In this script we will produce resource selection maps to predict
# use by Cape Vultures from different models

# Load libraries
library(tidyverse)
library(sf)
library(raster)
# library(lubridate)
library(amt)
library(furrr)


# Clean workspace
rm(list = ls())

# Load test data
test_data <- read_csv("data/working/test_data/test_data.csv")

# Load bird data base
db <- read_csv("data/working/bird_db.csv")


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
    mutate(tmerproj = map(data, ~ makeTmerProj(.x)))

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
test_data <- test_data %>% 
    mutate(av_area = future_map(test_data$colony, ~ st_buffer(x = .x, dist = 200000)))


# Load rasters and predict ------------------------------------------------

source("R/functions/prepCovtsPredict.R")

# Create data frame to store results
cv_df <- tibble(
    target_id = character(),
    mod = character(),
    pred_id = character(),
    log_lik = double()
    )

# Load model fits
fits <- list(colony = read_rds("output/rsf_colony_fit_ez05_ma14_mb06_na03_wt07.rds"),
             hr = read_rds("output/rsf_hr_fit_ct06_ct09_ez05_ez06_ma14_ma15_mb05_mb06_na03_na06_wt07_wt18.rds"),
             ssf = read_rds("output/ssf_fit_ct06_ct09_ez05_ez06_ma14_ma15_mb05_mb06_na03_na06_wt18.rds"))

# Extract scaling factor from data and remove data from fits to free up memory
for(j in 1:length(fits)){
    
    # Extract scaling factors for each model data
    fits[[j]]$data <- fits[[j]]$data %>% 
        future_map(~dplyr::select(.x, dist, srtm, slope, vrm3, NDVI_mean)) %>% 
        map(~map(.x, ~attr(.x, "scaled:scale"))) %>% 
        map(~unlist(.x))

}

gc()


# For each bird
for(i in 1:nrow(test_data)){
    # i <- 1
    
    target_id <- test_data$bird_id[i]
    
    a <- st_transform(test_data$av_area[[i]], crs = 4326)
    
    colony <- st_transform(test_data$colony[[i]], crs = 4326)
    
    year <- unique(lubridate::year(test_data$data[[i]]$datetime))
    
    covts <- prepCovtsPredict(av_area = a, year = year, colony = colony,
                              topo_covts = c("srtm", "slope", "vrm3"), 
                              loadCovtsPath = "R/functions/loadCovtsRasters.R",
                              covts_orig = "data/working")
    
    # For each of the different models calculate preference
    for(j in 1:length(fits)){
        # j <- 1
        
        mod <- names(fits)[j]
        
        fit <- fits[[j]]
        
        # Based on the models fit to each of the birds
        for(k in 1:nrow(fit)){
            # k <- 1
            
            pred_id <- fit$bird_id[k]
            
            if(target_id == pred_id) next
            
            # Fit from predictive bird
            fit_bird <- fit$fits[[k]]
            
            # Extract model coefficients
            coef_bird <- coefficients(fit_bird)
            
            # Extract scaling factors
            sd_factors <- fit$data[[k]]
            
            # Scale coefficients
            sd_factors <- sd_factors[names(covts)]
            sd_factors[is.na(sd_factors)] <- 1
            coef_bird <- coef_bird[names(covts)] / sd_factors
            
 
            # Predict
            log_pref <- raster::raster(covts)
            log_pref <- raster::setValues(log_pref, 0)
            
            for (r in names(coef_bird)) {
                log_pref <- log_pref + covts[[r]] * coef_bird[[r]]
            }
            
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
