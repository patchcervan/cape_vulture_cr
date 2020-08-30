# 24-08-2020

# In this script we fit a RSF model to all vultures around its colony

library(tidyverse)
library(sf)
library(amt)
library(raster)
library(lubridate)
library(furrr)

rm(list = ls())


# Read in training data ---------------------------------------------------

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


# Process tracks ----------------------------------------------------------

# make amt xyt tracks for adult birds only
ad_data <- test_data %>% 
    left_join(dplyr::select(db, bird_id, age), by = "bird_id") %>% 
    filter(age == "ad")

use_rdm <- tibble(
    bird_id =  ad_data %>% 
        pull(bird_id),
    # Create a track_xyt data frame
    data = ad_data %>% 
        pull(data) %>% 
        future_map(~ make_track(st_drop_geometry(.x), x, y, datetime, crs = crs(.x)))
)


# Define available area ---------------------------------------------------

# Calculate maximum distance from colony (quantile)
use_rdm <- use_rdm %>% 
    mutate(max_dist = future_map2(ad_data$colony, ad_data$data, ~ quantile(st_distance(.x, .y), 0.99)))

# Define available area
use_rdm <- use_rdm %>% 
    mutate(av_area = future_map2(ad_data$colony, use_rdm$max_dist, ~ st_buffer(x = .x, dist = .y)))


# Generate random points --------------------------------------------------

future::plan("multiprocess")
use_rdm <- use_rdm$data %>% 
    future_map2(use_rdm$av_area, ~ random_points(.y, n = nrow(.x) * 5, presence = .x))
future::plan("sequential")


# Extract covariates ------------------------------------------------------

source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates
future::plan("multiprocess")
use_rdm <- use_rdm %>% 
    future_map2(ad_data$tmerproj, ~ st_as_sf(.x, coords = c("x_", "y_"), remove = FALSE, crs = .y)) %>% 
    # calculate distance from colony
    future_map2(ad_data$colony, ~mutate(.x, dist = as.numeric(st_distance(.x, .y)))) %>% 
    # extract other covariates
    future_map(~ st_transform(.x, crs = 4326)) %>% 
    future_map(~ extractCovts(.x))
future::plan("sequential")


# Fit model ---------------------------------------------------------------

# Standardize covariates and fit model
use_rdm_st <- use_rdm %>% 
    future_map(~ mutate(.x, across(.cols = c("srtm", "slope", "vrm3", "dist"), scale)))

rsf_fits <- use_rdm_st %>% 
    future_map(~ fit_rsf(case_ ~ srtm + slope + vrm3 + dist, data = .x))

# Extract coefficients
rsf_coeff <- rsf_fits %>% 
    map(~ as_tibble(summary(.x)$coefficients, rownames = "variable")) %>% 
    map2(ad_data$bird_id, ~ mutate(.x, bird_id = .y)) %>% 
    do.call("rbind", .)

# Plot
rsf_coeff %>% 
    filter(variable != "(Intercept)") %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(bird_id ~ .)

rsf_coeff %>% 
    filter(variable != "(Intercept)") %>% 
    ggplot() +
    geom_pointrange(aes(x = bird_id, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`)) +
    facet_grid(variable ~ ., scales = "free")


# Save model outputs ------------------------------------------------------

# Combine fit and data
rsf_fits <- tibble(
    fits = rsf_fits,
    data = use_rdm_st
)

# Create file name
saveas <- paste0("output/rsf_colony_fit_", paste(ad_data$bird_id, collapse = "_"), ".rds")

# Save
write_rds(rsf_fits, saveas)


