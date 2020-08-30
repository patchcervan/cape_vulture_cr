# 24-08-2020

# In this script we fit a SSF model to all vultures

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



# Process track -----------------------------------------------------------

# make amt xyt tracks
use_rdm <- tibble(
    bird_id =  test_data %>% 
        pull(bird_id),
    # Create a track_xyt data frame
    data = test_data %>% 
        pull(data) %>% 
        future_map(~ make_track(st_drop_geometry(.x), x, y, datetime, crs = crs(.x)))
)


# Round avg sampling frequency up and resample tracks.
# Keep only those bursts that allow step and angle calculation
use_rdm <- use_rdm %>% 
    mutate(rate = future_map(test_data$data, ~ceiling(mean(.x$dt))))

# use_rdm <- future_map2(use_rdm$data, use_rdm$rate, 
#                        ~track_resample(.x, rate = hours(1),
#                                        tolerance = minutes(15))) %>% 
#     future_map( ~filter_min_n_burst(.x, min_n = 3))

use_rdm <- future_map2(use_rdm$data, use_rdm$rate, 
                       ~track_resample(.x, rate = hours(as.integer(.y)),
                                       tolerance = minutes(ceiling(.y*25*60/100)))) %>% 
    future_map( ~filter_min_n_burst(.x, min_n = 3))

# Keep only those birds with more than 100 data points
keep <- which(sapply(use_rdm, nrow) > 100)
use_rdm <- use_rdm[keep]

# Transform to step and angle
use_rdm <- use_rdm %>% 
    future_map(~steps_by_burst(.x))


# Generate random steps --------------------------------------------------

future::plan("multiprocess")
use_rdm <- use_rdm %>% 
    future_map(~ random_steps(.x, n_control = 5, presence = .x))
future::plan("sequential")


# Extract covariates ------------------------------------------------------
#source("../functions/extractCovts.R")
source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates
future::plan("multiprocess")
use_rdm <- use_rdm %>% 
    future_map2(test_data$tmerproj[keep], ~ st_as_sf(.x, coords = c("x2_", "y2_"), remove = FALSE, crs = .y)) %>% 
    # calculate distance from colony
    future_map2(test_data$colony[keep], ~mutate(.x, dist = as.numeric(st_distance(.x, .y)))) %>% 
    # extract other covariates
    future_map(~ st_transform(.x, crs = 4326)) %>% 
    future_map(~ extractCovts(.x))
future::plan("sequential")


# Fit model ---------------------------------------------------------------

# Create time of day variable
# Create log of step length and cosine of turning angle. These are used later in the mov. model
use_rdm <- use_rdm %>% 
    future_map(~mutate(.x, hourday = lubridate::hour(t1_),
                       ttnoon = 12 - hourday,
                       ttnoon_sq = (ttnoon)^2,
                       log_sl = if_else(sl_ > 0, log(sl_), log(min(.$sl_[.$sl_ >0]))), # Otherwise model complains about infinite predictor
                       cos_ta = cos(ta_)))

# Standardize covariates and fit model
use_rdm_st <- use_rdm %>% 
    future_map(~ mutate(.x, across(.cols = c("srtm", "slope", "vrm3", "dist"), scale)))

ssf_fits <- use_rdm_st %>% 
    future_map(~ fit_issf(case_ ~ srtm + slope + vrm3 + dist +
                             sl_ + sl_:ttnoon + sl_:ttnoon_sq +
                             log_sl + log_sl:ttnoon + log_sl:ttnoon_sq +
                             cos_ta + cos_ta:ttnoon + cos_ta:ttnoon_sq +
                             strata(step_id_), data = .x))

# Plot use vs available
use_rdm %>% 
    future_map2(test_data$bird_id[keep], ~mutate(.x, bird_id = .y)) %>% 
    do.call("rbind",.) %>% 
    arrange(case_) %>% 
    ggplot() +
    geom_point(aes(x = x2_, y = y2_, colour = case_), alpha = 0.5, size = 1) +
    scale_colour_manual(values = c("black", "red")) +
    facet_wrap("bird_id", scales = "free")
    

# Extract coefficients
ssf_coeff <- ssf_fits %>% 
    map(~ as_tibble(summary(.x)$coefficients, rownames = "variable")) %>% 
    map2(test_data$bird_id[keep], ~ mutate(.x, bird_id = .y)) %>% 
    map2(test_data %>% 
    left_join(dplyr::select(db, bird_id, age), by = "bird_id") %>% 
        slice(keep) %>% pull(age), ~ mutate(.x, age = .y)) %>%
    do.call("rbind", .)

# Plot
ssf_coeff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist")) %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = coef, 
                        ymin = coef - `se(coef)`, 
                        ymax = coef + `se(coef)`,
                        colour = age)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(bird_id ~ .)

ssf_coeff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist")) %>% 
    ggplot() +
    geom_pointrange(aes(x = bird_id, y = coef, 
                        ymin = coef - `se(coef)`, 
                        ymax = coef + `se(coef)`,
                        colour = age)) +
    facet_grid(variable ~ ., scales = "free")

# Save model outputs ------------------------------------------------------

# Combine fit and data
ssf_fits <- tibble(
    fits = ssf_fits,
    data = use_rdm_st
)

# Create file name
saveas <- paste0("output/ssf_fit_", paste(test_data$bird_id[keep], collapse = "_"), ".rds")

# Save
write_rds(ssf_fits, saveas)


