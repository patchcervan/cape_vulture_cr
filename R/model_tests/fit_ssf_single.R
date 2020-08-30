# 24-08-2020

# In this script we fit a SSF model to a single vulture

library(tidyverse)
library(sf)
library(amt)
library(raster)

rm(list = ls())


# Read in training data ---------------------------------------------------

# Load test data
test_data <- read_csv("data/working/test_data/test_data.csv")

# Load bird data base
db <- read_csv("data/working/bird_db.csv")


# PREPARE DATA FOR MODEL FITTING ------------------------------------------

# Create a spatial object for one bird
trk <- test_data %>% 
    filter(bird_id == "ma14") %>% 
    dplyr::select(bird_id, datetime, dt, lon, lat) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE)


# Project trajectories ----------------------------------------------------

# Define projection for each data frame
source("R/functions/makeTmerProj.R")

tmerproj <- trk %>% makeTmerProj()

# Project trajectories
trk <- trk %>% st_transform(crs = tmerproj)

# Change x and y coordinates within data frames
trk <- trk %>% 
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2])


# Find colonies -----------------------------------------------------------

source("R/functions/findColony.R")

colony <- trk %>% findColony(bw = 1000, sp_proj = tmerproj)


# Process track -----------------------------------------------------------

# Create a track_xyt data frame
use_rdm <- make_track(st_drop_geometry(trk), x, y, datetime, crs = crs(trk))

# Resample track to one location per hour and
# keep only those burst that allow step and angle calculation
use_rdm <- track_resample(use_rdm, rate = hours(1),
                          tolerance = minutes(15)) %>% 
    filter_min_n_burst(min_n = 3)

any(duplicated(use_rdm$t_))
    
all(complete.cases(use_rdm))

# Transform to step and angle
use_rdm <- steps_by_burst(use_rdm)

# Generate random steps --------------------------------------------------

use_rdm <- use_rdm %>% 
    random_steps(n_control = 5, presence = use_rdm)


# Extract covariates ------------------------------------------------------

source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates

use_rdm <- use_rdm %>% 
    st_as_sf(coords = c("x2_", "y2_"), remove = FALSE, crs = crs(colony)) %>% 
    # calculate distance from colony
    mutate(dist = as.numeric(st_distance(., colony))) %>% 
    # extract other covariates
    st_transform(crs = 4326) %>% 
    extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R")


# Fit RSF model -----------------------------------------------------------

# Create time of day variable
# Create log of step length and cosine of turning angle. These are used later in the mov. model
use_rdm <- use_rdm %>% 
    mutate(hourday = lubridate::hour(t1_),
           ttnoon = 12 - hourday,
           ttnoon_sq = (ttnoon)^2,
           log_sl = if_else(sl_ > 0, log(sl_), log(min(.$sl_[.$sl_ >0]))), # Otherwise model complains about infinite predictor
           cos_ta = cos(ta_))

# Standardize covariates and fit model
use_rdm_st <- use_rdm %>% 
    mutate(across(.cols = c("srtm", "slope", "vrm3", "dist"), scale))
    
ssf_fit <- use_rdm_st %>%
    fit_issf(case_ ~ srtm + slope + vrm3 + dist +
                 sl_ + sl_:ttnoon + sl_:ttnoon_sq +
                 log_sl + log_sl:ttnoon + log_sl:ttnoon_sq +
                 cos_ta + cos_ta:ttnoon + cos_ta:ttnoon_sq +
                 strata(step_id_))

summary(ssf_fit)

# Extract coefficients
ssf_coeff <- summary(ssf_fit)$coefficients %>%
    as_tibble(rownames = "variable")

# Plot
ssf_coeff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist")) %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = coef, 
                        ymin = coef - `se(coef)`, 
                        ymax = coef + `se(coef)`)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")


# Save model outputs ------------------------------------------------------

# Combine fit and data
ssf_fit <- tibble(
    fits = list(ssf_fit),
    data = list(use_rdm_st)
)

# Create file name
saveas <- paste0("output/ssf_fit_", unique(trk$bird_id), ".rds")

# Save
write_rds(ssf_fit, saveas)
