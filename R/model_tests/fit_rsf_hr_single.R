# 24-08-2020

# In this script we fit a RSF model to a single vulture around its home range

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


# Define available area ---------------------------------------------------

av_area <- sf::st_buffer(hr_isopleths(hr_mcp(use_rdm, levels = 1)), dist = 5000)


# Generate random points --------------------------------------------------
use_rdm <- use_rdm %>% 
    random_points(av_area, n = nrow(.) * 5, presence = .)


# Extract covariates ------------------------------------------------------
source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates
use_rdm <- use_rdm %>% 
    st_as_sf(coords = c("x_", "y_"), remove = FALSE, crs = crs(colony)) %>% 
    # calculate distance from colony
    mutate(dist = as.numeric(st_distance(., colony))) %>% 
    # extract other covariates
    st_transform(crs = 4326) %>% 
    extractCovts(loadCovtsPath = "R/functions/loadCovtsRasters.R")


# Fit model ---------------------------------------------------------------

# Standardize covariates and fit model
use_rdm_st <- use_rdm %>% 
    mutate(across(.cols = c("srtm", "slope", "vrm3", "dist"), scale))

rsf_fit <- use_rdm_st %>%
    fit_rsf(case_ ~ srtm + slope + vrm3 + dist)

summary(rsf_fit)

# Extract coefficients
rsf_coeff <- summary(rsf_fit)$coefficients %>%
    as_tibble(rownames = "variable")

# Plot
rsf_coeff %>% 
    filter(variable != "(Intercept)") %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")


# Save model outputs ------------------------------------------------------

# Combine fit and data
rsf_fit <- tibble(
    fits = rsf_fit,
    data = list(use_rdm_st)
)

# Create file name
saveas <- paste0("output/rsf_hr_fit_", unique(trk$bird_id), ".rds")

# Save
write_rds(rsf_fit, saveas)
