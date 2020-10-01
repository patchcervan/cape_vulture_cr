# 24-08-2020

# In this script we fit a RSF model to all vultures within their home range

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

test_data <- test_data %>% 
    mutate(colony = future_map2(test_data$data, test_data$tmerproj, ~ findColony(.x, bw = 1000, sp_proj = .y, plotkde = F)))


# Process tracks ----------------------------------------------------------

# make amt xyt tracks for adult birds only
test_data <- test_data %>% 
    left_join(dplyr::select(db, bird_id, age), by = "bird_id") %>% 
    filter(age == "ad")

use_rdm <- tibble(
    bird_id =  test_data %>% 
        pull(bird_id),
    # Create a track_xyt data frame
    data = test_data %>% 
        pull(data) %>% 
        future_map(~ make_track(st_drop_geometry(.x), x, y, datetime, crs = crs(.x)))
)


# Define available area ---------------------------------------------------

use_rdm <- use_rdm %>% 
    mutate(av_area = future_map(use_rdm$data, ~ sf::st_buffer(hr_isopleths(hr_mcp(.x, levels = 1)), dist = 5000)))


# Generate random points --------------------------------------------------


use_rdm <- use_rdm$data %>% 
    future_map2(use_rdm$av_area, ~ random_points(.y, n = nrow(.x) * 5, presence = .x))

# Plot use vs available
useAvaPlot <- use_rdm %>% 
    future_map2(test_data$bird_id, ~mutate(.x, bird_id = .y)) %>%
    future_map(~dplyr::select(.x, x_, y_, case_, bird_id)) %>% 
    do.call("rbind",.) %>% 
    arrange(case_) %>% 
    ggplot() +
    geom_point(aes(x = x_, y = y_, colour = case_), alpha = 0.5, size = 1) +
    scale_colour_manual(values = c("black", "red")) +
    facet_wrap("bird_id", scales = "free")

# useAvaPlot
ggsave(useAvaPlot, filename = "output/rsf_hr_multi_useava.png", dpi = 700)


# Extract covariates ------------------------------------------------------

source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates

trk_year <- future_map_dbl(test_data$data, ~lubridate::year(min(.x$datetime)))

use_rdm <- use_rdm %>% 
    future_map2(test_data$tmerproj, ~ st_as_sf(.x, coords = c("x_", "y_"), remove = FALSE, crs = .y)) %>% 
    # calculate distance from colony
    future_map2(test_data$colony, ~mutate(.x, dist = as.numeric(st_distance(.x, .y)))) %>% 
    # extract other covariates
    future_map(~ st_transform(.x, crs = 4326)) %>% 
    future_map(~ extractCovts(.x,
                              loadCovtsPath = "R/functions/loadCovtsRasters.R",
                              extractCovtPath = "R/functions/extractCovt.R",
                              covts_path = "data/working/covts_rasters",
                              covts_names = c("srtm", "slope", "vrm3"),
                              return_sf = TRUE, extract_method = "merge")) %>% 
    future_map2(trk_year, ~ extractCovts(.x,
                                         loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                         extractCovtPath = "R/functions/extractCovt.R",
                                         covts_path = "data/working/covts_rasters/modis",
                                         covts_names = paste0(c("NDVI_doy"), .y),
                                         return_sf = FALSE, extract_method = "stack"))


# Load protected areas
pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")

# Calculate intersection between locations and protected areas
pa_int <- use_rdm %>%
    future_map2(test_data$colony, ~st_as_sf(.x, coords = c("x_", "y_"), remove = FALSE, crs = crs(.y))) %>%
    future_map(~st_transform(.x, crs = 4326)) %>%
    future_map(~st_intersects(.x, pa, sparse = FALSE))

# Create a protected areas covariate
use_rdm <- use_rdm %>%
    future_map2(pa_int, ~mutate(.x, prot_area = .y)) %>%
    future_map(~mutate(.x, prot_area = if_else(prot_area == FALSE, 0, 1)))

# Remove intersection
rm(pa_int)


# Standardize covariates
use_rdm_st <- use_rdm %>% 
    future_map(~mutate(.x, NDVI_mean = rowMeans(dplyr::select(., which(str_detect(names(.), "NDVI")))))) %>% 
    future_map(~ mutate(.x, across(.cols = c("srtm", "slope", "vrm3", "dist", "NDVI_mean"), scale)))


# Fit model ---------------------------------------------------------------

rsf_fits <- use_rdm_st %>% 
    future_map(~ fit_rsf(case_ ~ srtm + slope + vrm3 + dist + prot_area + NDVI_mean, data = .x))

# Extract coefficients
rsf_coeff <- rsf_fits %>% 
    map(~ as_tibble(summary(.x)$coefficients, rownames = "variable")) %>% 
    map2(test_data$bird_id, ~ mutate(.x, bird_id = .y)) %>% 
    map2(test_data %>% 
             left_join(dplyr::select(db, bird_id, age), by = "bird_id") %>% 
             pull(age), ~ mutate(.x, age = .y)) %>%
    do.call("rbind", .)

# Plot
rsf_coeff %>% 
    filter(variable != "(Intercept)") %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`,
                        colour = age)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(bird_id ~ .)

rsf_coeff %>% 
    filter(variable != "(Intercept)") %>% 
    ggplot() +
    geom_pointrange(aes(x = bird_id, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`,
                        colour = age)) +
    facet_grid(variable ~ ., scales = "free")


# Save model outputs ------------------------------------------------------

# Combine fit and data
rsf_fits <- tibble(
    bird_id = test_data$bird_id,
    fits = rsf_fits,
    data = use_rdm_st
)

# Save
write_rds(rsf_fits, "output/rsf_hr_fit_multi.rds")


