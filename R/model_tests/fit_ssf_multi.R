# 24-08-2020

# In this script we fit a SSF model to all vultures

library(tidyverse)
library(sf)
library(amt)
library(raster)
library(lubridate)
library(furrr)
future::plan("sequential")
future::plan("multiprocess")

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

# temporal object to detect problems with the sampling rates
use_rdm_temp <- future_map2(use_rdm$data, use_rdm$rate, 
                       ~track_resample(.x, rate = hours(as.integer(.y)),
                                       tolerance = minutes(ceiling(.y*25*60/100)))) %>% 
    future_map( ~filter_min_n_burst(.x, min_n = 3))

# Which birds don't have enough locations per burst to calculate angle
which(sapply(use_rdm_temp, nrow) < 100)

# change sampling rate for number 6
use_rdm$rate[12] <- 4

# Repeat sampling rate problem detection
use_rdm_temp <- future_map2(use_rdm$data, use_rdm$rate, 
                            ~track_resample(.x, rate = hours(as.integer(.y)),
                                            tolerance = minutes(ceiling(.y*25*60/100)))) %>% 
    future_map( ~filter_min_n_burst(.x, min_n = 3))

# Which birds don't have enough locations per burst to calculate angle
which(sapply(use_rdm_temp, nrow) < 100)


# Keep only those birds with more than 100 data points
keep <- which(sapply(use_rdm_temp, nrow) > 100)
use_rdm <- use_rdm_temp[keep]

# Transform to step and angle
use_rdm <- use_rdm %>% 
    future_map(~steps_by_burst(.x))


# Generate random steps --------------------------------------------------

use_rdm <- use_rdm %>% 
    future_map(~ random_steps(.x, n_control = 5, presence = .x))

# Plot use vs available
useAvaPlot <- use_rdm %>%
    future_map2(test_data$bird_id[keep], ~mutate(.x, bird_id = .y)) %>%
    future_map(~dplyr::select(.x, x2_, y2_, case_, bird_id)) %>% 
    do.call("rbind",.) %>% 
    arrange(case_) %>% 
    ggplot() +
    geom_point(aes(x = x2_, y = y2_, colour = case_), alpha = 0.5, size = 1) +
    scale_colour_manual(values = c("black", "red")) +
    facet_wrap("bird_id", scales = "free")

# useAvaPlot
ggsave(useAvaPlot, filename = "output/ssf_multi_useava.png", dpi = 700)


# Extract covariates ------------------------------------------------------

source("R/functions/extractCovts.R")

# Reproject use-available points and extract covariates

trk_year <- future_map_dbl(test_data$data, ~lubridate::year(min(.x$datetime)))

use_rdm <- use_rdm %>% 
    future_map2(test_data$tmerproj[keep], ~ st_as_sf(.x, coords = c("x2_", "y2_"), remove = FALSE, crs = .y)) %>% 
    # calculate distance from colony
    future_map2(test_data$colony[keep], ~mutate(.x, dist = as.numeric(st_distance(.x, .y)))) %>% 
    # extract other covariates
    future_map(~ st_transform(.x, crs = 4326)) %>% 
    future_map(~ extractCovts(.x,
                              loadCovtsPath = "R/functions/loadCovtsRasters.R",
                              extractCovtPath = "R/functions/extractCovt.R",
                              covts_path = "data/working/covts_rasters",
                              covts_names = c("srtm", "slope", "vrm3"),
                              return_sf = TRUE, extract_method = "merge")) %>% 
    future_map2(trk_year[keep], ~ extractCovts(.x,
                              loadCovtsPath = "R/functions/loadCovtsRasters.R",
                              extractCovtPath = "R/functions/extractCovt.R",
                              covts_path = "data/working/covts_rasters/modis",
                              covts_names = paste0(c("NDVI_doy"), .y),
                              return_sf = FALSE, extract_method = "stack"))

# Load protected areas
pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")


# Calculate intersection between locations and protected areas
pa_int <- use_rdm %>%
    future_map2(test_data$colony[keep], ~st_as_sf(.x, coords = c("x2_", "y2_"), remove = FALSE, crs = crs(.y))) %>%
    future_map(~st_transform(.x, crs = 4326)) %>%
    future_map(~st_intersects(.x, pa, sparse = FALSE))

# Create a protected areas covariate
use_rdm <- use_rdm %>%
    future_map2(pa_int, ~mutate(.x, prot_area = .y)) %>%
    future_map(~mutate(.x, prot_area = if_else(prot_area == FALSE, 0, 1)))

# Remove intersection
rm(pa_int)

# Create time of day variable
# Create log of step length and cosine of turning angle. These are used later in the mov. model
use_rdm <- use_rdm %>% 
    future_map(~mutate(.x, hourday = lubridate::hour(t1_),
                       ttnoon = 12 - hourday,
                       ttnoon_sq = (ttnoon)^2,
                       log_sl = if_else(sl_ > 0, log(sl_), log(min(.$sl_[.$sl_ >0]))), # Otherwise model complains about infinite predictor
                       cos_ta = cos(ta_)))

# Standardize covariates
use_rdm_st <- use_rdm %>% 
    future_map(~mutate(.x, NDVI_mean = rowMeans(dplyr::select(., which(str_detect(names(.), "NDVI")))))) %>% 
    future_map(~ mutate(.x, across(.cols = c("srtm", "slope", "vrm3", "dist", "NDVI_mean"), scale)))



# Fit model ---------------------------------------------------------------

ssf_fits <- use_rdm_st %>% 
    future_map(~ fit_issf(case_ ~ srtm + slope + vrm3 + dist + prot_area + NDVI_mean +
                             sl_ + sl_:ttnoon + sl_:ttnoon_sq +
                             log_sl + log_sl:ttnoon + log_sl:ttnoon_sq +
                             cos_ta + cos_ta:ttnoon + cos_ta:ttnoon_sq +
                             strata(step_id_), data = .x))


    

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
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist", "prot_area", "NDVI_mean")) %>% 
    ggplot() +
    geom_pointrange(aes(x = variable, y = coef, 
                        ymin = coef - `se(coef)`, 
                        ymax = coef + `se(coef)`,
                        colour = age)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(bird_id ~ .)

ssf_coeff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist", "prot_area", "NDVI_mean")) %>% 
    ggplot() +
    geom_pointrange(aes(x = bird_id, y = coef, 
                        ymin = coef - `se(coef)`, 
                        ymax = coef + `se(coef)`,
                        colour = age)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(variable ~ ., scales = "free")

# Save model outputs ------------------------------------------------------

# Combine fit and data
ssf_fits <- tibble(
    bird_id = test_data$bird_id[keep],
    fits = ssf_fits,
    data = use_rdm_st
)

# Save
write_rds(ssf_fits, "output/ssf_fit_multi.rds")




# Random effects test -----------------------------------------------------

library(glmmTMB)

# Prepare data
glmm_data <- use_rdm_st %>% 
    future_map2(test_data$bird_id[keep], ~mutate(.x, bird_id = .y)) %>%
    future_map(~dplyr::select(.x, srtm , slope , vrm3 , dist , prot_area , NDVI_mean,
                              ttnoon, ttnoon_sq, sl_, log_sl, cos_ta,
                              case_, step_id_, bird_id)) %>% 
    do.call("rbind",.) %>% 
    left_join(dplyr::select(db, bird_id, age), by = "bird_id") %>% 
    mutate(step_id_ = paste(bird_id, step_id_, sep = "_"))

# scale step lengths
glmm_data <- glmm_data %>% 
    mutate(sl_sc = scale(sl_))

# Make response a factor
glmm_data <- glmm_data %>% 
    mutate(case_fct = if_else(case_ == TRUE, 1, 0)) %>% 
    mutate(case_fct = factor(case_fct))
           
# ssf_model <- glmmTMB(case_ ~ -1 + (1:age|step_id_) +
#                          (srtm + slope + vrm3 + dist + prot_area + NDVI_mean)*age + 
#                          sl_sc + sl_sc:(ttnoon + ttnoon_sq) + 
#                          (-1 + slope + vrm3 + dist + prot_area + NDVI_mean + 
#                               sl_sc + sl_sc:(ttnoon + ttnoon_sq)|bird_id),
#                      family = poisson, data = glmm_data, doFit = FALSE,
#                      control = glmmTMBControl(parallel = 4)) 

ssf_model <- glmmTMB(case_fct ~ -1 + srtm + slope + dist:age +
                         prot_area + NDVI_mean +
                         sl_sc + sl_sc:(ttnoon + ttnoon_sq) + 
                         (0 + srtm|bird_id) + (0 + slope|bird_id) + (0 + dist|bird_id) +
                         (0 + prot_area|bird_id) + (0 + NDVI_mean|bird_id) +
                         (0 + sl_sc|bird_id) + (0 + sl_sc:ttnoon|bird_id) + (0 + sl_sc:ttnoon_sq|bird_id) +
                         (1|step_id_),
                     family = poisson, data = glmm_data, doFit = FALSE)

nrm <- length(ssf_model$parameters$theta)
ssf_model$parameters$theta[nrm] = log(1e3)

ssf_model$mapArg = list(theta = factor(c(seq(1,nrm-1,1), NA)))

#ssf_model$mapArg = list(theta = factor(NA))
startime <- Sys.time()
ssf_fit_rm <- glmmTMB::fitTMB(ssf_model) 
endtime <- Sys.time()
endtime - startime

summary(ssf_fit_rm)

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr

# Effects
ggeffects::ggpredict(ssf_fit_rm, terms = "slope", type = "fe", back.transform = FALSE)


# Random effects
rmeff <- ranef(ssf_fit_rm)

# Fixed effects
fxeff <- fixef(ssf_fit_rm)

# Individual effects
ideff <- sweep(rmeff$cond$bird_id, 2, fxeff$cond, FUN = "+") %>% 
    mutate(bird_id = rownames(.)) %>% 
    gather(variable, estimate, -bird_id)

# Plot
ideff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist", "NDVI_mean")) %>% 
    ggplot() +
    geom_point(aes(x = variable, y = estimate)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed")

ideff %>% 
    filter(variable %in%  c("srtm", "slope", "vrm3", "dist", "prot_area", "NDVI_mean")) %>% 
    ggplot() +
    geom_point(aes(x = bird_id, y = estimate)) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    facet_grid(variable ~ ., scales = "free")

