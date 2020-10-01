
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

# Create a spatial object nested by bird
test_data <- test_data %>% 
    dplyr::select(bird_id, datetime, dt, lon, lat) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE) %>%
    nest(data = c(-bird_id))

# Read in cross-validation results
cv_df <- read_rds("output/cv_results_tests.rds")

# Load bird data base
db <- read_csv("data/working/bird_db.csv")

# Load model fits
rsf_fits <- read_rds("output/rsf_hr_fit_ct06_ct09_ez05_ez06_ma14_ma15_mb05_mb06_na03_na06_wt07_wt18.rds")


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

# Extract coefficients
rsf_coeff <- rsf_fits$fits %>% 
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
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    geom_pointrange(aes(x = bird_id, y = Estimate, 
                        ymin = Estimate - `Std. Error`, 
                        ymax = Estimate + `Std. Error`,
                        colour = age)) +
    facet_grid(variable ~ ., scales = "free")

# Explore covariates
source("R/functions/loadCovtsRasters.R")

proj <- test_data %>% 
    filter(bird_id == "ct09") %>% 
    select(tmerproj) %>% 
    unnest(cols = c(tmerproj)) %>% 
    pull(tmerproj)

ct09 <- rsf_fits %>% 
    filter(bird_id == "ct09") %>% 
    unnest(cols = c(bird_id, data)) %>% 
    st_as_sf(coords = c("x_", "y_"), crs = proj, remove = F) %>% 
    st_transform(crs = 4326)

srtm <- loadCovts(ct09, covt = "srtm")

plot(srtm[[1]])
plot(st_geometry(ct09), add = T)

srtm <- crop(srtm[[1]] , ct09)

plot(srtm)
plot(st_geometry(filter(ct09, case_ == TRUE)), add = T)


slope <- loadCovts(ct09, covt = "slope")
slope <- crop(slope[[1]] , ct09)

plot(slope)
plot(st_geometry(filter(ct09, case_ == TRUE)), add = T)
