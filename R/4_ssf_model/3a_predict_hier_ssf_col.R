# 3-11-2020

# In this script we estimate resource selection from a step-selection model

# We predict use around a colony

rm(list = ls())

library(tidyverse)
library(raster)
library(sf)
library(glmmTMB)



# Load data ---------------------------------------------------------------

# Load model fit
ssf_fit_rm <- read_rds("output/ssf_fit_rm.rds")

# summary(ssf_fit_rm)

# Extract bird codes
fit_bird_id <- unique(ssf_fit_rm$frame$bird_id)

# Read in roost and colony data
colony_orig <- read_csv("data/raw/ewt_cv_Colonies_20200601.csv")


# Load rasters around a colony --------------------------------------------

# Make colonies a spatial object
colony_orig <- colony_orig %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

# Select a colony
target_col <- colony_orig %>% 
      filter(ID_key == "CVC0157")

# Load function to load covariate rasters
source("R/functions/loadCovtsPredict.R")

# Define variables that we need
names(ssf_fit_rm$frame)

rcovts <- c("srtm0", "slope", "vrm3", "LC100_global_v3.0.1_")

# Create a raster stack with all covariates
rr <- vector("list", length = length(rcovts))

subdir <- paste0(rep("data/working/covts_rasters", 4), c("", "", "", "/copernicus"))

require(furrr)
future::plan("multiprocess")

for(i in 1:length(rr)){
      rr[[i]] <- loadCovtsPredict(x = target_col, buffer = 2e5, covt = rcovts[i], 
                                  loadCovtsPath = "R/functions/loadCovtsRasters.R",
                                  covts_path = subdir[i])
}

# Bring all rasters to the same (coarse ~ 1.1 km) resolution
sapply(rr, res)

rr[[4]] <- aggregate(rr[[4]], fact = 0.01/0.000992, fun = modal)

rr[1:3] <- future_map(rr[1:3], ~resample(.x, rr[[4]], method = "bilinear")) # this is the same as taking the mean

future::plan("sequential")

# Fix habitat codes

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv")

temp <- as.data.frame(rr[[4]], xy = T)

# Reclassify
temp <- temp %>% 
   left_join(hab_meta, by = c(layer = 'Map code'))

values(rr[[4]]) <- factor(temp$class_code)

hab_key <- factor(temp$class_code)


# Load non-raster covariates around colony --------------------------------

# Transform template raster into matrix of coordinates
temp <- as.data.frame(rr[[1]], xy = T)


# Create a distance to colony raster --------------------------------------

# Load Rcpp function to speed up computation
require(Rcpp)
Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")

# Reproject colony and raster template
ct <- st_coordinates(target_col)

prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                  " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

target_col <- st_transform(target_col, crs = prjmerc)

x <- st_coordinates(target_col)

temp <- st_as_sf(temp, coords = c("x", "y"), crs = 4326)

temp <- st_transform(temp, crs = prjmerc)

temp <- st_coordinates(temp)

# Calculate distances
dist <- minDist_cpp(temp, x)

# Allocate values to raster
rdist <- rr[[1]]
values(rdist) <- dist

names(rdist)

# combine with the other covariates
rr <- c(rr, list(rdist))
rm(rdist)


# Rasterize protected areas -----------------------------------------------

# Load protected areas
pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")

# Rasterize
rpa <- rasterize(pa, rr[[1]], field = 1, background = 0)

# add to covariates list
rr <- c(rr, list(rpa))

rm(rpa)


# Predict selection -------------------------------------------------------

# Create raster stack
rr <- stack(rr)
names(rr) <- c("elev", "slope", "ruggd", "land_cov", "dist_col", "prot_area")

plot(rr)

# Create data frame
rr_df <- as.data.frame(rr)

# Add and modify variables necessary to predict from model
names(ssf_fit_rm$frame)

rr_df <- rr_df %>% 
      rename(srtm = elev, vrm3 = ruggd) %>% 
      mutate(case_num = 1, z_2 = 1, z_3 = 0, z_4 = 0, sl_ = 0, ttnoon = 0, ttnoon_sq = 0,
             subad = 0, juv = 0, bird_id = "pred_id", step_id_ = "pred_step") %>% 
      mutate(i = 1) %>% 
      spread(land_cov_VALUE, i, fill = 0)


# Standardize covariates:

# Extract model covariates
keep_vars <- names(ssf_fit_rm$frame)

# Extract scaling factors
cnt <- lapply(ssf_fit_rm$frame, attr, "scaled:center")
sds <- lapply(ssf_fit_rm$frame, attr, "scaled:scale")

cnt <- cnt[!sapply(cnt, is.null)]
sds <- sds[!sapply(sds, is.null)]

# Scale new data frame
sc_vars <- rr_df %>% 
      dplyr::select(names(cnt))

sc_vars <- sweep(sc_vars, 2, unlist(cnt), "-")
sc_vars <- sweep(sc_vars, 2, unlist(sds), "/")

rr_df <- cbind(rr_df %>% 
                     # mutate(case_num = if_else(case_ == TRUE, 1, 0)) %>% 
                     dplyr::select(all_of(keep_vars)) %>% 
                     dplyr::select(-c(names(cnt))),
               sc_vars)

# step lenght (sl_) was centred but it shouldn't. Bring it back to zero
rr_df$sl_ <- 0

# Predict from model
pred_sel <- predict(ssf_fit_rm, newdata = rr_df, se.fit = T, re.form = NA, allow.new.levels = T)

# Put results in a raster
r_sel <- rr[[i]]
values(r_sel) <- exp(pred_sel$fit)/sum(exp(pred_sel$fit))
names(r_sel) <- "pred_sel"

plot(r_sel, axes = F)
title("Relative selection")

plot(log(r_sel), axes = F)
title(" Log of relative selection")

# Multiply ruggedens by 10 so that the legend fits in the plot
rr$ruggd <- rr$ruggd*10

# Plot covariates
plot(rr, axes = F)

# Put SE in a raster
r_sel_se <- rr[[i]]
values(r_sel_se) <- exp(pred_sel$se.fit)/sum(exp(pred_sel$fit))
names(r_sel_se) <- "pred_sel_se"

plot(r_sel_se, axes = F, col = rev(heat.colors(50)))
plot(log(r_sel_se))
title("Uncertainty in relative selection")
