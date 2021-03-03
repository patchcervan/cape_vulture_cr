# 04-02-2021

# In this script we predict vulture utilization distribution from an SSA and 
# breeding colonies

rm(list = ls())

library(tidyverse)
library(glmmTMB)
library(sf)
library(raster)
library(Rcpp)

# In case we want to run outside RStudio uncomment:
setwd("/media/pachorris/DATA/Documentos/mis_trabajos/Academic/cape_vulture_cr")


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read_csv("data/working/colony_data_all_upt.csv")

# Load restaurant data
sfs <- read_csv("data/working/restaurants.csv")
# 
# # Load protected areas
# pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp")
# 
# # Habitat metadata
# hab_meta <- read_csv("data/working/copernicus_codes.csv")

# Model fit
ssf_fit <- readRDS("output/ssf_fit_rm.rds")

# Load Rcpp distance calculation function to speed up computation
Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")


# Define prediction variables --------------------------------------------

# Variables necessary for prediction
vv <- all.vars(ssf_fit$call$formula)

# Of these which ones we can extract from rasters (we need to use the name that
# appears in the stored files - elev = srtm0, rugg = vrm3, land_cov = LC100)
rcovts <- c("srtm0", "slope", "vrm3", "LC100_global_v3.0.1_2019")

# distances to focal sites
dcovts <- c("dist_col", "dist_sfs", "dist_col_any")

# extracted from shapefiles
scovts <- c("prot_area")

covts <- list(rcovts = rcovts, dcovts = dcovts, scovts = scovts)


# Prepare data and variables ----------------------------------------------

# Make colonies a spatial object
colony_all <- st_as_sf(colony_all, coords = c("lon", "lat"), crs = 4326, remove = F)

# Make sfs a spatial object
sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F)


# Define prediction area --------------------------------------------------

pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>% 
   st_as_sf() %>%
   filter(NAME_1 == "Western Cape")

# # Subset generating colonies and roosts to 300 km around prediction area
# colony_sel <- st_intersection(colony_all, st_geometry(st_buffer(pred_area, dist = 3)))


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))

# col_to_pred %>% dplyr::select(-names_old) %>% print(n = Inf)


# Select age --------------------------------------------------------------

age <- "ad"


# Find map codes ----------------------------------------------------------

source("R/functions/findMapCodes.R")

map_codes <- findMapCodes(pred_area)


# PREDICT FROM MODEL AND COLONIES -----------------------------------------

# Source prediction function
source("R/functions/predSelFromCol.R")

for(i in seq_along(map_codes)){
   for(j in 1:nrow(col_to_pred)){

      print(paste("Map", i, "colony", j))
      
      # Select colony
      col_target <- slice(col_to_pred, j)
      
      # Skip if raster for map code, colony and age is present
      if(list.files("output/pred_rasters/1_pred_map_col/", pattern = "tif$") %>% 
         str_detect(paste(age, map_codes[i], col_target$id, sep = "_")) %>% 
         any){ next }
      
      # Predict (printing output)
      print( predSelFromCol(map_codes[i], col_target, ssf_fit, age, covts, output_res = 0.01,
                            colony_all = colony_all, sfs = sfs) )
      
   }
}



# I think the rest of the code is already implemented in the following scripts

# # SUM RASTERS AND SCALE BY COLONY SIZE ------------------------------------
# 
# # Load the name of all raster files
# rasterdir <- "output/pred_rasters/"
# rfiles <- list.files(paste0(rasterdir, "1_pred_map_col/"), pattern = ".tif$")
# 
# # Extract raster of the desired age
# age <- "ad"
# rfiles <- rfiles[str_detect(rfiles, age)]
# 
# # Isolate unique raster codes
# r_ids <-  rfiles %>%
#       str_sub(8, -14) %>%
#       unique()
# 
# # For all raster codes (locations)
# for(i in seq_along(r_ids)){
# 
#    print(i)
# 
#    # Identify all rasters at one location
#    r_id_files <- rfiles[str_detect(rfiles, r_ids[i])]
# 
#    # Select first colony
#    col_sel <- col_to_pred$id[1]
# 
#    # Load raster for the location and first colony
#    rr <- raster(paste0(rasterdir, "1_pred_map_col/", r_id_files[str_detect(r_id_files, col_sel)]))
# 
#    # Scale by number of birds
#    count <- paste0("avg_", age)
#    rr <- rr * col_to_pred[[1,count]]
# 
#    # Load the rasters of the remaining colonies, scale by number of birds and sum up
#    for(j in 2:nrow(col_to_pred)){
#       col_sel <- col_to_pred$id[j]
#       temp <- raster(paste0(rasterdir, "1_pred_map_col/", r_id_files[str_detect(r_id_files, col_sel)]))
#       rr <- rr + (temp * col_to_pred[[j, count]])
#    }
# 
#    exportname <- paste0(rasterdir, "2_pred_map_scaled/scl_", r_ids[i], "_", age, ".tif")
# 
#    writeRaster(rr, exportname, overwrite = T)
# }
# 
# 
# 
# # CROP TO PREDICTION AREA -------------------------------------------------
# 
# # Load the name of all raster files
# rasterdir <- "output/pred_rasters/"
# rfiles <- list.files(paste0(rasterdir, "2_pred_map_scaled/"), pattern = ".tif$")
# 
# # Extract raster of the desired age
# age <- "ad"
# rfiles <- rfiles[str_detect(rfiles, age)]
# 
# # Load each raster file and crop to prediction area
# for(i in seq_along(rfiles)){
#    
#    r <- raster(paste0(rasterdir, "2_pred_map_scaled/", rfiles[i]))
#    
#    if(!is.null(intersect(extent(r), extent(pred_area)))){
#       r <- crop(r, pred_area)
#    } else {
#       next
#    }
#    
#    writeRaster(r, paste0(rasterdir, "2_pred_map_scaled/eastern_cape/", rfiles[i]), overwrite = T)
# }
