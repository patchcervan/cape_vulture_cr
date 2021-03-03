# 22-02-2021

# In this script we estimate an utilization distribution within a defined area
# based on selection values estimated using an SSF model

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read_csv("data/working/colony_data_all_upt.csv")

# Define raster directory
rasterdir <- "output/pred_rasters/"

# Load function to sum up rasters
source("R/functions/sumSelRasters.R")


# Define prediction area --------------------------------------------------

pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>% 
   st_as_sf() %>%
   filter(NAME_1 == "Eastern Cape")


# Define age --------------------------------------------------------------

age <- "juv"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Standardize selection around colonies -----------------------------------

# The code below takes all rasters predicted from a colony and divides the 
# values by the sum of all values so that the total sum is 1.

# RUN ONLY ONCE!!!

# Load raster file names
rfiles <- list.files(paste0(rasterdir, "1_pred_map_col/"), pattern = ".tif$")

for(k in 1:nrow(col_to_pred)){

   print(k)

   # Select colony
   col_target <- slice(col_to_pred, k)

   # Identify colony rasters
   col_files <- rfiles[str_detect(rfiles, col_target$id)]

   # of the correct age
   col_files <- col_files[str_detect(col_files, age)]
   
   # Load and merge colony rasters
   rr <- raster(paste0(rasterdir, "1_pred_map_col/", col_files[1]))

   for(i in 2:length(col_files)){
      rr <- merge(rr, raster(paste0(rasterdir, "1_pred_map_col/", col_files[i])))
   }

   # Find the sum of the values of the rasters
   denom <- sum(values(rr))

   # Standardize rasters
   for(j in 1:length(col_files)){
      r <- raster(paste0(rasterdir, "1_pred_map_col/", col_files[j]))
      r <- setValues(r, values(r)/denom)
      writeRaster(r, paste0(rasterdir, "2_pred_map_col_std/", col_files[j]), overwrite = T)
   }

   rm(r, rr)
}


# Sum rasters without scaling by size -------------------------------------

sumSelRasters(col_to_pred = col_to_pred,
              raster_dir = paste0(rasterdir, "2_pred_map_col_std/"),
              age = age,
              scale_sel = FALSE,
              output_dir = paste0(rasterdir, "3_pred_map_scaled/"))


# Sum rasters and scale by colony size ------------------------------------

sumSelRasters(col_to_pred = col_to_pred,
              raster_dir = paste0(rasterdir, "2_pred_map_col_std/"),
              age = age,
              scale_sel = TRUE,
              output_dir = paste0(rasterdir, "3_pred_map_scaled/"))


# CROP TO PREDICTION AREA -------------------------------------------------

# Load the name of all raster files
rfiles <- list.files(paste0(rasterdir, "3_pred_map_scaled/"), pattern = ".tif$")

# Extract raster of the desired age
rfiles <- rfiles[str_detect(rfiles, age)]

# Load each raster file and crop to prediction area
for(i in seq_along(rfiles)){
   
   r <- raster(paste0(rasterdir,"3_pred_map_scaled/", rfiles[i]))
   
   if(!is.null(intersect(extent(r), extent(pred_area)))){
      r <- crop(r, pred_area)
   } else {
      next
   }
   
   writeRaster(r, paste0(rasterdir,"4_provinces/eastern_cape/", rfiles[i]), overwrite = T)
}
