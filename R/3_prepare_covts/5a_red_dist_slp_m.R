# In this script we take the resulting distance to slope rasters and transform
# and interpolate values to increase resolution as well as transform to units in 
# meters

rm(list = ls())

library(raster)
library(dplyr)
library(magrittr)
library(stringr)
library(furrr)

future::plan("multisession", workers = 5)

# List all slope raster files
rfiles <- list.files("data/working/covts_rasters", pattern = "dist_slp")

# Keep only files that have not been processed
procs <- list.files("data/working/covts_rasters", pattern = "dist_slp_m")

rfiles <- rfiles[!str_remove(rfiles, "srtm0_") %in% str_remove(procs, "dist_slp_m")]

# Load target resolution
r <- raster("data/working/covts_rasters/srtm0_40_15.tif")

targetres <- res(r)[1]

rm(r)

# Make a function to run in parallel
toMeterSmall <- function(fl){
      
      # Select a target raster to process
      r <- raster(paste0("data/working/covts_rasters/", fl))
      
      # Change raster resolution
      r <- disaggregate(r, fact = res(r)[1]/targetres, method = "bilinear")
      
      # Transform to meters
      r <- r * 111000
      
      # Save rasters
      writeRaster(r, filename = paste0("data/working/covts_rasters/",
                                       str_replace(fl, "dist_slp", "dist_slp_m")), overwrite = T)
      
}

future_map(rfiles, ~toMeterSmall(.x))

future::plan("sequential")