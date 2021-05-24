# 11-03-2021

# In this script we download find the average NDVI value for each pixel each year

rm(list = ls())

library(raster)
library(tidyverse)


# Define years and months to analyse
years <- as.character(2004:2020)
months <- paste0("0", c(1:5))


# Define raster directory
rasterdir <- "data/working/covts_rasters/modis/"

# list NDVI files
rfiles <- list.files(rasterdir, pattern = ".tif")

for(i in seq_along(years)){
      
      # all files for a given year
      rfiles_sel <- rfiles[str_detect(rfiles, years[i])]
      
      # Stack all rasters for the year
      rr <- stack(map(rfiles_sel, ~raster(paste0(rasterdir, .x))))
      
      # Calculate mean
      rr_mean <- calc(rr, fun = mean, na.rm = T)
      
      # Save
      filename <- paste0("data/working/covts_rasters/mean_NDVI_", years[i],".tif")
      writeRaster(x = rr_mean, filename = filename, overwrite = T)
}