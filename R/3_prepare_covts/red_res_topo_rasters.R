# 12-03-2021

# In this script we reduce the resolution of the rasters related to topography to 0.01 degrees:

# Elevation
# Slope
# Vector Ruggedeness Measure

library(raster)
library(tidyverse)
library(furrr)

future::plan("multisession")

rm(list = ls())

# Set target resolution
output_res = 0.01

# List all elevation files
rfiles <- list.files("data/working/covts_rasters", pattern = ".tif$")


# Process elevation files -------------------------------------------------

topofiles <- rfiles[str_detect(rfiles, "srtm0")]

for(i in seq_along(topofiles)){
    
    file_sel <- topofiles[i]
    
    r <- raster(paste0("data/working/covts_rasters/", file_sel))
    
    # Set to target resolution
    r <- aggregate(r, fact = output_res/res(r)[1], fun = mean)
    
    # Save raster with new resolution
    writeRaster(r, paste0("data/working/covts_rasters/topo_res01/res01_", file_sel), overwrite = T)
}


# Process slope files -------------------------------------------------

topofiles <- rfiles[str_detect(rfiles, "slope")]

for(i in seq_along(topofiles)){
    
    file_sel <- topofiles[i]
    
    r <- raster(paste0("data/working/covts_rasters/", file_sel))
    
    # Set to target resolution
    r <- aggregate(r, fact = output_res/res(r)[1], fun = mean)
    
    # Save raster with new resolution
    writeRaster(r, paste0("data/working/covts_rasters/topo_res01/res01_", file_sel), overwrite = T)
}


# Process ruggedness files -------------------------------------------------

topofiles <- rfiles[str_detect(rfiles, "vrm3")]

for(i in seq_along(topofiles)){
    
    file_sel <- topofiles[i]
    
    r <- raster(paste0("data/working/covts_rasters/", file_sel))
    
    # Set to target resolution
    r <- aggregate(r, fact = output_res/res(r)[1], fun = mean)
    
    # Save raster with new resolution
    writeRaster(r, paste0("data/working/covts_rasters/topo_res01/res01_", file_sel), overwrite = T)
}


future::plan("sequential")
