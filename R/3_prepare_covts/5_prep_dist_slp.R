rm(list = ls())

library(raster)
library(dplyr)
library(magrittr)
library(stringr)
library(Rcpp)
library(furrr)

future::plan("multisession", workers = 5)

# Load steep slopes coordinates
slp <- readRDS("data/working/steep_slp.rds")

# List all slope raster files
rfiles <- list.files("data/working/covts_rasters", pattern = "srtm0")

# Keep only files that have not been processed
procs <- list.files("data/working/covts_rasters", pattern = "dist_slp")

rfiles <- rfiles[!str_remove(rfiles, "srtm0_") %in% str_remove(procs, "dist_slp_")]

# Make a function to run in parallel
distoslp <- function(fl, slp){
      
      # Load function to calculate distances
      sourceCpp("R/functions/minDist_cpp.cpp")
      
      # Select a target raster to process
      r <- raster(paste0("data/working/covts_rasters/", fl))
      
      # Change raster resolution
      r <- aggregate(r, fact = 0.01/res(r)[1], fun = modal)
      
      # Extend extent
      ext <- extent(r) + c(-1, 1, -1, 1) * 0.5
      
      # Crop steep slopes to area within extent
      slp_sel <- slp %>% 
            filter(x > ext[1], x < ext[2],
                   y > ext[3], y < ext[4])
      
      # Extract raster coordinates
      cc <- coordinates(r)
      
      # Extract slope coordinates
      ss <- slp_sel %>% 
            dplyr::select(x, y) %>% 
            as.matrix()
      
      # Calculate distances
      st <- Sys.time()
      dist <- minDist_cpp(cc, ss)
      en <- Sys.time()
      
      # Allocate values to raster
      values(r) <- dist
      
      # Save rasters
      writeRaster(r, filename = paste0("data/working/covts_rasters/",
                                      str_replace(fl, "srtm0", "dist_slp")), overwrite = T)
   
}

future_map(rfiles, ~distoslp(.x, slp))

future::plan("sequential")