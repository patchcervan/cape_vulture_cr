# 26-08-2020

# In this script we prepare the covariates rasters related to topography:

# Slope
# Vector Ruggedeness Measure

# There are two reasons to do this. First, it speeds up model fitting.
# Second, both variables need contiguous cells for the calculations and therefore
# leave out borders if calculated from single rasters.

library(raster)
library(tidyverse)
library(furrr)

rm(list = ls())

# Create a data frame with all raster filepaths. 
# One variable is the East coordinate, the other is the North coordinate
rfiles <- expand.grid(39:44, 16:19) %>% 
    rename(east = Var1, north = Var2) %>% 
    mutate(filename = paste0("data/working/covts_rasters/srtm_", east, "_", north,".tif"))

# Keep only those files that exist
rfiles <- rfiles %>% 
    filter(filename %in% paste0("data/working/covts_rasters/",list.files("data/working/covts_rasters/", pattern = ".tif")))

# Iterate through files
for(i in 1:nrow(rfiles)){

    # Select a target elevation raster to process
    targetfile <- rfiles$filename[i]
    
    # find contiguous rasters
    nexteast <- rfiles$east[i] + c(-1, 0, 1)
    nextnorth <- rfiles$north[i] + c(-1, 0, 1)
    
    nextrfiles <- expand.grid(rfiles$east[i], nextnorth) %>% 
        rbind(expand.grid(nexteast, rfiles$north[i])) %>% 
        rename(east = Var1, north = Var2) %>% 
        distinct(east, north) %>% 
        mutate(filename = paste0("data/working/covts_rasters/srtm_", east, "_", north,".tif"))
    
    
    # Keep only those rasters that exist and flag the target raster
    nextrfiles <- nextrfiles %>% 
        filter(filename %in% rfiles$filename) %>% 
        mutate(target = if_else(filename == rfiles$filename[i], 1, 0))
    
    # Load target raster
    targetr <- nextrfiles %>% 
        filter(target == 1) %>% 
        pull(filename) %>%
        raster()
    
    # Load contiguous rasters
    nextr <- map(nextrfiles[nextrfiles$target == 0,]$filename, ~raster(.x))
    
    # Extend target extent to crop contiguous rasters
    ext <- extent(targetr) + c(-1, 1, -1, 1) * 0.5
    
    # Crop contiguous rasters
    nextr <- future_map(nextr, ~crop(.x, ext))
    
    # Merge rasters
    elev <- do.call(merge, nextr)
    elev <- merge(elev, targetr)
    
    # plot(elev)
    
    # Compute covariates
    slope <- terrain(elev, "slope")
    vrm3 <- spatialEco::vrm(elev, s = 3)
    
    # Crop covariates to original extent
    slope <- crop(slope, targetr)
    vrm3 <- crop(vrm3, targetr)
    
    # Save rasters
    writeRaster(slope, filename = str_replace(targetfile, "srtm", "slope"), overwrite = T)
    writeRaster(vrm3, filename = str_replace(targetfile, "srtm", "vrm3"), overwrite = T)
}