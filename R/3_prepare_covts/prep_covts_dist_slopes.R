# 01-12-2020

# In this script we first define those slopes that are considered steep.
# Then we calculate distance to them.

library(raster)
library(tidyverse)
# library(furrr)

rm(list = ls())

# List all slope raster files
rfiles <- list.files("data/working/covts_rasters", pattern = "slope")

# Create a data frame to store results
out <- tibble(x = numeric(), y = numeric(), slp = numeric())

# Iterate through files
for(i in 1:length(rfiles)){

    # Select a target slope raster to process
    steep <- raster(paste0("data/working/covts_rasters/", rfiles[i]))
    
    # Identify steep slopes (NA anything less steep)
    steep[steep < 0.3] <- NA
    
    # Remove small clumps of steep slopes
    rc <- clump(steep)

    # get frequency table
    f <- freq(rc)

    # save frequency table as data frame
    f <- as.data.frame(f)

    # ID's to be removed
    excludeID <- f$value[which(f$count <= 9)]

    # make a new raster to be sieved
    steep_cl <- rc

    # assign NA to all clumps whose IDs are found in excludeID
    steep_cl[rc %in% excludeID] <- NA

    # assign 1 to all pixels of steep slope
    steep_cl[!is.na(values(steep_cl))] <- 1

    steep_df <- as.data.frame(steep_cl, xy = T) %>% 
        rename(slp = 3) %>% 
        filter(!is.na(slp))

    # plot(steep_cl)
    rm(f, rc, steep, steep_cl)
    
    out <- rbind(out, steep_df)
    
    print(i)
}

write_rds(out, "data/working/steep_slp.rds")
