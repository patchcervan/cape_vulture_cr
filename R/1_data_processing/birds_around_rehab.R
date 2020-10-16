# 14-10-2020

# In this script, we explore whether some birds do not get far
# from their release place. Some birds were rehabilitated and
# might not display the same behaviour as wild birds.

rm(list = ls())

library(tidyverse)
library(sf)

# Set rehab centre location
rehab <- st_as_sf(tibble(y = -25.711257, x = 27.952947), coords = c("x", "y"), crs = 4326, remove = F, dim = "XY")

# Make a 10 km radius buffer
rehab_10k <- st_buffer(rehab, dist = 0.1)

# List birds in the database
trk_files <- list.files("data/working/bird_tracks/fit_ready", pattern = ".csv")

static_birds <- tibble(bird_id = character(length(trk_files)),
                       rate = double(length(trk_files)))

for(i in 1:length(trk_files)){
    
    # i <- 1
    
    # Load one bird
    trk <- read_csv(paste0("data/working/bird_tracks/fit_ready/", trk_files[i]), col_types = cols())
    
    id_sel <- unique(trk$bird_id)
    
    # Make track spatial
    trk <- st_as_sf(trk, coords = c("x_", "y_"), crs = 4326, remove = F, dim = "XY")
    
    int <- st_intersects(trk, rehab_10k, sparse = F)
    
    rate <- sum(int) / nrow(int)
    
    if(rate > 0.1){
        plot(st_geometry(trk), axes = T)
        plot(st_geometry(rehab_10k), add = T)
        title(id_sel)
    }
    
    static_birds$bird_id[i] <- id_sel
    static_birds$rate[i] <- rate
       
}
    