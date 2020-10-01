# 03-08-2020

# In this script we check and fix the altitude from the vulture tracking data

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)
# library(sp)
# library(spatialEco)
# library(elevatr)

# Extract elevation raster file names
elevfiles <- list.files("data/raw/srtm3_elevation", pattern = ".tif")

# Extract bird track file names
trkfiles <- list.files("data/working/bird_tracks", pattern = ".csv")

# Create a diagnosis vector. This vector will have a TRUE when all locations of a given bird
# have elevation data and FALSE when some locations are missing
diag_elev <- vector(length = length(trkfiles))

# IF WE WANT TO RUN ONLY THOSE FILES THAT PRODUCED FALSE IN DIAGNOSIS VECTOR, THEN RUN THE
# NEXT 3 LINES:
# diag_previous <- read_rds("output/diag_elev.rds")
# trkfiles <- trkfiles[!diag_previous]
# diag_elev <- vector(length = length(trkfiles))

# For each bird
for(i in 1:length(trkfiles)){
    
    # Remove Namibian birds for now
    # if(str_detect(trkfiles[i], "na")) next
    
    # Read in track
    trk <- read_csv(paste0("data/working/bird_tracks/", trkfiles[i]))
    
    # Make spatial object
    trk <- st_as_sf(trk, coords = c("lon", "lat"), crs = 4326, dim = "XY", remove = FALSE)
    
    # Create a list of elevation rasters with those rasters that intersect the bird track
    elev <- vector("list", length = length(elevfiles))
    
    for(r in 1:length(elevfiles)){
        
        # Read in raster
        elevtemp <- raster(paste0("data/raw/srtm3_elevation/",elevfiles[r]))
        
        # Save if intersects with bird track else NA
        elev[[r]] <- if(!is.null(intersect(extent(elevtemp), extent(trk)))) elevtemp else NA
    }
    
    # Remove NAs from elevation raster list
    elev <- elev[!is.na(elev)]
    
    # Create a matrix in which each column represent an elevation raster and each
    # row is a location along the bird track
    trk_elev <- matrix(NA, nrow = nrow(trk), ncol = length(elev))
    
    # Compute elevation for each point using each elevation raster
    for(r in 1:length(elev)){
        trk_elev[,r] <- extract(elev[[r]], trk)
    }
    
    # Calculate the mean measurement for each location (although there should be only one)
    trk_elev <- apply(trk_elev, 1, mean, na.rm = T)
    
    diag_elev[i] <- !anyNA(trk_elev)
    
    # Save elevation values
    trk <- trk %>% 
        mutate(elev = trk_elev)
    
    # # Extract raster values
    # trk <- trk %>% 
    #     mutate(elev = extract(elev, trk),
    #            slope = extract(slope, trk),
    #            vrm3 = extract(vrm3, trk),
    #            vrm5 = extract(vrm5, trk))
    
    # Save track with altitude data
    write_csv(st_drop_geometry(trk), paste0("data/working/bird_tracks/", trkfiles[i]))
}

# Which files have elevation missing?
trkfiles_miss <- trkfiles[which(diag_elev == FALSE)]

# Extract bird track file names (need to do this in caseb a previous diagnosis vector was loaded)
trkfiles <- list.files("data/working/bird_tracks", pattern = ".csv")

diag_elev <- !(trkfiles %in% trkfiles_miss)

# Save diagnostics
write_rds(diag_elev, path = "output/diag_elev.rds")
