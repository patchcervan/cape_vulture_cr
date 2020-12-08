# 18-10-2020

# In this script add the variable distance to steep slopes to the
# vulture data

rm(list = ls())

library(dplyr)
library(tidyr)
library(magrittr)
library(furrr)
library(sp)
library(Rcpp)

future::plan("multiprocess")

setwd("/home/crvfra001/vults")


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/data_ssf_ready.rds")

# Load steep slopes coordinates
slp <- readRDS("data/steep_slp.rds")

# We will need the Rcpp function to calculate minimum distances
sourceCpp("functions/minDist_cpp.cpp")


# Create new variable to fill in ------------------------------------------

vults <- vults %>% 
      mutate(dist_slp = NA)

# nest by id
vults <- vults %>% 
   nest(data = !bird_id)



# Create a function to calculate distances for each bird ------------------

calcDistances <- function(x, slp){
   
   # For each bird create a spatial object
   trk <- SpatialPointsDataFrame(coords = x[,c("lon1_", "lat1_")], data = x,
                                 proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
   
   # Define projected coordinate system
   ct <- apply(coordinates(trk),2,median)
   
   tmerproj <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                      " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
   
   # subset slope coordinates
   box_lon <- range(coordinates(trk)[,1]) + c(-5, 5)
   box_lat <- range(coordinates(trk)[,2]) + c(-5, 5)
   
   slp_sel <- slp %>% 
      filter(x > box_lon[1] & x < box_lon[2],
             y > box_lat[1] & y < box_lat[2])
   
   # Make a spatial object
   slp_sel <- SpatialPoints(coords = slp_sel[,c("x", "y")],
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
   
   # Project slope coordinates
   slp_sel <- spTransform(slp_sel, CRSobj = CRS(tmerproj))
   
   # Calculate distances -----------------------------------------------------
   
   # Find closest distance to the trk points
   mindist <- minDist_cpp(as.matrix(trk@data[,c("x2_", "y2_")]), coordinates(slp_sel))
   
   # Fill in variable in original data frame
   x$dist_slp <- mindist
   
   return(x)
   
}


# Calculate distances for all birds ---------------------------------------

out <- future_map(vults$data, ~calcDistances(.x, slp))

# Bind results
out <- do.call("rbind", out)

# unnest original data frame and transfer distances
vults <- vults %>% 
   unnest(data)

# transfer distances
vults$dist_slp <- out$dist_slp


# Save data ---------------------------------------------------------------

saveRDS(vults, "data_ssf_ready.rds")
