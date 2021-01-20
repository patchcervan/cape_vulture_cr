# 14-10-2020

# In this script we measure distance to known roosts

library(tidyverse)
library(sf)
library(raster)
library(Rcpp)

rm(list = ls())

# Read in data
roost <- read_csv("data/raw/CV_Colonies_20200601.csv")


# Keep only roosts and fix names
roost <- roost %>% 
  dplyr::select(ID_key, latitude, longitude, ColonyType) %>% 
  rename_all(tolower) %>% 
  filter(str_detect(colonytype, "Roost"))


# Create raster layer with distance to roost -------------------------

# Create Rcpp function for calculating distances (much faster than R)
cppFunction('NumericVector minDist_cpp(NumericMatrix r, NumericMatrix x) {
  int n = r.nrow();
  int m = x.nrow();
  
  NumericVector d(n);
  
  for(int i = 0; i < n; ++i) {
    NumericVector cc = r(i,_);
    NumericVector dd(m);
    
    for(int j = 0; j < m; ++j){
        dd[j] = sqrt(sum(pow(cc - x(j,_), 2.0)));
    }

    d[i] = min(dd);
  }
  return d;
}')


# Create a matrix of coordinates from the restaurant layer
roost <- st_as_sf(roost, coords = c("longitude", "latitude"), crs = 4326)

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
  
  # Read in template
  temp <- raster(targetfile)
  
  # Downscale raster to speed up computation
  temp <- raster(extent(temp), ncol = ncol(temp)/10, nrow = nrow(temp)/10)
  
  # Reproject restaurants and raster template
  ct <- apply(as.matrix(extent(temp)), 1, mean)
  
  prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                    " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  roost <- st_transform(roost, crs = prjmerc)
  
  x <- st_coordinates(roost)
  
  # Transform raster into matrix of coordinates
  r <- as.data.frame(temp, xy = T)
  r <- st_as_sf(r, coords = c("x", "y"), crs = 4326)
  r <- st_transform(r, crs = prjmerc)
  
  r <- st_coordinates(r)
  
  dist <- minDist_cpp(r, x)
  
  # Allocate values to raster
  values(temp) <- dist
  # plot(temp)
  
  # Save rasters
  writeRaster(temp, filename = str_replace(targetfile, "srtm", "dist_roost"), overwrite = T)
}