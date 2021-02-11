# 14-10-2020

# In this script we prepare the layer with vulture restaurants

library(tidyverse)
# library(sf)
# library(raster)
# library(Rcpp)

rm(list = ls())

# distproj <- "+proj=lcc +lon_0=25 +lat_1=-40 +lat_2=-10"

# Read in data
rest <- readxl::read_excel("data/raw/SFSDatabase_2020_FIAO_VulPro_KZNwildlife_EWT (1).xlsx")

# Keep only relevant fields and make all names lower case
rest <- rest %>% 
    dplyr::select(SFS_Code, Structure, Closure_date, Latitude, Longitude, 
                  Status_Category, Verified_2018, last_verified = Last_Status_VerrificationDate) %>% 
    rename_all(tolower)

# There are other things than feeding sites and some feeding sites are closed.
# Keep only feeding sites that are active (according to Arjun mail, those that are
# status_category == "Active" and verified_2018 == "Y")
rest <- rest %>% 
    filter(structure == "SFS",
           status_category == "Active",
           verified_2018 == "Y")

# Latitude and longitude columns seem to be characters for some reason.
# Make them numeric
rest <- rest %>% 
    mutate(latitude = as.double(latitude),
           longitude = as.double(longitude))

# Do all restaurants have coordinates?
summary(rest)

# One hasn't. Filter it out
rest <- rest %>% 
    filter(!is.na(latitude))

# Save restaurant data
write_csv(rest, file = "data/working/restaurants.csv")

# 
# # Create raster layer with distance to restaurant -------------------------
# 
# # Create Rcpp function for calculating distances (much faster than R)
# cppFunction('NumericVector minDist_cpp(NumericMatrix r, NumericMatrix x) {
#   int n = r.nrow();
#   int m = x.nrow();
#   
#   NumericVector d(n);
#   
#   for(int i = 0; i < n; ++i) {
#     NumericVector cc = r(i,_);
#     NumericVector dd(m);
#     
#     for(int j = 0; j < m; ++j){
#         dd[j] = sqrt(sum(pow(cc - x(j,_), 2.0)));
#     }
# 
#     d[i] = min(dd);
#   }
#   return d;
# }')
# 
# 
# # Create a matrix of coordinates from the restaurant layer
# rest <- st_as_sf(rest, coords = c("longitude", "latitude"), crs = 4326)
# 
# # Create a data frame with all raster filepaths. 
# # One variable is the East coordinate, the other is the North coordinate
# rfiles <- expand.grid(39:44, 16:19) %>% 
#   rename(east = Var1, north = Var2) %>% 
#   mutate(filename = paste0("data/working/covts_rasters/srtm_", east, "_", north,".tif"))
# 
# # Keep only those files that exist
# rfiles <- rfiles %>% 
#   filter(filename %in% paste0("data/working/covts_rasters/",list.files("data/working/covts_rasters/", pattern = ".tif")))
# 
# # Iterate through files
# for(i in 1:nrow(rfiles)){
#   
#   # Select a target elevation raster to process
#   targetfile <- rfiles$filename[i]
#   
#   # Read in template
#   temp <- raster(targetfile)
#   
#   # Downscale raster to speed up computation
#   temp <- raster(extent(temp), ncol = ncol(temp)/10, nrow = nrow(temp)/10)
#   
#   # Reproject restaurants and raster template
#   ct <- apply(as.matrix(extent(temp)), 1, mean)
#   
#   prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
#                     " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#   
#   rest <- st_transform(rest, crs = prjmerc)
#   
#   x <- st_coordinates(rest)
#   
#   # Transform raster into matrix of coordinates
#   r <- as.data.frame(temp, xy = T)
#   r <- st_as_sf(r, coords = c("x", "y"), crs = 4326)
#   r <- st_transform(r, crs = prjmerc)
#   
#   r <- st_coordinates(r)
#   
#   dist <- minDist_cpp(r, x)
#   
#   # Allocate values to raster
#   values(temp) <- dist
#   # plot(temp)
#   
#   # Save rasters
#   writeRaster(temp, filename = str_replace(targetfile, "srtm", "dist_sfs"), overwrite = T)
# }