calcModelDists <- function(points, col_target=NULL, col_any, sfs, buffer = 1e4){
      
      if(!"sf" %in% class(points)){
            stop("Points need to be spatial objects")
      }
      
      require(Rcpp)
      
      # Function to calculate minimum distance
      sourceCpp("R/functions/minDist_cpp.cpp")
      
      # Define projection
      ct <- apply(sf::st_coordinates(points),2,median)
      
      prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                        " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
      
      crs_orig <- st_crs(points)
      
      points <- points %>% 
         st_transform(prjmerc)
      
      if(!is.null(col_target)){
         points <- points %>% 
            mutate(dist_col = as.numeric(st_distance(., st_transform(col_target, prjmerc))))
      } else {
         points <- points %>% 
            mutate(dist_col = NA)
      }

      
      
      # Find distance to any colony or roost ------------------------------------
      
      # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
      col_any <- st_as_sf(col_any, coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
            st_transform(prjmerc)
      
      # Then, we need to remove any colony or roost that is less than 10km away from the central colony
      if(!is.null(col_target)){
         col_others <- col_any %>% 
            mutate(dist_from_central = st_distance(col_any, st_transform(col_target, prjmerc))) %>% 
            # remove those that are closer than 10km
            filter(as.numeric(dist_from_central) > buffer)
      } else {
         col_others <- col_any
      }
      
      # For each year calculate distance between locations and any colony or roost
      # considering only those selected roosts and colonies that are further than 10km
      # from central colony
      points <- points %>% 
            mutate(dist_col_any = minDist_cpp(st_coordinates(.), st_coordinates(col_others)))
      
      
      # Find distance to restaurants --------------------------------------------
      
      # To create a matrix of coordinates from the sfs layer, we need to create a spatial object
      sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% 
            st_transform(prjmerc)
      
      # Then, we need to remove any colony or roost that is less than 10km away from the central colony
      if(!is.null(col_target)){
         sfs_others <- sfs %>% 
            mutate(dist_from_central = st_distance(., st_transform(col_target, prjmerc))) %>% 
            # remove those that are closer than 10km
            filter(as.numeric(dist_from_central) > buffer)
      } else {
         sfs_others <- sfs
      }
      
      # For each year calculate distance between locations and any colony or roost
      # considering only those selected roosts and colonies that are further than 10km
      # from central colony
      points <- points %>% 
            mutate(dist_sfs = minDist_cpp(st_coordinates(.), st_coordinates(sfs_others)))
      
      points <- st_transform(points, crs_orig)
      
      return(points)
}