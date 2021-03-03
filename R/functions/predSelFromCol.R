predSelFromCol <- function(map_code, colony, model, age, covts, output_res,
                           colony_all = colony_all, sfs = sfs){
   
   # Load necessary covariates (REVIEW IF NECESSARY) -------------------------
   
   # Load protected areas
   pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp", quiet = T)
   
   # Habitat metadata
   hab_meta <- read_csv("data/working/copernicus_codes.csv", col_types = cols())
   
   # Load Rcpp distance calculation function to speed up computation
   # Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")
   
   
   # Prepare covariates ------------------------------------------------------
   
   gc(verbose = F)
   
   # Define a UTM projection
   ct <- st_coordinates(colony)
   
   prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                     " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
   
   # Define a zone for the colony
   colony <- colony %>% 
      mutate(zone = case_when(lon > 24 & lat > -27.5 ~ 1,
                              lon > 24 & lat < -27.5 ~ 2,
                              lon < 24 & lat < -27.5 ~ 3,
                              lon < 24 & lat > -27.5 ~ 4))
   
   # Reproject colonies and sfs
   colony_all <- colony_all %>% 
      st_transform(crs = prjmerc)
   
   sfs <- sfs %>% 
      st_transform(crs = prjmerc)
   
   colony <- colony %>% 
      st_transform(crs = prjmerc)
   
   
   # Predict at map code -----------------------------------------------------
   
   # What rasters do we need? We don't paste land use because it is a single raster
   toload <- paste(rcovts, map_code, sep = "_")
   
   temp_covts <- list.files("output/pred_rasters/temp_covts")
   
   # Run the following only if it has not been ran before
   if(!any(str_detect(temp_covts, map_code))){
      
      # Load topo
      rr <- stack(paste0("data/working/covts_rasters/", toload[1:3], ".tif"))
      
      # Set to target resolution
      rr <- aggregate(rr, fact = output_res/res(rr)[1], fun = mean)
      
      # Load land Cover
      rlc <- st_bbox(rr) %>% 
         st_as_sfc() %>% 
         st_sf() %>% 
         loadCovts(covts_path = "data/working/covts_rasters/copernicus", covt = covts$rcovts[4])
      
      # Crop land cover (keep only extents that overlap with topographic layers)
      rlc <- rlc[!map_lgl(rlc, ~is.null(intersect(extent(.x), extent(rr))))]
      
      lc <- merge(stack(rlc))
      lc <- crop(lc, rr)
      
      # Set to target resolution
      lc <- resample(lc, rr, method = "ngb")
      
      names(lc) <- "land_cover"
      
      # Add to stack
      rr <- stack(rr, lc)
      
      rm(lc)
      
      # Rasterize protected areas -----------------------------------------------
      
      rpa <- pa %>% 
         st_crop(rr[[1]]) %>% 
         rasterize(rr[[1]], field = 1, background = 0)
      
      names(rpa) <- "prot_area"
      
      # add to covariates list
      rr <- stack(rr, rpa)
      
      rm(rpa)
      
      
      # Create a distance to SFS raster ----------------------------------------
      
      # Create a matrix of coordinates from one of the rasters in the stack
      temp <- rr[[1]] %>% 
         as.data.frame(xy = T) %>% 
         st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
         st_transform(crs = prjmerc) %>% 
         st_coordinates()
      
      # Extract transformed target colony coordinates
      x <- st_coordinates(sfs)
      
      # Calculate distances
      dist <- minDist_cpp(temp, x)
      
      # Allocate values to raster
      rdist_sfs <- rr[[1]]
      rdist_sfs <- setValues(rdist_sfs, dist)
      
      names(rdist_sfs) <- "dist_sfs"
      
      # combine with the other covariates
      rr <- stack(rr, rdist_sfs)
      
      rm(rdist_sfs)
      
      # AT THIS POINT I CAN SAVE THE RASTER STACK FOR USING IN OTHER COLONIES
      writeRaster(rr, paste0("output/pred_rasters/temp_covts/covts_", map_code, ".grd"),
                  format = "raster", overwrite = T)
      
   } else {
      
      print(paste("loading", map_code, "from disk"))
      
      # Read covariates stack from disk
      rr <- stack(paste0("output/pred_rasters/temp_covts/covts_", map_code, ".grd"))
      
      # Create a matrix of coordinates from one of the rasters in the stack
      temp <- rr[[1]] %>% 
         as.data.frame(xy = T) %>% 
         st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
         st_transform(crs = prjmerc) %>% 
         st_coordinates()
   }
   
   
   # Fix habitat codes -------------------------------------------------------
   
   hab <- as.data.frame(rr$land_cover, xy = T) %>% 
      rename(map_code = 3)
   
   # Reclassify
   hab <- hab %>% 
      left_join(hab_meta, by = c("map_code" = 'Map code'))
   
   rr$land_cover <- setValues(rr$land_cover, factor(hab$class_code))
   
   
   # Create a distance to colony raster --------------------------------------
   
   # Extract transformed target colony coordinates
   x <- st_coordinates(colony)
   
   # Calculate distances
   dist <- minDist_cpp(temp, x)
   
   # Calculate the log of distance to colony
   #dist <- log(dist)
   
   # Allocate values to raster
   rdist_col <- rr[[1]]
   rdist_col <- setValues(rdist_col, dist)
   
   names(rdist_col) <- "dist_col"
   
   # combine with the other covariates
   rr <- stack(rr, rdist_col)
   rm(rdist_col)
   
   
   # Create a distance to any colony raster --------------------------------------
   
   # We need to remove the central colony
   col_sel <- colony_all %>% 
      filter(name != colony$name)
   
   # Extract transformed target colony coordinates
   x <- st_coordinates(col_sel)
   
   # Calculate distances
   dist <- minDist_cpp(temp, x)
   
   # Allocate values to raster
   rdist_col_any <- rr[[1]]
   rdist_col_any <- setValues(rdist_col_any, dist)
   
   names(rdist_col_any) <- "dist_col_any"
   
   # combine with the other covariates
   rr <- stack(rr, rdist_col_any)
   
   rm(rdist_col_any)
   
   
   # Predict from rasters ----------------------------------------------------
   
   names(rr)
   
   # Create data frame
   rr_df <- as.data.frame(rr)
   
   # We can now get rid of all the rasters but one to free up memory
   rr <- rr[[1]]
   
   # Add and modify variables necessary to predict from model
   vv
   names(rr_df)
   
   names(rr_df)[str_detect(names(rr_df), "srtm")] <- "elev"
   names(rr_df)[str_detect(names(rr_df), "vrm")] <- "rugg"
   names(rr_df)[str_detect(names(rr_df), "slope")] <- "slope"
   names(rr_df)[str_detect(names(rr_df), "land_cover")] <- "land_cover"
   
   rr_df <- rr_df %>% 
      mutate(case = 1, sl_ = 0, res = 0, ttnoon = 0, ttnoon_sq = 0,
             bird_id = "pred_id", step_id_ = "pred_step",
             subad = if_else(age == "subad", 1, 0),
             juv = if_else(age == "juv", 1, 0), 
             z_2 = if_else(colony$zone == 2, 1, 0),
             z_3 = if_else(colony$zone == 3, 1, 0),
             z_4 = if_else(colony$zone == 4, 1, 0)) %>% 
      mutate(j = 1) %>% 
      spread(land_cover, j, fill = 0)
   
   # Standardize covariates:
   
   # Extract scaling factors
   cnt <- lapply(model$frame, attr, "scaled:center")
   sds <- lapply(model$frame, attr, "scaled:scale")
   
   cnt <- cnt[!sapply(cnt, is.null)]
   sds <- sds[!sapply(sds, is.null)]
   
   # Scale new data frame
   sc_vars <- rr_df %>% 
      dplyr::select(names(sds))
   
   tryCatch(
      sc_vars <- sweep(sc_vars, 2, unlist(cnt), "-"),
      error = function(cond) {
         message("No centering!!! Perhaps variables were not centred?")
      })
   
   tryCatch(
      sc_vars <- sweep(sc_vars, 2, unlist(sds), "/"),
      error = function(cond) {
         message("No scaling!!! Perhaps variables were not scaled? Unlikely!")
      })
   
   rr_df <- cbind(rr_df %>% 
                     # mutate(case_num = if_else(case_ == TRUE, 1, 0)) %>% 
                     dplyr::select(all_of(vv)) %>% 
                     dplyr::select(-c(names(sds))),
                  sc_vars)
   
   # Predict from model
   rm(dist, sc_vars, temp, sds) # To save memory
   gc(verbose = F)
   
   pred_sel <- predict(model, newdata = rr_df, se.fit = F, re.form = NA, allow.new.levels = T)
   
   rr_df$pred_sel <- pred_sel
   
   # Put results in a raster
   r_sel <- rr
   r_sel <- setValues(r_sel, exp(2*pred_sel))
   names(r_sel) <- "pred_sel"
   
   # Save raster
   exportname <- paste0("output/pred_rasters/1_pred_map_col/sel_", age, "_",  map_code,
                        "_", colony$id, ".tif")
   
   writeRaster(r_sel, filename = exportname, overwrite = TRUE)
   
}
