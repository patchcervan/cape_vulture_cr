createTempCovtRaster <- function(map_code, covts, output_res,
                              covtsdir, outputdir, overwrite = FALSE){
   if(overwrite == FALSE){
      tempcovts <- list.files(outputdir, pattern = ".gri")
      tempcovts <- tempcovts[nchar(tempcovts) == 15]
      tempcovts <- str_sub(tempcovts, 7,-5)
      
      # Remove those maps that are already prepared and prepare the rest
      if(any(str_detect(map_code, tempcovts))){
         stop(paste("map", map_code, "already on disk. Set overwrite == TRUE to overwrite"))
      }
   }
   
   # Load all rasters that intersect with area
   source("R/functions/loadCovtsRasters.R")
   
   # Load necessary covariates (REVIEW IF NECESSARY) -------------------------
   
   # Load protected areas
   pa <- st_read("data/working/WDPA_protected_areas/prot_areas_single.shp", quiet = T)
   
   # Habitat metadata
   hab_meta <- read_csv("data/working/copernicus_codes.csv", col_types = cols())
   
   # Load Rcpp distance calculation function to speed up computation
   # Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")
   
   
   # Predict at map code -----------------------------------------------------
   
   # What rasters do we need? We don't paste land use because it is a single raster
   toload <- paste(covts, map_code, sep = "_")
   
   temp_covts <- list.files(paste0(outputdir, "temp_covts"))
   
   
   # Load topo
   rr <- stack(paste0(covtsdir, toload[1:3], ".tif"))
   
   # Set to target resolution
   rr <- aggregate(rr, fact = output_res/res(rr)[1], fun = mean)
   
   # Load land Cover
   rlc <- st_bbox(rr) %>% 
      st_as_sfc() %>% 
      st_sf() %>% 
      loadCovts(covts_path = "data/working/covts_rasters/copernicus", covt = covts[4])
   
   # Crop land cover (keep only extents that overlap with topographic layers)
   rlc <- rlc[!map_lgl(rlc, ~is.null(intersect(extent(.x), extent(rr))))]
   
   lc <- merge(stack(rlc))
   lc <- crop(lc, rr)
   
   # Set to target resolution
   lc <- aggregate(lc, fact = output_res/res(lc)[1], fun = modal)
   lc <- resample(lc, rr, method = "ngb")
   
   names(lc) <- "land_cover"
   
   # Add to stack
   rr <- stack(rr, lc)
   
   # Fix levels
   hab <- as.data.frame(rr$land_cover, xy = T) %>% 
      rename(map_code = 3)
   
   # # Reclassify
   # hab <- hab %>% 
   #    left_join(hab_meta, by = c("map_code" = 'Map code'))
   # 
   # rr$land_cover <- setValues(rr$land_cover, factor(hab$class_code))
   
   rm(lc)
   
   # Rasterize protected areas -----------------------------------------------
   
   rpa <- pa %>% 
      st_crop(rr[[1]])
   
   if(nrow(rpa)>0){
      rpa <- rasterize(rpa, rr[[1]], field = 1, background = 0)
   } else {
      rpa <- setValues(rr[[1]], 0)
   }
   
   names(rpa) <- "prot_area"
   
   # add to covariates list
   rr <- stack(rr, rpa)
   
   rm(rpa)
   
   # AT THIS POINT I CAN SAVE THE RASTER STACK FOR USING IN OTHER COLONIES
   writeRaster(rr, paste0(outputdir, "covts_", map_code, ".grd"),
               format = "raster", overwrite = T)
   
}
