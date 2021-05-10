calcUD <- function(col_to_pred, mask_area, raster_dir, age, output_dir, multicore = NULL){
   
   require(furrr)
   
   if(!is.null(multicore)){
      future::plan("multisession", workers = multicore)
   } 
   
   # Load the name of all raster files
   rfiles <- list.files(raster_dir, pattern = ".tif$")
   
   if(length(rfiles) == 0){
      stop("The directory doesn't exist or there are no files")
   }
   
   # Extract raster of the desired age
   rfiles <- rfiles[str_detect(rfiles, age)]
   
   # Define UD calculation function
   f <- function(col_sel, mask_area.=mask_area, raster_dir.=raster_dir, rfiles.=rfiles, output_dir.=output_dir){
require("sf")
      # Identify all raster with the selected colony id
      rfiles_col <- rfiles.[str_detect(rfiles., col_sel)]
      
      # Sum the selection values across rasters
      denom <- 0
      for(j in seq_along(rfiles_col)){
         r <- raster(paste0(raster_dir., rfiles_col[j])) %>% 
            raster::mask(., mask_area.) %>% 
            raster::values()
         
         denom <- denom + sum(r, na.rm = T)
      }
      
      # Scale all rasters of the selected colony and save
      for(j in seq_along(rfiles_col)){
         rr <- raster(paste0(raster_dir., rfiles_col[j]))
         rr <- rr/denom
         
         writeRaster(rr, paste0(output_dir., str_replace(rfiles_col[j], "sel", "ud")), overwrite = T)
      }
   }
   
   # Execute for each colony
   # for(i in seq_along(col_to_pred$id)){
   #    f(col_to_pred$id[i])
   # }
   future_map(col_to_pred$id, ~f(col_sel = .x, mask_area. = as(mask_area, "Spatial")), .options = furrr_options(seed = TRUE))
   
   future::plan("sequential")
}
