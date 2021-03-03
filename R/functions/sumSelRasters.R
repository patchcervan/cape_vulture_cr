sumSelRasters <- function(col_to_pred, raster_dir, age, scale_sel = FALSE, output_dir){
      
      # Load the name of all raster files
      rfiles <- list.files(raster_dir, pattern = ".tif$")
      
      # Extract raster of the desired age
      rfiles <- rfiles[str_detect(rfiles, age)]
      
      # Isolate unique raster codes
      r_ids <-  rfiles %>%
            str_sub(nchar(age) + 6, -14) %>%
            unique()
      
      # Define scale factor for counts if necessary
      if(isTRUE(scale_sel)){
            count_var <- paste0("avg_", age)
            counts <- col_to_pred[[count_var]]
      } else {
            counts <- rep(1, nrow(col_to_pred))
      }
      
      # For all raster codes (locations)
      for(i in seq_along(r_ids)){
            
            print(paste(i, "out of", length(r_ids)))
            
            # Identify all rasters at one location
            r_id_files <- rfiles[str_detect(rfiles, r_ids[i])]
            
            # Select first colony
            col_sel <- col_to_pred$id[1]
            
            # Load raster for the location and first colony
            rr <- raster(paste0(raster_dir, r_id_files[str_detect(r_id_files, col_sel)]))
            
            # Scale by number of birds
            rr <- rr * counts[1]
            
            # Load the rasters of the remaining colonies, scale by number of birds and sum up
            for(j in 2:nrow(col_to_pred)){
                  col_sel <- col_to_pred$id[j]
                  temp <- raster(paste0(raster_dir, r_id_files[str_detect(r_id_files, col_sel)]))
                  rr <- rr + (temp * counts[j])
            }
            
            if(isTRUE(scale_sel)){
                  exportname <- paste0(output_dir, "scl_", r_ids[i], "_", age, ".tif")
            } else {
                  exportname <- paste0(output_dir, "nonscl_", r_ids[i], ".tif")
            }
            
            
            writeRaster(rr, exportname, overwrite = T)
      }
}