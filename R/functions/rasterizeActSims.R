rasterizeSims <- function(col_to_pred, mapcode, raster_dir, age, scale = FALSE, output_dir, multicore = NULL){
   
   require(furrr)
   
   if(!is.null(multicore)){
      future::plan("multisession", workers = multicore)
   } 
   
   # Load the name of all raster files
   rfiles <- list.files(paste0(raster_dir, "1_pred_map_col"), pattern = ".rds$")
   
   if(length(rfiles) == 0){
      stop("The directory doesn't exist or there are no files")
   }
   
   # Extract raster of the desired age
   rfiles <- rfiles[str_detect(rfiles, age)]
   
   # Define a background raster
   r <- raster(paste0(rasterdir, "temp_covts/covts_", mapcode, ".gri"))
   
   
   # Define UD calculation function
   f <- function(col_sel, raster_dir.=raster_dir, rfiles.=rfiles, output_dir.=output_dir){
      
      require("sf")
      
      # Identify all raster with the selected colony id
      rfiles_col <- rfiles.[str_detect(rfiles., paste0(col_sel,"_"))]
      
      # Rasterize and standardize
      sims <- readRDS(paste0(raster_dir, "1_pred_map_col/", rfiles_col))
      
      # Count points per pixel
      counts <- rasterize(sims[,c("lon", "lat")], r, fun='count', background = 0)
      
      # smooth counts
      # counts <- focal(counts, w = matrix(1/9, nc = 3, nr = 3))
      
      # standardize
      # counts <- counts/nrow(sims)
      
      return(counts)
   }
   
   # Execute for each colony
   out <- future_map(col_to_pred$id, ~f(col_sel = .x), .options = furrr_options(seed = TRUE))
   
   filename <- paste0(output_dir, "ud_sims_", mapcode, ".tif")
   
   if(scale == TRUE){
      if(age == "ad"){
         for(i in 1:nrow(col_to_pred)){
            
            out[[i]] <- out[[i]] * col_to_pred$avg_ad[i]
         }
      } else {
         
         print("Assuming birds are juveniles")
         
         for(i in 1:nrow(col_to_pred)){
            
            out[[i]] <- out[[i]] * col_to_pred$avg_juv[i]
         }
      }
      
      filename <- paste0(output_dir, "ud_sims_", mapcode, "_", age, ".tif")
   }
   
   out <- Reduce("+", out)
   
   future::plan("sequential")
   
   writeRaster(out, filename = filename, overwrite = T)
}
