calcUD <- function(.mapcode, .col_to_pred = NULL, .age, scale = FALSE, .rasterdir, .outputdir){
      
      # Load a template raster
      r <- raster(paste0("output/pred_raster_sims/temp_covts/covts_", .mapcode, ".gri"))
   
      # r <- raster(paste0(.rasterdir, "temp_covts/covts_", .mapcode, ".gri"))
      
      rdf <- as.data.frame(r, xy = T)
      
      rdf <- rdf %>% 
            mutate(x = round(x, 3),
                   y = round(y, 3))
      
      counts <- rep(0, nrow(rdf))
      smoothcounts <- rep(0, nrow(rdf))
      
      colfiles <- list.files(paste0(.rasterdir, "col_gam"))
      colfiles <- colfiles[str_detect(colfiles, .age)]
      
      if(!scale){
         
         for(i in seq_along(colfiles)){
            
            ud_col <- readRDS(paste0(.rasterdir, "col_gam/", colfiles[i]))
            
            ud_col <- ud_col %>% 
               mutate(x = round(lon, 3),
                      y = round(lat, 3))
            
            total_count <- round(sum(ud_col$count, na.rm = T), 0)
            total_gamfit <- round(sum(ud_col$gamfit, na.rm = T), 0)
            
            coldf <- rdf %>% 
               left_join(ud_col, by = c("x", "y"))
            
            coldf$count[is.na(coldf$count)] <- 0
            coldf$gamfit[is.na(coldf$gamfit)] <- 0
            
            counts <- counts + coldf$count/total_count
            smoothcounts <- smoothcounts + coldf$gamfit/total_gamfit
         }
         
         countfile <- paste0(.outputdir, "count_", .mapcode, "_", .age, "_nosc", ".tif")
         gamfile <- paste0(.outputdir, "gamfit_", .mapcode, "_", .age, "_nosc", ".tif")
         
      } else {
         
         if(is.null(.col_to_pred)) stop("col_to_pred needs to be a dataframe with colony sizes")
         
         for(i in seq_along(colfiles)){
            
            ud_col <- readRDS(paste0(.rasterdir, "col_gam/", colfiles[i]))
            
            id_sel <- str_remove(colfiles[i], .age)
            id_sel <- str_sub(id_sel, 5, -6)
            
            col_sel <- .col_to_pred %>% 
               filter(id == id_sel)
            
            size <- case_when(.age == "ad" ~ col_sel$avg_ad,
                              .age == "juv" ~ col_sel$avg_juv)
            
            ud_col <- ud_col %>% 
               mutate(x = round(lon, 3),
                      y = round(lat, 3))
            
            total_count <- round(sum(ud_col$count, na.rm = T), 0)
            total_gamfit <- round(sum(ud_col$gamfit, na.rm = T), 0)
            
            coldf <- rdf %>% 
               left_join(ud_col, by = c("x", "y"))
            
            coldf$count[is.na(coldf$count)] <- 0
            coldf$gamfit[is.na(coldf$gamfit)] <- 0
            
            counts <- counts + (coldf$count/total_count*size)
            smoothcounts <- smoothcounts + (coldf$gamfit/total_gamfit*size)
         }
         
         countfile <- paste0(.outputdir, "count_", .mapcode, "_", .age, ".tif")
         gamfile <- paste0(.outputdir, "gamfit_", .mapcode, "_", .age, ".tif")
         
      }
      
      
      
      r <- setValues(r, counts)
      
      writeRaster(r, filename = countfile, overwrite = T)
      
      r <- setValues(r, smoothcounts)
      
      writeRaster(r, filename = gamfile, overwrite = T)
      
}
