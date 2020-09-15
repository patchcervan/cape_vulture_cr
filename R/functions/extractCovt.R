# With this function we prepare a covariate to use in RSA/SSA

# Setting the extract_method to "merge", covariates are extracted as if 
# all rasters for the same covariate were merged into a single raster. One variable is produced.
# Setting the extract_method to "stack", covariates are extracted as if
# all rasters for the same covariate were stacked into a raster stack. Multiple variables are produced (for the same covariate).
# This is useful for time indexed covariates were we want to keep the information of all rasters.

extractCovt <- function(x, loadCovtsPath = "R/functions/loadCovtsRasters.R",
                         covts_path = "data/working/covts_rasters",
                         covt_name = "srtm",
                         return_sf = FALSE, extract_method = c("merge", "stack")){
    
    # Make sure x and colony are spatial objects
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    source(loadCovtsPath)
    
    # Load rasters
    rr <- loadCovts(x, covts_path = covts_path, covt = covt_name)
    
    
    # Extract covariate -------------------------------------------------------
    
    # Create a column for the new covariate
    if(extract_method == "merge"){
        var_names <- covt_name
        x <- x %>% 
            mutate(!!covt_name := as.double(NA))
        
    } else if(extract_method == "stack"){
        
        # Create as many variables as rasters in rr
        var_names <- paste(covt_name, seq_along(rr), sep = "_")
        for(n in seq_along(rr)){
            x <- x %>% 
                mutate(!!var_names[n] := as.double(NA))
        }
        
        # Stack rasters
        rr <- list(raster::stack(rr))
        
    }
    
    
    # Extract coordinaces
    cc <- st_coordinates(x)
    
    for(j in seq_along(rr)){
        
        ex <- extent(rr[[j]])
        
        keep <- which(cc[,1] >= ex[1] & cc[,1] <= ex[2] &
                          cc[,2] >= ex[3] & cc[,2] <= ex[4])
        
        subcc <- cc[keep,, drop = F]
        
        # Extract covariates
        covts <- raster::extract(rr[[j]], subcc)
        
        # Add to data frame
        x[keep, var_names] <- covts
    }
    
    return(if(return_sf == FALSE) st_drop_geometry(x) else x)
    
}