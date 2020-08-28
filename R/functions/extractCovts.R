# With this function we prepare covariates to use in RSA/SSA around a colony

extractCovts <- function(x, loadCovtsPath = "model_tests/functions/loadCovtsRasters.R",
                         covts_names = c("srtm", "slope", "vrm3")){
    
    # Make sure x and colony are spatial objects
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    source(loadCovtsPath)
    
    for(i in 1:length(covts_names)){
        
        # Load rasters
        rr <- loadCovts(x, covts_path = "data/working/covts_rasters", covt = covts_names[i])
        
        # Extract covariates ------------------------------------------------------
        
        # Create a column for the new covariate
        x <- x %>% 
            mutate(!!covts_names[[i]] := as.double(NA))
        
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
            x[keep, covts_names[[i]]] <- covts
        }
    }
    
    return(st_drop_geometry(x))
    
}