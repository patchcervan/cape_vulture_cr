# With this function we prepare covariates to use in RSA/SSA

# Setting the extract_method to "merge", covariates are extracted as if 
# all rasters for the same covariate were merged into a single raster. One variable is produced.
# Setting the extract_method to "stack", covariates are extracted as if
# all rasters for the same covariate were stacked into a raster stack. Multiple variables are produced (for the same covariate).
# This is useful for time indexed covariates were we want to keep the information of all rasters.

extractCovts <- function(x, loadCovtsPath = "R/functions/loadCovtsRasters.R",
                         extractCovtPath = "R/functions/extractCovt.R",
                         covts_path = "data/working/covts_rasters",
                         covts_names = c("srtm", "slope", "vrm3"),
                         return_sf = FALSE, extract_method = c("merge", "stack")){
    
    # Make sure x and colony are spatial objects
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    source(loadCovtsPath)
    source(extractCovtPath)
    
    for(i in 1:length(covts_names)){
        
        x <- extractCovt(x, loadCovtsPath = loadCovtsPath,
                         covts_path = covts_path,
                         covt_name = covts_names[i],
                         return_sf = TRUE, 
                         extract_method = extract_method)
    }
    
    return(if(return_sf == FALSE) st_drop_geometry(x) else x)
    
}