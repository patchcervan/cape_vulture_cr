# This function takes a track (spatial object) and looks for those
# rasters in a folder given by covts_path that intersect with it

loadCovts <- function(x, covts_path = "data/working/covts_rasters", covt = "srtm"){
    
    require(raster)
    
    if(crs(x, asText = T) != "+proj=longlat +datum=WGS84 +no_defs") warning("CRS seems to be different from lat-lon WGS84")

    # Extract raster file names
    rfiles <- list.files(covts_path, pattern = glob2rx(paste0("*",covt,"*.tif")))
    
    # Create a list of rasters with those rasters that intersect the spatial object
    rr <- vector("list", length = length(rfiles))
    
    for(r in 1:length(rfiles)){
        
        # Read in raster
        rtemp <- raster(paste0(covts_path,"/",rfiles[r]))
        
        # Save if intersects else NA
        rr[[r]] <- if(!is.null(raster::intersect(extent(rtemp), extent(x)))) rtemp else NA
    }
    
    # Remove NAs from raster list
    rr <- rr[!is.na(rr)]
    
    return(rr)
}