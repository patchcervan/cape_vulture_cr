
# This function will load a raster covariate centred at the location x.
# The extent of the raster will be given by the argument av_area if provided.
# Otherwise a buffer argument may be provided to take a buffer around the colony.
# Note that the function requires x to be in latitude/longitude and therefore
# the buffer will not be accurate. It can be passed in meters rather than degrees
# and a rough conversion of 0.00001° = 1.11 m will be performed internally.

loadCovtsPredict <- function(x, av_area = NULL, buffer = NULL, covt, 
                             loadCovtsPath = "R/functions/loadCovtsRasters.R",
                             covts_path = "data/working/covts_rasters"){
      
      # x = target_col; av_area <- NULL; buffer = 1e5; covt = "srtm0"
      
      # Make sure x is a spatial object and is in a geographical reference system
      if(!"sf" %in% class(x)) stop("x is not a spatial object")
      if(crs(x, asText = T) != "+proj=longlat +datum=WGS84 +no_defs") warning("CRS seems to be different from lat-lon WGS84")
      
      # Create availability area, either by buffer or av_area. If none of these are defined, stop.
      if(!is.null(buffer)){
            buffer <- buffer * 0.00001/1.11
            av_area <- st_buffer(x, buffer)
      } else if(is.null(av_area)){
            stop("Area needs to be defined either by av_area or buffer")
      } else {
            if(!"sf" %in% class(av_area)) stop("av_area is not a spatial object")
      }
      
      source(loadCovtsPath)
      
      # Load all rasters that intersect with av_area
      r <- loadCovts(av_area, covts_path = covts_path, covt = covt)
      
      # Crop these rasters to av_area
      r <- furrr::future_map(r, ~raster::crop(.x, av_area))
      
      if(length(r) > 1){
            r <- do.call(raster::merge, r)
      } else{
            r <- r[[1]]
      }

      return(r)
}