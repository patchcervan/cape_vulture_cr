findMapCodes <- function(area){
      
      # Load all rasters that intersect with area
      source("R/functions/loadCovtsRasters.R")
      
      # Topographic
      rtopo <- loadCovts(area, covts_path = "data/working/covts_rasters", covt = "srtm0")
      
      # All these rasters may potentially be very large, so keep the codes and remove
      topo_ids <- sapply(rtopo, names)
      topo_ids <- str_remove(topo_ids, "srtm0_")
      
      rm(rtopo)
      
      return(topo_ids)
}
