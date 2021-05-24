prepCovtsPredict <- function(av_area, year, colony, topo_covts, 
                             loadCovtsPath = "R/functions/loadCovtsRasters.R",
                             covts_orig = "data/working/covts_rasters/modis"){
    
    source(loadCovtsPath)
    
    rr <- vector("list", length = length(topo_covts) + 3)
    names(rr) <- c(topo_covts, "dist", "prot_area", "NDVI_mean")
    
    # Prepare topographical covariates
    future::plan("multiprocess")
    for(i in 1:length(topo_covts)){
        r <- loadCovts(av_area, covts_path = paste0(covts_orig, "/covts_rasters"), covt = topo_covts[i])
        r <- furrr::future_map(r, ~raster::crop(.x, av_area))
        if(length(r) > 1){
            r <- do.call(raster::merge, r)
        } else{
            r <- r[[1]]
        }
        rr[[i]] <- r
    }
    future::plan("sequential")
    
    rm(r)
    gc()
    
    # Prepare NDVI covariates
    future::plan("multiprocess")
    rr$NDVI_mean <- loadCovts(av_area, 
                              covts_path = paste0(covts_orig,"/covts_rasters/modis"),
                              covt = paste0(c("NDVI_doy"), year)) %>% 
        furrr::future_map(~raster::crop(.x, av_area)) %>% 
        raster::stack() %>% 
        calc(fun = mean, na.rm = T) %>% 
        # Make NDVI have the same resolution as topographical variables
        raster::resample(rr[[1]])
    future::plan("sequential")
    gc()
    
    # Calculate distance
    rr$dist <- raster::distanceFromPoints(rr[[1]], sf::st_coordinates(colony))
    
    # Rasterize protected areas
    rr$prot_area <- sf::st_read("data/working/WDPA_protected_areas/prot_areas_single.shp") %>% 
        st_crop(av_area) %>% 
        rasterize(rr[[1]], background = 0)
    
    return(raster::brick(rr))
}