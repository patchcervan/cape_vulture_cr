# Convenience wrapper around SpatialEco::sp.kde.
# It takes a projected spatial object,
# computes kernel density with bandwidth bw (no need for optimization, we are only interested in maximum density point),
# creates a spatial point at location of maximum density


findColony <- function(x, coords = c("lon", "lat"), timevar = "datetime", bw = 0.1, sp_proj = 4326, plotkde = T){
    
    require(raster)
    
    # x needs to be a spatial object
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    # rename coordinates to "lon" - "lat"
    x <- x %>% 
        rename(lon = coords[1],
               lat = coords[2],
               datetime = timevar)
    
    # To find the colony, we first calculate the point with maximum density of locations
    
    # We are particularly interested in locations in early morning
    
    sunr <- x %>% 
        st_drop_geometry() %>%
        dplyr::select(lon, lat) %>% 
        as.matrix() %>% 
        maptools::sunriset(dateTime = x$datetime, POSIXct.out = T)
    
    x$ttsunr <- as.numeric(x$datetime - sunr$time)/3600
    
    # Filter locations that are within two hours of sunrise
    x <- x %>% 
        filter(abs(ttsunr) < 2)
    
    dims <- as.matrix(raster::extent(x), byrow = F)
    m <- if(str_detect(st_crs(x)$proj4string, "longlat")) 150 else 15
    nr <- m * max(1, (1 + log(abs(diff(dims[2,])))))
    nc <- m * max(1, (1 + log(abs(diff(dims[1,])))))
    
    kdens <- spatialEco::sp.kde(as(x, "Spatial"), bw = bw, 
                                newdata = as.vector(extent(x)),
                                nr = nr, nc = nc)
    
    # Extract point of maximum density
    colony <- xyFromCell(kdens, which.max(kdens))
    
    # create sf object
    colony <- st_as_sf(data.frame(x = colony[1], y = colony[2]),
                       coords = c("x", "y"), crs = sp_proj)
    
    if(isTRUE(plotkde)){
        print(raster::plot(kdens))
        print(plot(st_geometry(colony), add = T))
    }

    
    return(colony)
} 