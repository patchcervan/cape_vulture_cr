# Convenience wrapper around SpatialEco::sp.kde.
# It takes a projected spatial object,
# computes kernel density with bandwidth bw (no need for optimization, we are only interested in maximum density point),
# creates a spatial point at location of maximum density


findColony <- function(x, bw = 0.1, sp_proj = 4326, plotkde = T){
    
    # x needs to be a spatial object
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    # To find the colony, we first calculate the point with maximum density of locations
    dims <- as.matrix(extent(x), byrow = F)
    m <- if(str_detect(crs(x), "longlat")) 150 else 15
    nr <- m * (1 + log(abs(diff(dims[2,]))))
    nc <- m * (1 + log(abs(diff(dims[1,]))))
    
    kdens <- spatialEco::sp.kde(as(x, "Spatial"), bw = bw, 
                                newdata = as.vector(extent(x)),
                                nr = nr, nc = nc)
    
    # Extract point of maximum density
    colony <- xyFromCell(kdens, which.max(kdens))
    
    # create sf object
    colony <- st_as_sf(data.frame(x = colony[1], y = colony[2]),
                       coords = c("x", "y"), crs = sp_proj)
    
    if(isTRUE(plotkde)){
        plot(kdens)
        plot(st_geometry(colony), add = T)
    }

    
    return(colony)
} 