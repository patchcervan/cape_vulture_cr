makeTmerProj <- function(x){
    if(!"sf" %in% class(x)) stop("x is not a spatial object")
    
    ct <- apply(sf::st_coordinates(x),2,median)
    
    prjmerc <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                      " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    return(prjmerc)
}