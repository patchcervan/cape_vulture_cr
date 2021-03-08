ET <- raster::getData("GADM", country="South Africa", level=1)

AD<-subset(ET["NAME_1"],NAME_1=="Eastern Cape")

AD <- st_as_sf(AD)

plot(AD)
