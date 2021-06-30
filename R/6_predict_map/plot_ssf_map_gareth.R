library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())


# Define raster directory
rasterdir <- "output/pred_raster_sims/"


# Define area to predict --------------------------------------------------

provfiles <- list.files(paste0(rasterdir, "3_provinces/"), pattern = "gamfit")

provfiles <- provfiles[str_detect(provfiles, "eastern_cape")]

hgt <- FALSE

if(hgt){
   provfiles <- provfiles[str_detect(provfiles, "_hgt_")]
} else {
   provfiles <- provfiles[!str_detect(provfiles, "_hgt_")]
}

# Scaled by counts?
sc <- TRUE

if(sc){
   provfiles <- provfiles[!str_detect(provfiles, "nosc")]
} else {
   provfiles <- provfiles[str_detect(provfiles, "nosc")]
}


# Load rasters ------------------------------------------------------------

# Merge rasters
rr <- map(provfiles, ~raster(paste0(rasterdir,"3_provinces/", .x))) 

rr <- Reduce("+", rr)

source("R/functions/calcUDquantile.R")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(rr), bins = calcUDquantile(raster::values(rr), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")

# Colony palette (fixed)
pal_col <- colorFactor(c("yellow", "red", "grey"), domain = c(0, 1, 2))

leaflet() %>%
   addTiles() %>%
   addRasterImage(rr, colors = qpal, opacity = 0.8) %>%
   addLegend(pal = colorBin("RdYlBu", raster::values(rr), 
                            bins = calcUDquantile(raster::values(rr), 
                                                  udlevels),
                            na.color = "transparent", reverse = T),
             values = raster::values(rr), title = "UD", layerId = "udlegend", position = "bottomright",
             labFormat = function(type, cuts, p) { 
                paste0(labels)
             })

# Save final raster
cr <- if(hgt){"cr"} else {"ud"}
outputfile <- paste0(rasterdir, "3_provinces/", "ec_", cr, "_gareth.tif")
writeRaster(rr, outputfile, overwrite = T)
