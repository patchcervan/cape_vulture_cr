library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())


# Define raster directory
rasterdir <- "../vultRmap/analysis/output/risk_maps/"

# Area to predict
SA <- readRDS("data/working/gadm36_ZAF_1_sp.rds")

ec <- SA %>% 
   st_as_sf() %>% 
   filter(NAME_1 == "Eastern Cape")

# Risk map 2D -------------------------------------------------------------

hgt <- FALSE
r <- sum(raster(paste0(rasterdir, "enc_risk_ad.tif")),
         raster(paste0(rasterdir, "enc_risk_juv.tif")))

r <- crop(r, ec)
r <- mask(r, ec)

# Risk map 3D -------------------------------------------------------------

hgt <- TRUE
r <- sum(raster(paste0(rasterdir, "enc_hgt_risk_ad.tif")),
         raster(paste0(rasterdir, "enc_hgt_risk_juv.tif")))

r <- crop(r, ec)
r <- mask(r, ec)


# Load rasters ------------------------------------------------------------

source("R/functions/calcUDquantile.R")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")

# Colony palette (fixed)
pal_col <- colorFactor(c("yellow", "red", "grey"), domain = c(0, 1, 2))

leaflet() %>%
   addTiles() %>%
   addRasterImage(r, colors = qpal, opacity = 0.8) %>%
   addLegend(pal = colorBin("RdYlBu", raster::values(r), 
                            bins = calcUDquantile(raster::values(r), 
                                                  udlevels),
                            na.color = "transparent", reverse = T),
             values = raster::values(r), title = "UD", layerId = "udlegend", position = "bottomright",
             labFormat = function(type, cuts, p) { 
                paste0(labels)
             })

# Save final raster
cr <- if(hgt){"cr"} else {"ud"}
outputfile <- paste0("output/pred_raster_sims/3_provinces/", "ec_", cr, "_gareth.tif")
writeRaster(r, outputfile, overwrite = T)
