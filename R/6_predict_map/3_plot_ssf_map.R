library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())


# Define area to predict --------------------------------------------------

# Area to predict
pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>% 
      st_as_sf() %>%
      filter(NAME_1 %in% c("Eastern Cape", "Western Cape"))


# Find map codes ----------------------------------------------------------

source("R/functions/findMapCodes.R")

map_codes <- findMapCodes(pred_area)


# Define age --------------------------------------------------------------

age <- "juv"


# CROP TO PREDICTION AREA -------------------------------------------------

# Define raster directory
rasterdir <- "output/pred_rasters/"

# Load the name of all raster files
rfiles <- list.files(paste0(rasterdir, "2_pred_map_scaled/"), pattern = ".tif$")

# Extract raster of the desired age
rfiles <- rfiles[str_detect(rfiles, age)]

# within the map codes
rfiles <- rfiles[map_int(map_codes, ~which(str_detect(rfiles, .x)))]

# Load each raster file and crop to prediction area
for(i in seq_along(rfiles)){
   
   r <- raster(paste0(rasterdir,"2_pred_map_scaled/", rfiles[i]))
   
   if(!is.null(intersect(extent(r), extent(pred_area)))){
      r <- crop(r, pred_area)
   } else {
      next
   }
   
   writeRaster(r, paste0(rasterdir,"3_provinces/wc_ec_", rfiles[i]), overwrite = T)
}


# Load rasters ------------------------------------------------------------

rfiles <- list.files(paste0(rasterdir,"3_provinces/"), pattern = ".tif$")
rfiles <- rfiles[str_detect(rfiles, "scl")]

# Extract raster of the desired age
rfiles <- rfiles[str_detect(rfiles, age)]

# Merge rasters
rr <- raster(paste0(rasterdir,"3_provinces/", rfiles[1])) 

for(i in 2:length(rfiles)){
      rr <- raster::merge(rr, raster(paste0(rasterdir,"3_provinces/", rfiles[i])))
}

# Mask with prediction area
rr <- raster::mask(rr, pred_area)

# # Categorize selection
# nbreaks <- 11
# breaks <- quantile(rr, seq(0, 1, 1/nbreaks), na.rm = TRUE)
# val_fct <- cut(values(rr), breaks = breaks, labels = 0:(nbreaks-1))
# rr_fct <- setValues(rr, as.numeric(as.character(val_fct)))
# pal <- colorNumeric(viridis::magma(nbreaks, direction = 1), values(rr_fct),
#                     na.color = "transparent")

qpal <- colorQuantile("RdYlBu", values(rr), n = 10, na.color = "transparent", reverse = T)

leaflet() %>% addTiles() %>%
      addRasterImage(rr, colors = qpal, opacity = 0.8) %>%
      addLegend(pal = qpal, values = values(rr),
                title = "UD")

# Save final raster
outputfile <- paste0(rasterdir, "3_provinces/wc_ec_", age, "_final.tif")
writeRaster(rr, outputfile, overwrite = T)
