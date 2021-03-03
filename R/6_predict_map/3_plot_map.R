library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())

# Area to predict
pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>% 
      st_as_sf() %>%
      filter(NAME_1 == "Eastern Cape")

# Load the name of all raster files
rasterdir <- "output/pred_rasters/"
rfiles <- list.files(paste0(rasterdir,"4_provinces/eastern_cape/"), pattern = ".tif$")

# Extract raster of the desired age
age <- "ad"
rfiles <- rfiles[str_detect(rfiles, age)]

# Merge rasters
rr <- raster(paste0(rasterdir,"4_provinces/eastern_cape/", rfiles[1])) 

for(i in 2:length(rfiles)){
      rr <- raster::merge(rr, raster(paste0(rasterdir,"4_provinces/eastern_cape/", rfiles[i])))
}

# Mask with prediction area
rr <- raster::mask(rr, pred_area)

# Categorize selection
nbreaks <- 10
breaks <- quantile(rr, seq(0, 1, 1/nbreaks), na.rm = TRUE)

val_fct <- cut(values(rr), breaks = breaks, labels = 1:nbreaks)
rr_fct <- setValues(rr, val_fct)

pal <- colorNumeric(viridis::magma(nbreaks, direction = 1), values(rr_fct),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
      addRasterImage(rr_fct, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(rr_fct),
                title = "UD")

# Save final raster
outputfile <- paste0(rasterdir, "4_provinces/eastern_cape/east_cape_", age, "_final.tif")
writeRaster(rr_fct, outputfile, overwrite = T)
