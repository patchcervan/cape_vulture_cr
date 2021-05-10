library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())


# Define raster directory
rasterdir <- "output/pred_raster_sims/"


# Define area to predict --------------------------------------------------

provfiles <- list.files(paste0(rasterdir, "3_provinces/"), pattern = "gamfit")

provfiles <- provfiles[!str_detect(provfiles, "SA_")]


# Define age --------------------------------------------------------------

age <- "juv"

agefiles <- provfiles[str_detect(provfiles, age)]

# Scaled by counts?
sc <- FALSE

if(sc){
   agefiles <- agefiles[!str_detect(agefiles, "nosc")]
} else {
   agefiles <- agefiles[str_detect(agefiles, "nosc")]
}



# Load rasters ------------------------------------------------------------

# Merge rasters
rr <- raster(paste0(rasterdir,"3_provinces/", agefiles[1])) 

for(i in 2:length(agefiles)){
      rr <- raster::merge(rr, raster(paste0(rasterdir,"3_provinces/", agefiles[i])))
}


# Load colony data --------------------------------------------------------

col_join <- read.csv("data/working/colony_all_w_da.csv")

col_join <- col_join %>% 
   mutate(size = case_when(is.na(avg_ad) ~ 2,
                           avg_ad < 10 ~ 2,
                           avg_ad <= 20 ~ 5,
                           avg_ad > 20 & avg_ad <= 100 ~ 7,
                           avg_ad > 100 & avg_ad <= 1000 ~ 9,
                           avg_ad > 1000 ~ 11,
                           TRUE ~ 3),
          type_id = if_else(type == "roost", 0L, 1L))

# Function to modify legend circle size
addLegendCustom <- function(map, colors, labels, sizes, ...){
   colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px; border:1px solid black")
   labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                            sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                            labels, "</div>")
   
   return(addLegend(map, colors = colorAdditions, 
                    labels = labelAdditions, ...))
}


source("R/functions/calcUDquantile.R")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(rr), bins = calcUDquantile(raster::values(rr), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")

# Colony palette (fixed)
pal_col <- colorFactor(c("yellow", "red", "grey"), domain = c(0, 1, 2))

col_join %>%
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
             }) %>% 
   addCircleMarkers(label = col_join$name,
                    popup = paste("Name: ",col_join$name, "<br>",
                                  "Type: ", col_join$type, "<br>",
                                  "Avg_adults: ", round(col_join$avg_ad), "<br>",
                                  "Old names: ", col_join$names_old, "<br>"),
                    radius = col_join$size,
                    fillColor = pal_col(col_join$type_id), fillOpacity = 0.5,
                    color = "black", weight = 1, stroke = T, opacity = 1,
                    group = "colonies") %>% 
   addLegendCustom(colors = pal_col(c(1, 0, rep(2,5))), 
                   labels = c("breed", "roost", "No count", "<20", "20-100", "100-1000", ">1000"), sizes = c(6,6,seq(6, 20, length.out = 5)),
                   opacity = 1,
                   position = "bottomright",
                   title = "Adult individuals")

# Save final raster
outputfile <- paste0(rasterdir, "4_provinces/", filecode,"_", age, "_final.tif")
writeRaster(rr, outputfile, overwrite = T)
