library(tidyverse)
library(sf)
library(raster)
library(leaflet)

rm(list = ls())


# Define raster directory
rasterdir <- "output/pred_raster_sims/"


# Define age --------------------------------------------------------------

age <- "juv"

# Do we want to scale by colony size?
sc <- FALSE


# Define area to predict --------------------------------------------------

# Area to predict
SA <- readRDS("data/working/gadm36_ZAF_1_sp.rds")
LST <- readRDS("data/working/gadm36_LSO_1_sp.rds")
SWZ <- readRDS("data/working/gadm36_SWZ_1_sp.rds")
BWA <- readRDS("data/working/gadm36_BWA_1_sp.rds")
NAM <- readRDS("data/working/gadm36_NAM_1_sp.rds")

# For Lesotho and Swaziland we will predict for the whole countries rather than
# provinces
LST$NAME_1 <- LST$NAME_0
SWZ$NAME_1 <- SWZ$NAME_0

# South Africa gets plotted with Lesotho and Swaziland
SA_total <- rbind(SA,LST,SWZ) %>% 
   st_as_sf()
# pred_area <- rbind(SA,LST,SWZ)

# In Zimbabwe and Namibia we will only predict in a couple of provinces
# and will be combined

BWA <- BWA %>%
   st_as_sf()

BWA %>% 
   ggplot() +
   geom_sf(aes(fill = NAME_1))

BWA <- BWA %>% 
   filter(!NAME_1 %in% c("North-West", "Chobe", "Kgalagadi", "Ghanzi"))

plot(st_geometry(BWA))

NAM <- NAM %>%
   st_as_sf()

NAM %>% 
   ggplot() +
   geom_sf(aes(fill = NAME_1))

NAM <- NAM %>% 
   filter(NAME_1 %in% c("Otjozondjupa"))

plot(st_geometry(NAM))

NAM$NAME_1 <- NAM$NAME_0
BWA$NAME_1 <- BWA$NAME_0

pred_area <- rbind(SA_total, NAM, BWA)

# Find provinces
provs <- unique(pred_area$NAME_1)

for(i in seq_along(provs)){
   
   prov <- pred_area %>%
      filter(NAME_1 == provs[i])
   
   
   # Name for saving
   filecode <- str_replace(tolower(unique(prov$NAME_1)),  " ", "_")
   
   
   # Find map codes ----------------------------------------------------------
   
   source("R/functions/findMapCodes.R")
   
   map_codes <- findMapCodes(prov)
   
   
   # CROP TO PREDICTION AREA -------------------------------------------------
   
   # Load the name of all raster files
   rfiles <- list.files(paste0(rasterdir, "2_pred_map_ud/"), pattern = ".tif$")
   
   # Extract smooth raster of the desired age
   rfiles <- rfiles[str_detect(rfiles, age)]
   rfiles <- rfiles[str_detect(rfiles, "gamfit")]
   
   # Scaled by counts?
   if(sc == TRUE){
      rfiles <- rfiles[!str_detect(rfiles, "nosc")]
   } else {
      rfiles <- rfiles[str_detect(rfiles, "nosc")]
   }
   
   
   # within the map codes
   f <- function(x){
      y <- str_detect(rfiles, x)
      return( if(any(y)){ which(y) } else { NA_integer_ } )
   }
   
   rfiles <- rfiles[map_int(map_codes, ~f(.x))]
   rfiles <- rfiles[!is.na(rfiles)]
   
   # Load each raster file and crop to prediction area
   rr <- vector("list", length = length(rfiles))
   
   for(j in seq_along(rfiles)){
      
      r <- raster(paste0(rasterdir,"2_pred_map_ud/", rfiles[j]))
      
      if(!is.null(intersect(extent(r), extent(prov)))){
         rr[[j]] <- crop(r, prov)
      } else {
         next
      }
   }
   
   # Merge rasters
   if(length(rr) > 1){
      rr <- do.call("merge", rr)
   } else {
      rr <- rr[[1]]
   }
   
   
   # Mask with prediction area
   rr <- raster::mask(rr, prov)
   
   
   # RUN ONLY TO SEE IF CROPING IS CORRECT -----------------------------------
   
   # source("R/functions/calcUDquantile.R")
   # 
   # nlevels <- 4
   # udlevels <- seq(0, 1, length.out = nlevels + 1)
   # 
   # qpal <- colorBin("RdYlBu", raster::values(rr), bins = calcUDquantile(raster::values(rr), udlevels),
   #                  na.color = "transparent", reverse = T)
   # 
   # labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")
   # 
   # leaflet() %>%
   #    addTiles() %>%
   #    addRasterImage(rr, colors = qpal, opacity = 0.8) %>%
   #    addLegend(pal = qpal, title = "UD", values = udlevels,
   #              labFormat = function(type, cuts, p) {
   #                 paste0(labels)
   #              })
   
   
   # Save province map -------------------------------------------------------
   
   if(sc == TRUE){
      outputfile <- paste0(rasterdir, "3_provinces/", filecode, "_", age, "_gamfit.tif")
   } else {
      outputfile <- paste0(rasterdir, "3_provinces/", filecode, "_", age, "_nosc_gamfit.tif")  
   }

   
   writeRaster(rr, outputfile, overwrite = T)
   
}
