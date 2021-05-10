# 22-02-2021

# In this script we estimate an utilization distribution within a defined area
# based on selection values estimated using an SSF model

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Define raster directory
rasterdir <- "output/pred_raster_sims/"
# rasterdir <- "hpc/output/pred_raster/"


# Define prediction area --------------------------------------------------

pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>%
   st_as_sf() #%>%
   # filter(NAME_1 %in% c("Western Cape", "Eastern Cape"))

pred_area <- rbind(pred_area,
                   readRDS("data/working/gadm36_LSO_1_sp.rds") %>% st_as_sf())


# Define age --------------------------------------------------------------

age <- "juv"


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Find map codes ----------------------------------------------------------

source("R/functions/findMapCodes.R")

map_codes <- findMapCodes(pred_area)

# mapfiles <- list.files(paste0(rasterdir, "2_pred_map_ud/"), pattern = ".tif$")
# mapfiles <- mapfiles[str_detect(mapfiles, "gamfit")]
# mapfiles <- mapfiles[str_detect(mapfiles, age)]
# map_codes <- map_codes[!map_lgl(map_codes, ~any(str_detect(mapfiles, .x)))]


# Calculate UD from sims --------------------------------------------------

source("R/functions/calcUDFromSims.R")

# # Run these for single maps
# calcUD(.mapcode = map_codes, .rasterdir = rasterdir,
#        .outputdir = paste0(rasterdir, "2_pred_map_ud/"))
# 
# calcUD(.mapcode = map_codes[9], .col_to_pred = col_to_pred, .age = age, .rasterdir = rasterdir,
#        scale = T, .outputdir = paste0(rasterdir, "2_pred_map_ud/"))

# Estimate UD without scaling
map(map_codes, ~calcUD(.mapcode = .x, .col_to_pred = col_to_pred, .age = age, scale = F,
                       .rasterdir = rasterdir, .outputdir = paste0(rasterdir, "2_pred_map_ud/")))

# Estimate UD with scaling
map(map_codes, ~calcUD(.mapcode = .x, .col_to_pred = col_to_pred, .age = age, scale = T,
                       .rasterdir = rasterdir, .outputdir = paste0(rasterdir, "2_pred_map_ud/")))



# Explore -----------------------------------------------------------------

library(leaflet)

r <- raster(paste0(rasterdir, "2_pred_map_ud/gamfit_42_19_juv.tif"))

source("R/functions/calcUDquantile.R")

nlevels <- 10
udlevels <- seq(0, 1, length.out = nlevels + 1)

qpal <- colorBin("RdYlBu", raster::values(r), bins = calcUDquantile(raster::values(r), udlevels),
                 na.color = "transparent", reverse = T)

labels = paste0(udlevels*100, "-", lead(udlevels*100), " %")

leaflet() %>%
   addTiles() %>%
   addRasterImage(r, colors = qpal, opacity = 0.8) %>%
   addLegend(pal = qpal, title = "UD", values = udlevels,
             labFormat = function(type, cuts, p) { 
                paste0(labels)
             })

