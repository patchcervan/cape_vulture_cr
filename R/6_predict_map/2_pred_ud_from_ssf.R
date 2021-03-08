# 22-02-2021

# In this script we estimate an utilization distribution within a defined area
# based on selection values estimated using an SSF model

rm(list = ls())

library(tidyverse)
library(sf)
library(raster)


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read_csv("data/working/colony_data_all_upt.csv")

# Define raster directory
rasterdir <- "output/pred_rasters/"


# Define prediction area --------------------------------------------------

pred_area <- readRDS("data/working/gadm36_ZAF_1_sp.rds") %>% 
   st_as_sf() %>%
   filter(NAME_1 == "Western Cape")


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


# Scale rasters by colony size --------------------------------------------

# Load function to sum up rasters
source("R/functions/sumSelRasters.R")

for(i in seq_along(map_codes)){
   
   print(paste("Scaling", map_codes[i]))
   
   # Sum rasters without scaling by size 
   sumSelRasters(col_to_pred = col_to_pred,
                 raster_dir = paste0(rasterdir, "1_pred_map_col/"),
                 raster_id = map_codes[i],
                 age = age,
                 scale_sel = FALSE,
                 output_dir = paste0(rasterdir, "2_pred_map_scaled/"))
   
   
   # Sum rasters and scale by colony size
   sumSelRasters(col_to_pred = col_to_pred,
                 raster_dir = paste0(rasterdir, "1_pred_map_col/"),
                 raster_id = map_codes[i],
                 age = age,
                 scale_sel = TRUE,
                 output_dir = paste0(rasterdir, "2_pred_map_scaled/"))
   
}
