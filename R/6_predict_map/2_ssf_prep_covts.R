library(tidyverse)
library(raster)
library(sf)
library(glmmTMB)

rm(list = ls())

sf_use_s2(FALSE) # s2 throws an error when preparing protected areas

# Define raster output directory
rasterdir <- "output/pred_raster_sims/"


# Load data and model results ---------------------------------------------

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv", col_types = cols())

# Model fit
ssf_fit <- readRDS("output/ssf_fit_10pp.rds")


# Define prediction variables --------------------------------------------

# Variables necessary for prediction
vv <- all.vars(ssf_fit$call$formula)

# Of these which ones we can extract from rasters (we need to use the name that
# appears in the stored files - elev = srtm0, rugg = vrm3, land_cov = LC100)
rcovts <- c("srtm0", "slope", "vrm3", "LC100_global_v3.0.1_2019")

# extracted from shapefiles
scovts <- c("prot_area")

covts <- list(rcovts = rcovts, scovts = scovts)

# distances to focal sites will be calculated later for each colony separately


# Extract model elements ------------------------------------------------------

ssf_frame <- ssf_fit$frame

rm(ssf_fit)

# Extract scaling factors
sds <- lapply(ssf_frame, attr, "scaled:scale")
sds <- sds[!sapply(sds, is.null)]


# Find map codes ----------------------------------------------------------

# List of raster files will give us the codes that cover the entire range (select elevation for example)
rfiles <- list.files("data/working/covts_rasters/", pattern = "srtm_.*.tif$")

map_codes <- str_sub(rfiles, 6, -5)


# Predict for the colonies ----------------------------------------------------

source("R/functions/createTempCovtRaster.R")

for(k in seq_along(map_codes)){
   # k=11
   
   mapcode_sel <- map_codes[k]
   
   # Create basic covts. stack -----------------------------------------------
   
   # Remove those maps that are already prepared and prepare the rest
   try(
      createTempCovtRaster(map_code = mapcode_sel, covts=covts$rcovts, output_res=0.01,
                           covtsdir = "data/working/covts_rasters/topo_res01/res01_",
                           outputdir = paste0(rasterdir, "temp_covts/"),
                           overwrite = F)
   )
   
   # Load the multiple stacks corresponding to map_codes
   rr <- stack(paste0(rasterdir, "temp_covts/covts_", mapcode_sel, ".gri"))
   
   # Detect ocean pixels
   v <- getValues(rr$land_cover)
   v[v == 200] <- NA
   rr$land_cover <- setValues(rr$land_cover, v)
   
   # Tranform to data frame
   rcovts <- as.data.frame(rr, xy = T)
   
   # Remove NA values and fix names
   rcovts <- rcovts %>% 
      rename_with(.fn = ~"elev", .cols = contains("srtm")) %>% 
      rename_with(.fn = ~"slope", .cols = contains("slope")) %>% 
      rename_with(.fn = ~"rugg", .cols = contains("vrm3")) %>% 
      rename(lon = x,
             lat = y) %>% 
      filter(!is.na(land_cover))
   
   # ggplot(rcovts) +
   #    geom_raster(aes(x = lon, y = lat, fill = land_cover))
   
   # Fix habitat codes
   rcovts <- rcovts %>% 
      left_join(dplyr::select(hab_meta, "Map code", class_code),
                by = c("land_cover" = 'Map code'))
   
   # print(ggplot(rcovts) +
   #    geom_raster(aes(x = lon, y = lat, fill = class_code)))
   
   rcovts <- rcovts %>% 
      mutate(hab_fct = factor(class_code)) %>% 
      dplyr::select(-c(land_cover, class_code)) %>% 
      mutate(i = 1) %>%
      spread(hab_fct, i, fill = 0)
   
   # # Scale data frame
   # rcovts <- rcovts %>% 
   #    mutate(elev = elev / sds$elev,
   #           slope = slope / sds$slope,
   #           rugg = rugg / sds$rugg)
   
   print(any(is.na(rcovts)))
   
   saveRDS(rcovts, paste0(rasterdir, "temp_covts/df_", mapcode_sel, ".rds"))
   rm(rr)
}


# Merge all maps ----------------------------------------------------------

rm(list = ls())

ff <- list.files("output/pred_raster_sims/temp_covts", pattern = ".rds")

rr <- map_dfr(ff, ~readRDS(paste0("output/pred_raster_sims/temp_covts/", .x)))

# map_dfr populates with NA missing columns. In this case missing habitats.
# There shouldn't be other NAs so:
rr[is.na(rr)] <- 0
   
saveRDS(rr, "output/pred_raster_sims/df_hab_general.rds")
