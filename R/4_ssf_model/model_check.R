# 20-02-2021

# SSF model checking

rm(list = ls())

library(tidyverse)
library(glmmTMB)
# library(performance)
library(raster)
library(sf)


# Load model --------------------------------------------------------------

# ssf_fit <- readRDS("hpc/output/ssf_fit_rm.rds")
# vults <- ssf_fit$frame

# Load data
vults <- readRDS("data/working/data_ssf_ready.rds")

# Load prediction raster
rasterdir <- "output/pred_rasters/"
ud <- raster(paste0(rasterdir, "3_sel_final/eastern_cape/east_cape_final.tif"))

# Load prediction area
pred_area <- raster::getData("GADM", country = "South Africa", level = 1) %>% 
   st_as_sf() %>% 
   filter(NAME_1 == "Eastern Cape")

# Make tracking data a spatial object
vults <- st_as_sf(vults, coords = c("lon2_", "lat2_"), crs = 4326, remove = F)

# Find adult tracking locations intersecting the eastern cape
vult_sel <- vults %>% 
      filter(juv == 0, subad == 0) %>% 
      st_crop(vults, pred_area)

# Count points per pixel
counts <- rasterize(as(st_geometry(filter(vult_sel, case_ == TRUE)), "Spatial"), ud, fun='count', background = 0)

# Mask with prediction area
counts <- raster::mask(counts, pred_area)

plot(ud)
plot(counts)

plot(ud, counts)

# Make a data frame with UD and counts
pred_df <- tibble(ud = values(ud),
                  count = values(counts)) %>% 
   filter(!is.na(.$ud))

head(pred_df)

nbreaks = 10
breaks <- quantile(pred_df$ud, seq(0, 1, 1/nbreaks), na.rm = TRUE)

pred_df %>% 
   mutate(new_group = cut(pred_df$ud, breaks = breaks, labels = 1:nbreaks)) %>% 
   group_by(new_group) %>% 
   summarize(count = sum(count)) %>% 
   filter(!is.na(new_group))

pred_df <- pred_df %>% 
      mutate(new_group = cut(pred_df$ud, breaks = breaks, labels = 1:nbreaks)) %>% 
      group_by(new_group) %>% 
      summarize(count = sum(count)) %>% 
   filter(!is.na(new_group))

barplot(height = pred_df$count, names = pred_df$new_group, )
cor(pred_df$count, as.numeric(pred_df$new_group), method = "spearman")


# Categorize selection
nbreaks <- 10
breaks <- seq(0, maxValue(counts), length.out = (nbreaks+1))

val_fct <- cut(values(counts), breaks = breaks, labels = 1:nbreaks)
counts_fct <- setValues(counts, val_fct)

plot(ud)
plot(counts_fct)
plot(counts)

plot(st_geometry(vult_sel), add = T)

plot(st_geometry(vult_sel))

r2(ssf_fit)
check_collinearity(ssf_fit)
binned_residuals(ssf_fit, "dist_col")
