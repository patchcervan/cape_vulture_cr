library(tidyverse)
library(sp)
library(mgcv)
library(vultRmap)

rm(list = ls())

# Define raster input/output directory
rasterdir <- "hpc/output/pred_raster/"


# Read in data ------------------------------------------------------------

# Colony data
col_all <- read_csv("data/working/colony_all_w_da.csv")

# Supplementary feeding sites
sfs <- read_csv("data/working/restaurants.csv")

# Subset those colonies and roosts we have data for.
col_to_pred <- col_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Loop through colonies and fit GAM ---------------------------------------

# Object to store explained deviance values
expl_dev <- vector(length = nrow(col_to_pred))

# Define age
age <- "ad"

for(i in seq_len(nrow(col_to_pred))){
   
   # Select a colony to process
   col_sel <- col_to_pred[i,]
   
   # Load simulations
   sims <- readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel$id, "_", age, "_sims.rds"))
   
   # Prepare covariates
   hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon", "lat")]),
                               max_range = max(sims$dist_col)/1000 + 10,
                               col_all = col_all, sfs = sfs, scale = "ssf")
   
   # Count the number of visits to each cell
   counts <- sims %>% 
      mutate(lon = round(lon, 3),
             lat = round(lat, 3)) %>% 
      group_by(lon, lat) %>% 
      summarize(count = n())
   
   # Associate counts with covariates
   hab <- hab %>% 
      mutate(lon = round(lon, 3), 
             lat = round(lat, 3)) %>% 
      left_join(counts, by = c("lon", "lat"))
   
   # Make NA counts = 0 and reduce the size of the data frame
   hab <- hab %>% 
      mutate(count = if_else(is.na(count), 0L, count)) %>% 
      filter(dist_col < (max(.$dist_col[.$count > 0]))) %>% 
      dplyr::select(count, lon, lat, dist_col, log_dist_col, dist_sfs, dist_col_any, elev, slope, rugg,
                    closed, crops, urban, water, prot_area)
   
   # Fit GAM
   fit <- mgcv::bam(count ~ te(lon, lat) + s(dist_col) + s(log_dist_col) + dist_sfs + dist_col_any + elev + slope + rugg +
                       closed+ crops+ urban+ water+ prot_area, family = poisson(link = "log"), data = hab,
                    discrete = TRUE, nthreads = 5)
   
   expl_dev[[i]] <- 1 - fit$deviance/fit$null.deviance
   
   hab$gamfit <- fit$fitted.values
   
   # Save habitat grid with fitted values
   hab %>%
      dplyr::select(lon, lat, count, gamfit) %>%
      saveRDS(file = paste0(rasterdir, "col_gam/", "gam_", col_sel$id, "_", age, ".rds"))
   
}

# Save explained deviance
saveRDS(expl_dev, paste0("output/expl_dev_gam_", age, ".rds"))


# 
# # Unify GAM output into a single data frame --------------------------------
# 
# library(vultRmap)
# 
# rm(list = ls())
# 
# # Define raster input/output directory
# rasterdir <- "hpc/output/pred_raster/"
# 
# # Define age
# age <- "ad"
# 
# gamdf <- vultRmap::range_covts %>% 
#    dplyr::mutate(lon = round(lon, 3),
#                  lat = round(lat, 3)) %>% 
#    dplyr::select(lon, lat) %>% 
#    as.data.frame()
# 
# attr(gamdf, "mod_scale") <- NULL
# 
# ff <- list.files(paste0(rasterdir, "col_gam/"), pattern = paste0(age, ".rds"))
# 
# for(i in seq_along(ff)){
#    
#    df <- readRDS(paste0(rasterdir, "col_gam/", ff[i]))
#    
#    varname <- gsub(".rds$", "", ff[i])
#    varname <- gsub("^gam_", "", varname)
#    
#    gamdf <- dplyr::left_join(gamdf,
#                              df[,c("lon", "lat", "gamfit")],
#                              by = c("lon", "lat"))
#    
#    gamdf <- gamdf %>% 
#       dplyr::rename(!!varname := gamfit)
#    
# }
# 
# gamdf[is.na(gamdf)] <- 0
# 
# # Save data frame
# saveRDS(gamdf, paste0(rasterdir, "col_gam/gam_df_all_", age, ".rds"))
