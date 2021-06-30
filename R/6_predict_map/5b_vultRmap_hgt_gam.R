library(tidyverse)
library(mgcv)
library(vultRmap)

rm(list = ls())

# Define raster output directory
rasterdir <- "../vultRmap_data_aux/"

# Read in data ------------------------------------------------------------

# Colony data
col_all <- read_csv("../vultRmap_data_aux/colony_data.csv")

# Supplementary feeding sites
sfs <- read_csv("../vultRmap_data_aux/sup_feeding_data.csv")

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
   sims <- readRDS(paste0(rasterdir, "col_hgt_risk/", col_sel$id,"_", age, "_hgt_risk.rds"))

   # Prepare covariates
   hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon", "lat")]),
                               max_range = 1200,
                               col_all = col_all, sfs = sfs, scale = "hgt")

   # Count the number of visits to each cell
   counts <- sims %>%
      mutate(lon = round(lon, 3),
             lat = round(lat, 3)) %>%
      group_by(lon, lat) %>%
      summarize(count = sum(hgt_risk))

   # Associate counts with covariates
   hab <- hab %>%
      mutate(lon = round(lon, 3),
             lat = round(lat, 3)) %>%
      left_join(counts, by = c("lon", "lat"))

   # Make NA counts = 0 and reduce the size of the data frame
   hab <- hab %>%
      mutate(count = if_else(is.na(count), 0, count)) %>%
      filter(dist_col < (max(.$dist_col[.$count > 0]))) %>%
      dplyr::select(count, lon, lat, dist_col, log_dist_col, dist_sfs, dist_col_any, elev, slope, rugg,
                    closed, crops, urban, water, prot_area)

   # Fit GAM
   fit <- mgcv::bam(count ~ te(lon, lat) + s(dist_col) + s(log_dist_col) + dist_sfs + dist_col_any + elev + slope + rugg +
                       closed+ crops+ urban+ water+ prot_area, family = poisson(link = "log"), data = hab,
                    discrete = TRUE, nthreads = 10)

   expl_dev[[i]] <- 1 - fit$deviance/fit$null.deviance

   hab$gamfit <- fit$fitted.values

   # Save habitat grid with fitted values
   hab %>%
      dplyr::select(lon, lat, count, gamfit) %>%
      saveRDS(file = paste0(rasterdir, "col_hgt_gam/", "gam_hgt_", col_sel$id, "_", age, ".rds"))

}

# Save explained deviance
saveRDS(expl_dev, paste0("output/expl_dev_gam_", age, "_hgt.rds"))



# Unify GAM output into a single data frame --------------------------------

# library(vultRmap)
# 
# rm(list = ls())
# 
# # Define raster output directory
# rasterdir <- "/home/crvfra001/vults/output/vultRmap"
# 
# setwd("/home/crvfra001/vults")
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
# ff <- list.files(paste0(rasterdir, "/col_gam/"), pattern = paste0(age, ".rds"))
# 
# for(i in seq_along(ff)){
#    
#    df <- readRDS(paste0(rasterdir, "/col_gam/", ff[i]))
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
# saveRDS(gamdf, paste0(rasterdir, "/col_gam/gam_df_all_", age, ".rds"))
