library(tidyverse)
library(sp)
library(mgcv)
# library(furrr)

rm(list = ls())

rasterdir <- "hpc/output/pred_raster/"


# Read in data ------------------------------------------------------------

# Model fit
ssf_fit <- readRDS("output/ssf_fit_dist_tnoon.rds")

# Extract scaling factors
sds <- unlist(sapply(ssf_fit$frame, attr, "scaled:scale"))

# Colony data
colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Subset those colonies and roosts we have data for.
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Define age --------------------------------------------------------------

age <- "juv"

# Remove fits that are already computed
# gamfiles <- list.files(paste0(rasterdir, "col_gam"), pattern = age)
# 
# gamfiles <- str_remove(gamfiles, age)
# gamfiles <- str_sub(gamfiles, 5, -6)
# 
# col_to_pred <- col_to_pred %>% 
#    filter(!id %in% gamfiles)


# Make Spatial objects ----------------------------------------------------

# Make colonies a spatial object
colony_all <- SpatialPointsDataFrame(coords = colony_all[,c("lon", "lat")], data = colony_all,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))


# Create function to fit GAM in parallel ----------------------------------

# Free memory
rm(ssf_fit)
gc()

# To debug
col_sel_id <- col_to_pred$id[2]
col_to_pred <- col_to_pred
.sds <- sds
rasterdir <- rasterdir

i = 1
for(i in 1:nrow(col_to_pred)){
   
   col_sel_id <- col_to_pred$id[i]
   
   require(Rcpp)
   
   # Load Rcpp distance calculation function to speed up computation
   Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")
   
   col_sel <- col_to_pred %>% 
      filter(id == col_sel_id)
   
   # Load sims
   # sims <- rbind(readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k1.rds")),
   #               readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k2.rds")))
   
   sims <- readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims100k.rds"))
   
   # Define distance function
   calcDist <- function(s, x, y){
      sqrt((s[1] - x)^2 + (s[2] - y)^2)
   }

   # Load habitat data
   hab <- readRDS(paste0(rasterdir, "temp_covts/df_hab_general.rds"))
   
   # Calculate approximate distances to colony
   hab <- hab %>% 
      mutate(distp = calcDist(c(col_sel$lon, col_sel$lat), lon, lat))
   
   # Keep only cells within 10 degrees
   hab <- hab %>% 
      filter(distp < 10) %>% 
      dplyr::select(-distp)
   
   hab <- SpatialPointsDataFrame(coords = hab[,c("x", "y")], data = hab,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
   
   # Take colony to the centre of the grid
   dists <- calcDist(c(col_sel$lon, col_sel$lat), hab$x, hab$y)
   
   col_sel$lon <- hab$x[which.min(dists)]
   col_sel$lat <- hab$y[which.min(dists)]
   
   # Make spatial object
   col_sel <- SpatialPointsDataFrame(coords = col_sel[,c("lon", "lat")], data = col_sel,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))
   
   # Project colony and habitat
   ct <- apply(sp::coordinates(col_sel),2,median)
   tmerproj <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                      " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
   
   # col_sel <- spTransform(col_sel, tmerproj)
   hab <- spTransform(hab, tmerproj)
   
   hab_cc <- sp::coordinates(hab)
   
   
   # Find distance to any colony or roost ------------------------------------
   
   # To create a matrix of coordinates from the colony/roost layer, we need to create a spatial object
   colony_all <- spTransform(colony_all, tmerproj)
   
   # Then, we need to remove any colony or roost that is less than 5km away from the central colony
   col_others <- colony_all %>% 
      as.data.frame() %>% 
      mutate(dist_from_central = calcDist(c(0,0), lon.1, lat.1)) %>% 
      # remove those that are closer than 10km
      filter(as.numeric(dist_from_central) > 5e3)
   
   col_others <- SpatialPointsDataFrame(coords = col_others[,c("lon.1", "lat.1")], data = col_others,
                                        proj4string = CRS(tmerproj))
   
   # Calculate distance between locations and any colony or roost
   # considering only those selected roosts and colonies that are further than 5km
   # from central colony
   hab <- hab %>% 
      as.data.frame() %>% 
      mutate(dist_col_any = minDist_cpp(hab_cc, sp::coordinates(col_others)))
   
   
   # Build data frame --------------------------------------------------------
   
   # For some reason sp changes ":" by ".". Change back
   hab <- hab %>% 
      rename_with(.fn = ~str_replace_all(.x, "\\.", ":"), .cols = everything())
   
   hab <- hab %>% 
      as.data.frame() %>% 
      mutate(x = hab_cc[,1],
             y = hab_cc[,2]) %>% 
      mutate(cell_id = row_number(),
             dist_col = calcDist(c(0,0), x, y)/sds["dist_col"],
             dist_col = if_else(dist_col < 0.003, 0.003, dist_col), # to prevent log from being zero
             log_dist_col = log(dist_col),
             dist_col_any = dist_col_any / sds["dist_col_any"],
             across(contains("dist_col:"), ~dist_col),
             across(contains("dist_col_any:"), ~dist_col_any),
             across(contains("dist_sfs:"), ~dist_sfs),
             across(contains("juv"), ~.x*(age == "juv")))
   
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
                       closed, crops, urban, water, prot_area)# %>% 
      # mutate(count = if_else(count == 0, NA_integer_, count))
   
   # Fit GAM
   system.time(
   fit <- mgcv::bam(count ~ s(lon, lat) + s(dist_col) + s(log_dist_col) + dist_sfs + dist_col_any + elev + slope + rugg +
                 closed+ crops+ urban+ water+ prot_area, family = poisson(link = "log"), data = hab,
                 discrete = FALSE, nthreads = 1)
   )

   # summary(fit)

   diag_deviance <- 1 - fit$deviance/fit$null.deviance

   hab$gamfit <- fit$fitted.values

   # pred <- predict(fit, newdata = hab)
   # hab$gamfit <- exp(pred)
   # 
   hab %>%
      filter(count > 0, count < 500) %>%
      ggplot() +
      geom_point(aes(x = gamfit, y = count)) +
      geom_abline(aes(slope = 1, intercept = 0)) +
      coord_equal()
   
   hab %>% 
      filter(gamfit > 200)
   hab %>% 
      filter(count>0, count<20) %>% 
      pull(count) %>% 
      hist()
   hab %>% 
      filter(gamfit>0, gamfit<20) %>% 
      pull(gamfit) %>% 
      hist()

   hab %>%
      mutate(count = if_else(count==0, NA_integer_, count)) %>% 
      filter(count < 50) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = gamfit)) +
      scale_fill_viridis_c()
   
   hab %>%
      filter(gamfit < 5) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = gamfit)) +
      scale_fill_viridis_c()
   
   hab %>%
      # mutate(count = if_else(count==0, NA_integer_, count)) %>% 
      filter(count < 50) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = count)) +
      scale_fill_viridis_c() +
      coord_cartesian(xlim = c(27,29), ylim = c(-30,-32))

   hab %>%
      mutate(resd = count - gamfit) %>% 
      filter(resd<50, resd >0) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = resd)) +
      scale_fill_viridis_c() +
      coord_cartesian(xlim = c(27,29), ylim = c(-30,-32))
   
   hab %>%
      mutate(resd = count - gamfit) %>% 
      filter(resd<0) %>% 
      ggplot() +
      geom_raster(aes(x = lon, y = lat, fill = resd)) +
      scale_fill_viridis_c()
   
   # hab %>%
   #    filter(count > 20 & gamfit < 10)

   hab %>%
      dplyr::select(lon, lat, count, gamfit) %>%
      saveRDS(file = paste0(rasterdir, "col_gam/", "gam_", col_sel$id, "_", age, ".rds"))

   return(diag_deviance)
   
}

future::plan("multisession", workers = 1)

expl_dev <- future_map_dbl(col_to_pred$id, ~f(.x, col_to_pred))

future::plan("sequential")

saveRDS(expl_dev, paste0("output/expl_dev_gam_", age, ".rds"))

for(i in 1:nrow(col_to_pred)){
   f(col_to_pred$id[i], col_to_pred)
}

