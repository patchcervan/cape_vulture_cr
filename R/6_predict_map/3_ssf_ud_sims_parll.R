library(tidyverse)
library(raster)
library(sp)
library(glmmTMB)
library(furrr)

rm(list = ls())

# Set future maxsize to 850MB
options(future.globals.maxSize = 850*1024^2)
future::plan("multisession", workers = 5)
ncores <- future::nbrOfWorkers()

# Define raster output directory
rasterdir <- "output/pred_raster_sims/"


# Load data and model results ---------------------------------------------

# Tracking data
vults <- readRDS("data/working/data_ssf_ready.rds")

# Colony and roost data
colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Model fit
ssf_fit <- readRDS("output/ssf_fit_dist_tnoon.rds")
ssf_fit_summary <- readRDS("hpc/output/ssf_fit_summary.rds")

# Load movement kernel
mov_ker <- readRDS(paste0(rasterdir, "gamma_kern.rds"))

# Load Rcpp distance calculation function to speed up computation
Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")

# Load projection function
source("R/functions/makeTmerProj.R")


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))
#%>% 
#  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)


# Make Spatial objects ----------------------------------------------------

# Make colonies a spatial object
colony_all <- SpatialPointsDataFrame(coords = colony_all[,c("lon", "lat")], data = colony_all,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))


# Define age --------------------------------------------------------------

age = "juv"


# Extract model coefficients ----------------------------------------------

ssf_coef <- fixef(ssf_fit)$cond
mov_coef <- ssf_coef[str_detect(names(ssf_coef), "sl_")]

# Sample from random effects
sd_eff <- ssf_fit_summary$varcor$cond
sd_eff <- sqrt(do.call("c", sd_eff))

names(sd_eff) <- map_chr(ssf_fit_summary$varcor$cond, ~attr(.x, "dimnames")[[1]])
sd_eff <- sd_eff[names(sd_eff)!="(Intercept)"]

ssf_coefs <- vector("list", ncores)
ssf_coefs[[1]] <- ssf_coef

set.seed(3487)

if(ncores > 1){
   for(j in 2:ncores){
      for(i in 1:length(ssf_coef)){
         idx <- which(names(sd_eff) == names(ssf_coef)[i])
         if(length(idx) != 0){
            ssf_coefs[[j]][i] <- ssf_coef[i] + rnorm(1, 0, sd = sd_eff[idx])
         } else {
            ssf_coefs[[j]][i] <- ssf_coef[i]
         }
      }
      
      attributes(ssf_coefs[[j]]) <- attributes(ssf_coef)
   }
}

apply(do.call("cbind", ssf_coefs), 1, sd)


# Extract distribution of trip durations ----------------------------------

trip_dur <- vults %>% 
   group_by(age_fct, trip) %>% 
   summarize(dur = max(days_away))

# trip_dur %>% 
#    filter(dur > 100) %>% 
#    print(n = Inf)
# 
trip_dur %>%
   filter(dur < 10) %>%
   ggplot() +
   geom_histogram(aes(x = dur, y = stat(..density..))) +
   facet_wrap("age_fct")

# Find a parametric distribution for trip duration (neg binomial)
nbinom_dur <- fitdistrplus::fitdist(trip_dur$dur[trip_dur$age_fct == age], distr = "nbinom")$estimate

# curve(dnbinom(x, size = nbinom_dur["size"], mu = nbinom_dur["mu"]), xlim = c(0, 10))
durdist <- trip_dur %>% 
   filter(age_fct == age) %>% 
   group_by(dur) %>% 
   summarize(n = n()) %>% 
   mutate(p = cumsum(n)/sum(n),
          pnbin = pnbinom(dur, size = nbinom_dur["size"], mu = nbinom_dur["mu"]))


# Extract maximum distance from colony ------------------------------------

max_dist <- vults %>% 
   group_by(age_fct, trip) %>% 
   summarize(dist = max(dist_col))

# max_dist %>% 
#    ggplot() +
#    geom_histogram(aes(x = dist)) +
#    facet_wrap("age_fct")

# Max dist should be at least 500km



# Extract scaling factors -------------------------------------------------

sds <- unlist(sapply(ssf_fit$frame, attr, "scaled:scale"))


# Predict for the colonies ----------------------------------------------------

rm(ssf_fit, vults)
gc()

source("R/functions/simTrips.R")

for(k in 1:nrow(col_to_pred)){
   # k=1
   # k=7
   
   # Define distance function
   calcDist <- function(s, x, y){
      sqrt((s[1] - x)^2 + (s[2] - y)^2)
   }
   
   # Subset colony
   col_sel <- col_to_pred[k,]
   
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
   ct <- apply(coordinates(col_sel),2,median)
   tmerproj <- paste0("+proj=tmerc +lat_0=", ct[2], " +lon_0=", ct[1],
                      " +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
   
   # col_sel <- spTransform(col_sel, tmerproj)
   hab <- spTransform(hab, tmerproj)
   
   hab_cc <- coordinates(hab)

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
      mutate(dist_col_any = minDist_cpp(hab_cc, coordinates(col_others)))
   
   
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
             dist_col_any = dist_col_any / sds["dist_col_any"],
             dist_col = if_else(dist_col < 0.01, 0.01, dist_col), # 0.07 is the lower 99.9% of the data. Smaller numbers make pref. go to Inf quickly
             across(contains("dist_col:"), ~dist_col))
   
   # We will need distance from colony in projected coordinates later
   hab <- hab %>% 
      mutate(dist_col_sc = dist_col * sds["dist_col"],
             log_dist_col = log(dist_col_sc),
             across(contains("log_dist_col:"), ~log_dist_col), # Log of distance wasn't scaled
             across(contains("dist_col_any:"), ~dist_col_any),
             across(contains("dist_sfs:"), ~dist_sfs),
             across(contains("juv"), ~.x*(age == "juv")))
   
   # Remove "juv" terms if age is not "juv"
   if(age != "juv"){
      hab <- hab %>% 
         mutate(across(contains("juv"), ~.x*0))
   }
   
   # Define number of steps for the simulation
   totalsteps <- 10000
   
   durs <- vector("list", length = ncores)
   nsteps <- rep(ceiling(totalsteps/ncores), ncores) # rounded up, so that we get AT LEAST the desired number of steps
   
   # Reduce space to reduce computation time
   hab <- hab %>% 
      filter(dist_col < 4)
   
   source("R/functions/simTrips.R")
   
   # system.time(
   #    sims <- simTrips(.nsteps = nsteps[1], .ssf_coef = ssf_coefs[[1]])
   #    )
   
   #45345
   # sims <- furrr::future_map(nsteps, ~simTrips(.nsteps = .x, .ssf_coef = ssf_coefs[[1]]), .options = furrr_options(seed = 45345))
   sims <- furrr::future_map2(nsteps, ssf_coefs, ~simTrips(.nsteps=.x, .trip_dur=trip_dur, .age=age, .col_sel=col_sel,
                                                           .hab=hab, .mov_ker=mov_ker, .ssf_coef=.y),
                              .options = furrr_options(seed = 35563))
      
   for(i in seq_along(sims)){
      sims[[i]]$trip <- paste(i, sims[[i]]$trip, sep = "_")
   }
   
   sims <- do.call("rbind", sims)
   
   hab %>% 
      ggplot() +
      geom_raster(aes(lon, lat, fill = dist_col)) +
      # geom_point(data = sims, aes(x = lon, y = lat, col = factor(simsid))) +
      geom_density2d(data = sims, aes(x = lon, y = lat), col = "white")
   
   saveRDS(sims, paste0(rasterdir, col_sel$id, "_", age, "_sims100k.rds"))
   
}