library(tidyverse)
library(raster)
library(sf)
library(glmmTMB)

rm(list = ls())


# Define raster output directory
rasterdir <- "output/pred_raster_sims/"


# Load data and model results ---------------------------------------------

# Tracking data
vults <- readRDS("data/working/data_ssf_ready.rds")

# Colony and roost data
colony_all <- read_csv("data/working/colony_data_all_upt.csv")

# Model fit
ssf_fit <- readRDS("output/ssf_fit_dist_tnoon.rds")

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
      filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50)) %>% 
   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)


# Define age --------------------------------------------------------------

age = "ad"


# Define movement kernel correction ---------------------------------------

ssf_coef <- fixef(ssf_fit)$cond
mov_coef <- ssf_coef[str_detect(names(ssf_coef), "sl_")]


# Extract distribution of trip durations ----------------------------------

trip_dur <- vults %>% 
   group_by(age_fct, trip) %>% 
   summarize(dur = max(days_away))

trip_dur %>% 
   filter(dur > 100) %>% 
   print(n = Inf)

trip_dur %>% 
   filter(dur < 50) %>%
   ggplot() +
   geom_histogram(aes(x = dur)) +
   facet_wrap("age_fct")



# Extract maximum distance from colony ------------------------------------

max_dist <- vults %>% 
   group_by(age_fct, trip) %>% 
   summarize(dist = max(dist_col))

max_dist %>% 
   ggplot() +
   geom_histogram(aes(x = dist)) +
   facet_wrap("age_fct")

# Max dist should be at least 500km


# Predict for the colonies ----------------------------------------------------

rm(ssf_fit, vults)
gc()

for(k in 1:nrow(col_to_pred)){
   # k=6
   
   # Subset colony
   col_sel <- col_to_pred[k,]
   
   # Load habitat data
   hab <- readRDS(paste0(rasterdir, "df_", col_sel$id, "_", age, ".rds"))
   hab <- st_as_sf(hab, coords = c("x", "y"), crs = 4326, remove = F)
   
   # Define distance function
   calcDist <- function(s, x, y){
      sqrt((s[1] - x)^2 + (s[2] - y)^2)
   }
   
   # Take colony to the centre of the grid
   dists <- calcDist(c(col_sel$lon, col_sel$lat), hab$x, hab$y)
   
   col_sel$lon <- hab$x[which.min(dists)]
   col_sel$lat <- hab$y[which.min(dists)]

   col_sel <- col_sel %>% 
      st_drop_geometry() %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
   
   # Project colony and habitat
   tmerproj <- makeTmerProj(col_sel)
   col_sel <- st_transform(col_sel, tmerproj)
   hab <- st_transform(hab, tmerproj)
   
   hab <- hab %>% 
      rename(lon = x,
             lat = y) %>% 
      mutate(x = as.numeric(st_coordinates(.)[,1]),
             y = as.numeric(st_coordinates(.)[,2])) %>% 
      st_drop_geometry()
   
   # Define number of steps for the simulation
   nsteps <- 10000
   
   # Create a data frame to store simulations
   sims <- data.frame(lon = numeric(length = nsteps),
                      lat = numeric(length = nsteps),
                      ttnoon = integer(length = nsteps),
                      trip = integer(length = nsteps))#,
                      # dur = integer(length = nsteps))
   
   # Set initial point at colony in the morning
   state <- c(0, 0)
   ttnoon <- -7
   ttnoon_sq <- ttnoon^2
   
   durs <- trip_dur %>% 
      filter(age_fct == age, dur < 7) %>% # 99% of durations
      pull(dur) %>% 
      sample(size = nsteps/15, replace = T)
   
   d <- 0
   trip <- 1
   dlim <- durs[trip]
   
   # Initial location (important to keep track of projection)
   state <- c(col_sel$lon, col_sel$lat)
   state_proj <- c(0, 0)
   
   for(j in 1:nsteps){
      
      # Populate simulations
      sims[j, ] <- c(state[1], state[2], ttnoon, trip, dlim)
      
      # Calculate movement kernel weights
      hab <- hab %>% 
         mutate(sls = calcDist(state_proj, x, y),
                # Can't allow step length of zero so substitute with smallest step length
                sls = if_else(sls == 0, min(sls[sls != 0]), sls),
                wgamma = dgamma(sls, shape = mov_ker["shape"], scale = mov_ker["scale"]))
      
      # Apply model corrections for time of day
      hab <- hab %>% 
         mutate(sl_ = sls/mov_ker["model_sc"],
                `sl_:res` = sl_*1,
                `sl_:ttnoon` = sl_*ttnoon,
                `sl_:ttnoon_sq` = sl_*ttnoon_sq,
                `dist_col:ttnoon` = dist_col*ttnoon,
                `dist_col:ttnoon_sq` = dist_col*ttnoon_sq,
                `dist_col:juv:ttnoon` = `dist_col:ttnoon`*(age=="juv"),
                `dist_col:juv:ttnoon_sq` = `dist_col:ttnoon_sq`*(age=="juv"))
      
      # Predict from habitat
      X <- hab %>% 
         dplyr::select(names(ssf_coef)) %>% 
         # mutate(across(starts_with("ns(sl_"), ~.x * 0)) %>% # in case we want to remove sl_ effects
         as.matrix()
      
      predh <- exp(X %*% matrix(ssf_coef))
      
      # Calculate step sampling weights multiplying habitat and movement kernels
      hab <- hab %>% 
         mutate(w = predh*wgamma)
      
      # Sample one location
      step <- slice_sample(hab, n = 1, weight_by = w)
      
      # Update state and time
      state <- c(step$lon, step$lat)
      state_proj <- c(step$x, step$y) # Note that we need a state in projected coordinates
      
      ttnoon <- ttnoon + 1
      if(ttnoon > 7){
         d <- d + 1
         ttnoon <- -7
         if(d > dlim){
            trip <- trip + 1
            state <- c(col_sel$lon, col_sel$lat)
            state_proj <- c(0,0)
            d <- 0
            dlim <- durs[trip]
         }
      }
      ttnoon_sq <- ttnoon^2
      
      # hab %>%
      #    ggplot() +
      #    geom_raster(aes(lon, lat, fill = w))
      # ggsave(paste("figures/step_weights_", j-1, ".png" ))
      
   }
   
   saveRDS(sims, paste0(rasterdir, col_sel$id, "_sims10k.rds"))
   sims <- readRDS(paste0(rasterdir, col_sel$id, "_sims100k.rds"))
   
   # sims$simsid <- rep(c(1,2), each = 500)
   
   hab %>%
      mutate(dist_col = calcDist(c(0,0), x, y)) %>% 
      ggplot() +
      geom_raster(aes(lon, lat, fill = dist_col)) +
      # geom_point(data = sims, aes(x = lon, y = lat, col = factor(simsid))) +
      geom_density2d(data = sims, aes(x = lon, y = lat), col = "white")
   
   ggsave("figures/sims_cw_10daytrip_colreturn.png")
   
   sims %>% 
      slice(-1) %>% 
      group_by(trip, dur) %>% 
      summarize(days = ceiling(n()/15)) %>% 
      mutate(dur_obs = days - 1) %>% 
      ggplot() +
      geom_histogram(aes(x = dur_obs))
   
   hist(durs)
   
   sims <- sims %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>% 
      st_transform(tmerproj) %>% 
      mutate(x = st_coordinates(.)[,1],
             y = st_coordinates(.)[,2],
             dist_col = calcDist(c(0,0), x, y)) %>% 
      st_drop_geometry()
   
   # Days from colony
   trips <- sims %>% 
      mutate(dayincrs = if_else(ttnoon == -7, TRUE, FALSE),
             day = cumsum(dayincrs)) %>% 
      group_by(day) %>% 
      summarize(mindist = min(dist_col)) %>% 
      mutate(at_col = if_else(mindist <= 5e3, 1, 0))
   
   trips$trip <- trips$at_col
   
   # Initiate trips
   t <- 1
   trips$trip[1] <- t
   
   for(i in 2:nrow(trips)){
      if(trips$trip[i] == 0){
         trips$trip[i] <- t
      } else {
         t <- t + 1 
         trips$trip[i] <- t
      }
   }
   
   trips %>% 
      group_by(trip) %>% 
      summarize(days_away = n()-1) %>% 
      filter(days_away > 6)
      
   
   trips %>% 
      group_by(trip) %>% 
      summarize(days_away = n()-1) %>% 
      pull(days_away) %>% 
      hist()
   
   hist(trips$days_away)
   hist(durs)
   
   # Count points per pixel
   r_col <- raster(paste0(rasterdir, "temp_covts/", col_sel$id, "_covts.gri"))
   counts <- rasterize(sims[,c("lon", "lat")], r_col, fun='count', background = 0)
   
   plot(counts)
   
   # smooth counts
   counts <- focal(counts, w = matrix(1/9, nc = 3, nr = 3))
   
   writeRaster(counts, paste0(rasterdir, "col_sims/", col_sel$id, ".tif"), overwrite = T)
   
   # vults %>% 
   #    filter(age_fct == "ad", dist_col < 300000, ttnoon > 6, ttnoon < 8) %>% 
   #    ggplot() + 
   #    geom_histogram(aes(x = dist_col)) + 
   #    facet_wrap("bird_id", scales = "free")