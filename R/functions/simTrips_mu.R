simTrips <- function(.nsteps=nsteps, .trip_dur=trip_dur, .age=age, .col_sel=col_sel,
                     .hab=hab, .mov_ker=mov_ker, .ssf_coef=ssf_coef){
      
      # Define distance function
      calcDist <- function(s, x, y){
            sqrt((s[1] - x)^2 + (s[2] - y)^2)
      }
      
      # Create a data frame to store simulations
      sims <- data.frame(lon = numeric(length = .nsteps),
                         lat = numeric(length = .nsteps),
                         dist_col = numeric(length = .nsteps),
                         ttnoon = integer(length = .nsteps),
                         trip = integer(length = .nsteps),
                         dur = integer(length = .nsteps))
      
      # # Pre-order habitat variables to increase speed
      # .hab <- .hab %>% 
      #       dplyr::select("sl_:ttnoon","sl_:ttnoon_sq","dist_col:ttnoon", "dist_col:ttnoon_sq",
      #                     "dist_col:juv:ttnoon", "dist_col:juv:ttnoon_sq", everything())
      
      # Predict from habitat
      X <- vector("list", 15)
      
      for(t in 1:15){
         ttnoon <- -7 + t - 1
         X[[t]] <- .hab %>% 
            mutate(across(.cols = ends_with("ttnoon"), ~.x*ttnoon),
                   across(.cols = ends_with("ttnoon_sq"), ~.x*ttnoon)) %>% 
            dplyr::select(names(.ssf_coef)) %>% 
            as.matrix() %*% .ssf_coef
      }
      
      gc()
      
      # Set initial point at colony in the morning
      state <- c(.col_sel$lon, .col_sel$lat)
      ttnoon <- -7
      ttnoon_sq <- ttnoon^2
      dist_col <- 0
      dur <- 0
      trip <- 0
      atcol <- TRUE
      
      # Initial location (important to keep track of projection)
      state_proj <- c(0, 0)
      
      # Pre-calculate distance weights (we don´t have the same amount of cells at different distances)
      dweights <- .hab %>% 
         mutate(sls = calcDist(state, lon, lat),
                sls_p = round(sls, 3)) %>% 
         group_by(sls_p) %>% 
         mutate(n = n()) %>% 
         ungroup() %>% 
         arrange(sls_p) %>% 
         pull(n)
      
      # Subset to useful variables and coefficients
      .hab <- .hab %>% 
         dplyr::select(lon, lat, x, y, dist_col_sc, cell_id)
      
      sl_coefs <- .ssf_coef[str_detect(names(.ssf_coef), "sl_")]
      
      # Precalculate probabilities of turning back
      pback <- pnbinom(0:10, size = nbinom_dur["size"], mu = nbinom_dur["mu"])
      
      for(j in 1:.nsteps){
            
            # Populate simulations
            sims[j, ] <- c(state[1], state[2], dist_col, ttnoon, trip, dur)
            
            # Calculate movement kernel weights
            sls <- round(calcDist(state_proj, .hab$x, .hab$y),0)
            sls[sls == 0] <- min(sls[sls>0])
            
            stephab <- .hab %>% 
                  mutate(sls = sls) %>% 
                  arrange(sls) %>% 
                  mutate(wgamma = dgamma(sls, shape = .mov_ker["shape"], scale = .mov_ker["scale"])/dweights) %>% 
                  arrange(cell_id)
            
            # Apply model corrections for time of day (see names(sl_coefs))
            sls <- stephab$sls/.mov_ker["model_sc"]
            sls_correct <- cbind(sls, sls*ttnoon, sls*ttnoon_sq, sls, sls*(age=="juv")) %*% sl_coefs
            
            # Calculate step sampling weights multiplying habitat and movement kernels
            t <- which(c(-7:7) == ttnoon)
            stephab <- stephab %>% 
                  mutate(predh = exp(X[[t]] + sls_correct),
                         w = predh*wgamma)
            
            # Sample one location
            step <- slice_sample(stephab, n = 1, weight_by = w)
            
            # Update state and time
            state <- c(step$lon, step$lat)
            state_proj <- c(step$x, step$y) # Note that we need a state in projected coordinates
            dist_col <- step$dist_col_sc
            ttnoon <- ttnoon + 1
            
            if(dist_col > 5000){
                  dur <- dur + 1
                  
            } else {
                  dur <- 0
                  atcol <- TRUE
            }
            
            if(dur == 1){
                  trip <- trip + 1
            }
            
            if(ttnoon > 7){

                  ttnoon <- -7
                  
                  if(!atcol && rbinom(1, 1, pback[floor(dur/15)])){
                     state <- c(.col_sel$lon, .col_sel$lat)
                     state_proj <- c(0,0)
                     dur <- 0
                     dist_col <- 0
                  }
                  
                  atcol <- FALSE
            }
            
            ttnoon_sq <- ttnoon^2
            
            # hab %>%
            #    ggplot() +
            #    geom_raster(aes(lon, lat, fill = w))
            # ggsave(paste("figures/step_weights_", j-1, ".png" ))
            
      }
      
      return(sims)
}