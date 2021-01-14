fitMixBatch <- function(trk, fig_path = getwd(), file_path = getwd()){
      
      require(tidyverse)
      require(suncalc)
      require(moveHMM)
      require(mixtools)
      require(lubridate)

      # Idenfify bird
      id_sel <- unique(trk$bird_id)
      
      print(id_sel)
      
      # Check previous processing steps
      if(length(attr(trk, "colony")) == 0){
         stop("No colony information?")
      }
      
      # remove NA speeds and speeds greater than 150 km/h
      if(id_sel != "ma07"){ # We know ma07 has no speed information, we deal with it later
         trk <- trk %>% 
            filter(!is.na(spd_h))
         
         trk <- trk %>% 
            filter(spd_h < 150)
         
         if(nrow(trk) == 0) stop("No valid locations")
      }

      
      # remove night-time locations
      daylight <- trk %>% 
            dplyr::select(datetime, lat, lon) %>% 
            mutate(date = date(datetime)) %>% 
            suncalc::getSunlightTimes(data = ., keep = c("sunrise", "sunset"))
      
      trk <- trk %>% 
            mutate(day = if_else(datetime > daylight$sunrise & datetime < daylight$sunset, 1, 0))
      
      trk <- trk %>% 
            filter(day == 1)
      
      # Re-calculate time increments
      trk <- trk %>% 
            mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours")))
      
      print(paste0("median dt = ", median(trk$dt, na.rm = T)))
      

      # ADD SOME COVARIATES -----------------------------------------------------
      
      # Time to noon covariate
      trk <- trk %>% 
            mutate(hour = hour(datetime))
      
      trk <- trk %>% 
            mutate(ttnoon = hour - 10,
                   ttnoon_sq = ttnoon^2)
      
      # Determine whether the bird is at a colony or SFS (< 1 km)
      trk <- trk %>% 
            mutate(at_col = if_else(dist_col < 1000, 1, 0),
                   at_col_any = if_else(dist_col_any < 1000, 1, 0),
                   at_sfs = if_else(dist_sfs < 1000, 1, 0))
      
      # Select covariates
      trk_mod <- trk %>% 
            dplyr::select(spd_h, avg_spd, lon, lat, bird_id, dt, ttnoon, ttnoon_sq,
                          dist_col, dist_col_any, dist_sfs,
                          at_col, at_col_any, at_sfs) %>% 
            as.data.frame()
      
      # Because ma07 has no speed information we will not process it further
      if(id_sel == "ma07"){
         trk$state <- NA
         
         # Add attribute move
         attr(trk, "move") <- 1
         
         saveRDS(trk, paste0(file_path, "/", id_sel, ".rds"))
         stop("ma07 has no speed information")
      }
      
      if(median(trk$dt, na.rm = T) < 3 & !(id_sel %in% c("kr01"))){
            
            # FIT HMM -----------------------------------------------------------------
            
            # Identify bursts
            trk_mod$ID <- NA
            k <- 1
            
            for(j in 1:(nrow(trk_mod)-1)){
                  trk_mod$ID[j] <- as.character(k)
                  if(trk_mod$dt[j] > 3) k <- k + 1
            }
            
            trk_mod$ID[nrow(trk_mod)] <- k
            
            # Compute number of locations per burst and keep only bursts with more than one
            # Do for both the model data and original data
            trk_mod <- trk_mod %>% 
                  add_count(ID, name = "nloc")
            
            trk <- trk %>% 
                  filter(trk_mod$nloc > 1)
            
            print(paste0("keeping ", 100* nrow(trk) / nrow(trk_mod), "% of data"))
            
            trk_mod <- trk_mod %>% 
                  filter(nloc > 1)
            
            # Prepare moveHMM data
            if(!id_sel %in% c("mb04", "mt02")){
               data <- prepData(trk_mod, type = "LL", coordNames = c("lon", "lat"))
            } else {
               # Hack data because there are bursts with 2 locations.
               # these are fine for our purposes
               data <- trk_mod
               class(data) <- c("moveData", "data.frame")
            }
            
            # there is an NA in dt that's where warning comes from
            
            # set all angle data to zero
            data$angle <- 0
            
            # Change step length by speed
            data$step <- trk_mod$spd_h
            
            # Choose initial values
            stepPar0 = c(1, 20, 1, 10, 0.5, 0.01)
            beta0 <- NULL
            
            # if(id_sel %in% c("ez02", "kr01", "kr02", "wt17")){
            #    stepPar0 <- c(1, 50, 1, 10, 0.5, 0.01)
            #    beta0 <- matrix(0, nrow = 6, ncol = 2)
            # } else if(id_sel %in% c("wt08", "wt13")){
            #    beta0 <- cbind(c(1, -1, -1, -1, 0, -1),
            #                   c(-1, 1, 1, 1, 0, 1))
            # }
            
            # Fit HMM
            hmm_fit <- fitHMM(data, nbStates = 2, 
                              formula = ~ 1 + at_col + at_col_any + at_sfs + ttnoon + ttnoon_sq,
                              stepPar0 = stepPar0,
                              beta0 = beta0,
                              stepDist = "gamma",
                              angleDist = "none")
            
            # hmm_fit
            
            # plot(hmm_fit)
            
            # compute the pseudo-residuals
            pr <- pseudoRes(hmm_fit)
            # plot(pr$stepRes)
            
            if(!any(!is.finite(pr$stepRes))){
                  # time series, qq-plots, and ACF of the pseudo-residuals
                  png(filename = paste0(fig_path, "/", id_sel, "_psd_res.png"))
                  plotPR(hmm_fit)
                  dev.off()
            }
            
            states <- viterbi(hmm_fit)
            
            trk$state <- states
            
      } else {
            
            # FIT MIXTURE MODEL -------------------------------------------------------
            
         if(id_sel %in% c("wt10", "wt12", "wt14", "wt15")){
            # these birds seem to have only one state apart from the zero speed
            trk$state <- NA
            trk$state[trk$spd_h <= 6] <- 1
            trk$state[trk$spd_h > 6] <- 2
         } else {
            # Take records with speed greater than 0 and save proportion of zeros for later
            trk_mod <- trk_mod %>%
               filter(spd_h > 0)
            
            prop0 <- sum(trk$spd_h == 0) / nrow(trk)
            
            ini.lambda <- c(sum(trk_mod$spd_h < 6),
                            sum(trk_mod$spd_h > 6)) / nrow(trk_mod)
            ini.mu <- c(1.5, 30)
            ini.scale <- c(2, 5)
            ini.shape <- ini.mu/ini.scale
            
            mixmod <- gammamixEM(x = trk_mod$spd_h, lambda = ini.lambda, alpha = ini.shape, beta = ini.scale,
                                 k = 2, mom.start = F, epsilon = 1e-6, maxit = 1000)
            
            plot(mixmod$all.loglik)
            mixmod$gamma.pars
            mixmod$lambda
            
            mu_fit <- mixmod$gamma.pars[1,] * mixmod$gamma.pars[2,]
            
            hist(trk_mod$spd_h, freq = F)
            curve(dgamma(x, shape = mixmod$gamma.pars[1,1], scale = mixmod$gamma.pars[2,1])*mixmod$lambda[1], col = "red", add = T)
            curve(dgamma(x, shape = mixmod$gamma.pars[1,2], scale = mixmod$gamma.pars[2,2])*mixmod$lambda[2], add = T)
            
            # Annotate with states
            states_df <- as.data.frame(mixmod$posterior) %>% 
               mutate(state = if_else(V1 > V2, 1, 2))
            
            trk$state <- NA
            trk$state[trk$spd_h == 0] <- 1
            trk$state[trk$spd_h > 0] <- states_df$state
            
            # Plot fit
            plot_mix_comps <- function(x, alpha, beta, lam) {
                  lam * dgamma(x, alpha, 1/beta)
            }
            
            trk_mod %>%
                  ggplot() +
                  geom_histogram(aes(spd_h, ..density..), binwidth = 1, colour = "black", 
                                 fill = "white") +
                  stat_function(geom = "line", fun = plot_mix_comps,
                                args = list(mixmod$gamma.pars[1,1], mixmod$gamma.pars[2,1], lam = mixmod$lambda[1]),
                                colour = "red", lwd = 1.5) +
                  stat_function(geom = "line", fun = plot_mix_comps,
                                args = list(mixmod$gamma.pars[1,2], mixmod$gamma.pars[2,2], lam = mixmod$lambda[2]),
                                colour = "blue", lwd = 1.5) +
                  ylab("Density")
            
            ggsave(filename = paste0(fig_path, "/", "diag_", id_sel,".png"))
         }
      }
      
      trk_stt_plot <- ggplot(trk) +
            geom_point(aes(x = lon, y = lat, col = factor(state)), alpha = 0.5) +
            ggtitle(paste0(id_sel, "_states"))
      
      stt_hist <- trk %>% 
            ggplot() +
            geom_histogram(aes(x = spd_h), col = "black", fill = "lightgreen") +
            geom_point(aes(x = spd_h, y = -5), shape = 124) +
            facet_wrap("state", nrow = 2)
      
      # locations in state 1 that present speed > 0
      # hist(trk$spd_h[trk$state == 1 & trk$spd_h > 0], freq = T)
      
      # locations in state 2 that present speed < 10
      # hist(trk$spd_h[trk$state == 2 & trk$spd_h < 10])
      
      # trk_sel <- trk %>% 
      #    filter(state == 2)
      
      # Plot
      trk_plots <- gridExtra::arrangeGrob(
            grobs = list(trk_stt_plot, stt_hist),
            ncol = 2, nrow = 1)
      
      ggsave(trk_plots, filename = paste0(fig_path, "/", id_sel, "_states.png"))
      
      # Add attribute move
      attr(trk, "move") <- 1
      
      saveRDS(trk, paste0(file_path, "/", id_sel, ".rds"))
}