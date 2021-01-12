# 21-12-2020

# In this script we explore vulture speed data to identify those locations
# when birds are not moving, and possibly roosting

rm(list = ls())

library(tidyverse)
library(sf)
library(moveHMM)
library(mixtools)
library(lubridate)


# Read in data ------------------------------------------------------------

# We want to use tracks annotated with distance to colonies and SFSs

# List files
trkfiles <- list.files("data/working/bird_tracks/keep/", pattern = "col.rds")

for(i in 5:length(trkfiles)){
   
   # Read in original files from these birds
   trk <- readRDS(paste0("data/working/bird_tracks/keep/", trkfiles[i]))
   
   # Idenfify bird
   id_sel <- unique(trk$bird_id)
   
   print(id_sel)
   
   # remove NA speeds
   trk <- trk %>% 
      filter(!is.na(spd_h))
   
   # And speeds greater than 150 km/h
   trk <- trk %>% 
      filter(spd_h < 150)
   
   if(nrow(trk) == 0) next
   
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
   
   print(median(trk$dt, na.rm = T))
   
   # trk %>% 
   #    dplyr::select(datetime, day) %>% 
   #    print(n = 100)
   
   
   # ADD SOME COVARIATES -----------------------------------------------------
   
   # Time to noon covariate
   trk <- trk %>% 
      mutate(hour = hour(datetime))
   
   plot(trk$hour, trk$spd_h)
   
   trk <- trk %>% 
      mutate(ttnoon = hour - 10,
             ttnoon_sq = ttnoon^2)
   
   # Determine whether the bird is at a colony or SFS
   trk <- trk %>% 
      mutate(at_col = if_else(dist_col < 1000, 1, 0),
             at_col_any = if_else(dist_col_any < 1000, 1, 0),
             at_sfs = if_else(dist_sfs < 1000, 1, 0))
   
   # Select covariates
   trk_mod <- trk %>% 
      # mutate(across(.cols = c(dist_col, dist_col_any, dist_sfs), scale)) %>% 
      dplyr::select(spd_h, lon, lat, ID = bird_id, dt, ttnoon, ttnoon_sq,
                    dist_col, dist_col_any, dist_sfs,
                    at_col, at_col_any, at_sfs) %>% 
      as.data.frame()
   
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
      
      print(nrow(trk) / nrow(trk_mod))
      
      trk_mod <- trk_mod %>% 
         filter(nloc > 1)
      
      data <- prepData(as.data.frame(trk_mod),
                       type = "LL", coordNames = c("lon", "lat"))
      
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
         png(filename = paste0("output/mixture_plots/", id_sel, "_psd_res.png"))
         plotPR(hmm_fit)
         dev.off()
      }
      
      states <- viterbi(hmm_fit)
      
      trk$state <- states
      
   } else {
      
      # FIT MIXTURE MODEL -------------------------------------------------------
      
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
      
      ggsave(filename = paste0("output/mixture_plots/diag_", id_sel,".png"))
      
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
   
   ggsave(trk_plots, filename = paste0("output/mixture_plots/", id_sel, "_states.png"))
   
   saveRDS(trk, paste0("data/working/bird_tracks/keep/", id_sel, "_mxt.rds"))
}


# Explore state proportions -----------------------------------------------

# List files annotated with states
trkfiles <- list.files("data/working/bird_tracks/keep/", pattern = "mxt.rds")

# Make a data frame to store the results
states_df <- data.frame()

for(i in 1:length(trkfiles)){
   
   # Read in original files from these birds
   trk <- readRDS(paste0("data/working/bird_tracks/keep/", trkfiles[i]))
   
   # Idenfify bird
   id_sel <- unique(trk$bird_id)
   
   # Find state proportions
   trk_states <- trk %>% 
      group_by(bird_id, state) %>% 
      summarize(n = n())
   
   # Calculate proportions
   trk_states <- trk_states %>% 
      mutate(total = sum(n),
             prop = n / total) %>% 
      ungroup()
   
   # Add to general data frame
   states_df <- rbind(states_df,
                      trk_states)
}

# Plot sample sizes
states_df %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = total))

# Plot proportions in moving state (2)
states_df %>% 
   filter(state == 2) %>% 
   ggplot() +
   geom_histogram(aes(x = prop))

# Find 90% lower quantile of total number of locations
states_df %>% 
   pull(total) %>% 
   unique() %>% 
   quantile(., 0.1)

# Find 90% lower quantile of proportions in state 2
states_df %>% 
   filter(state == 2) %>% 
   pull(prop) %>% 
   quantile(., 0.1)

# Find 90% lower quantile of birds with more than 200 locations in state 2
states_df %>% 
   filter(state == 2, total > 650) %>% 
   pull(prop) %>% 
   quantile(., 0.1)

# Plots proportion of state 2
states_df %>% 
   filter(state == 2) %>% 
   mutate(size = case_when(n <= 200 ~ 1,
                           n > 200 & n <= 1e3 ~ 2,
                           n > 1e3 & n <= 5e3 ~ 3,
                           n > 5e3 ~ 4)) %>% 
   ggplot() +
   geom_point(aes(x = prop, y = bird_id, size = size, col = factor(size)), alpha = 0.7) +
   scale_size_continuous(name = "size", range = c(2,9), labels = c("<200", "200-1e3", "1e3-5e3", ">5e3")) +
   theme(axis.text.y = element_text(size = 8))

# Based on the proportion of locations with moving state 2 of those birds with more
# than 650 locations in total (90%), I will remove those birds with less than 15%
states_df <- states_df %>% 
   filter(state == 2) %>% 
   mutate(keep = if_else(prop < 0.15, 0, 1))

# Plots proportion of state 2
stt_plot <- states_df %>% 
   mutate(size = case_when(n <= 300 ~ 1,
                           n > 300 & n <= 1e3 ~ 2,
                           n > 1e3 & n <= 5e3 ~ 3,
                           n > 5e3 ~ 4)) %>% 
   ggplot() +
   geom_point(aes(x = prop, y = bird_id, size = size, fill = size), alpha = 0.7, pch = 21) +
   geom_point(data = filter(states_df, keep == 0), aes(x = prop, y = bird_id), shape = 4, col = "red", size = 5) +
   scale_size_continuous(name = "n_locs", range = c(2, 9), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   scale_fill_viridis_c(name = "n_locs", option = "C", limits = c(1, 4), breaks = c(1, 2, 3, 4), labels = c("<300", "300-1e3", "1e3-5e3", ">5e3")) +
   guides(fill = guide_legend(), size = guide_legend()) +
   xlab("Proportion of locations in moving state") +
   theme(axis.text.y = element_text(size = 8))

ggsave(filename = "output/prop_state2.png", plot = stt_plot, dpi = 300)

# Save data frame with problem birds
states_df %>% 
   dplyr::select(bird_id, prop, keep) %>% 
   write_csv("data/working/birds_mix_exclude.csv")


# Explore zeros in state 2 ------------------------------------------------

# ez02 and wt03 are good examples
trk <- readRDS("data/working/bird_tracks/keep/ez02_mix.rds")

# Plot lon/lat sequence with state allocation
trk %>% 
   ggplot() +
   geom_point(aes(x = lon, y = lat, col = factor(state)))

trk %>% 
   dplyr::select(datetime, lon, lat, state, spd_h) %>% 
   gather(dim, coord, -c(datetime, state)) %>% 
   group_by(dim) %>% 
   slice(1:50) %>% 
   ggplot() +
   geom_line(aes(x = datetime, y = coord), linetype = 2) +
   geom_point(aes(x = datetime, y = coord, col = factor(state))) +
   facet_wrap("dim", ncol = 1, scales = "free")

# There might be a problem with the resolution
dplyr::select(trk, datetime, dt)

head(difftime(trk$datetime, lag(trk$datetime), units = "hours"))

# remove night-time locations
daylight <- trk %>% 
   dplyr::select(datetime, lat, lon) %>% 
   mutate(date = date(datetime)) %>% 
   slice(1:100) %>% 
   suncalc::getSunlightTimes(data = ., keep = c("sunrise", "sunset"))

trk %>% 
   dplyr::select(datetime, lat, lon) %>% 
   slice(1:100) %>% 
   mutate(day = if_else(datetime > daylight$sunrise & datetime < daylight$sunset, 1, 0))
