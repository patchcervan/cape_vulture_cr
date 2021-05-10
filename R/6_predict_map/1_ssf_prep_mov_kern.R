rm(list = ls())

library(tidyverse)
library(glmmTMB)

# Define raster output directory
rasterdir <- "output/pred_raster_sims/"

# Load data
vults <- readRDS("data/working/data_ssf_ready.rds")

# what vultures have 1 hour resolution
hd_ids <- vults %>% 
      filter(res_fct == "res_1") %>% 
      pull(bird_id) %>% 
      unique()

# Load model
fit <- readRDS("output/ssf_fit_dist_tnoon.rds")

# Extract scaling factors for step length
sdsl <- attr(fit$frame$sl_, "scaled:scale")

# Extract coefficient estimate for the step length
sl_effects <- fixef(fit)$cond[str_detect(names(fixef(fit)$cond), "sl_")]

# Remove to free memory
rm(vults, fit)

gamma_df <- data.frame()
vonmis_df <- data.frame()

# Extract movement parameters
for(i in seq_along(hd_ids)){
      trk <- readRDS(paste0("data/working/bird_tracks/fit_ready/ssf/", hd_ids[i], ".rds"))
      atts <- attributes(trk)
      gamm_pars <- data.frame(bird_id = unique(trk$bird_id),
                              shape = atts$sl_$params$shape,
                              scale = atts$sl_$params$scale)
      vonm_pars <- data.frame(bird_id = unique(trk$bird_id),
                              kappa = atts$ta_$params$kappa,
                              mu = atts$ta_$params$mu)
      
      gamma_df <- rbind(gamma_df, gamm_pars)
      vonmis_df <- rbind(vonmis_df, vonm_pars)
}

hist(vonmis_df$kappa)
hist(vonmis_df$mu)

hist(gamma_df$shape)
hist(gamma_df$scale)

mu_shape <- mean(gamma_df$shape)
mu_scale <- mean(gamma_df$scale)

gamma_kern <- c(shape = mu_shape, scale = mu_scale, model_sc = sdsl)

curve(dgamma(x, shape = gamma_kern["shape"], scale = gamma_kern["scale"]), xlim = c(0, 1e5))

saveRDS(gamma_kern, paste0(rasterdir, "gamma_kern.rds"))


# Correct gamma with model coefficients -----------------------------------

# Correct scale
ttnoon <- 0; ttnoon_sq <- ttnoon^2
fct <- (sl_effects["sl_"] + sl_effects["sl_:ttnoon"]*ttnoon + sl_effects["sl_:ttnoon_sq"]*ttnoon^2 + sl_effects["sl_:res"]*1)*sdsl
scale_new <- 1/(1/mu_scale - fct)

curve(dgamma(x, shape = gamma_kern["shape"], scale = 4), xlim = c(0, 1e5))

