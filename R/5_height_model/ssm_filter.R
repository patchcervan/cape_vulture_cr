# 29-10-2020

# In this script we simulate from an OU model with measurement error

library(tidyverse)
library(TMB)

rm(list = ls())

setwd("/media/pachorris/DATA/Documentos/mis_trabajos/Academic/cape_vulture_cr")


# Load data ---------------------------------------------------------------

vults <- read_rds("data/working/data_height_ready.rds")

# Remove records with no height or no dt
vults <- vults %>% 
   filter(!is.na(height),
          !is.na(dt))

# Load function to calculate AIC from TMB object
source("R/functions/TMBAIC.R")


# Prepare variables -------------------------------------------------------

# Change variable names to make them more readable
vults <- vults %>% 
   rename(elev = srtm0,
          rugg = vrm3,
          dist_slp = dist_slp_m)

# Add log of distances since the effect probably saturates
vults <- vults %>% 
   mutate(log_dist_col = log(dist_col),
          log_dist_col_any = log(dist_col_any),
          log_dist_sfs = log(dist_sfs),
          log_dist_slp = log(dist_slp))


# Specify model -----------------------------------------------------------

model <- formula(height ~ log_dist_col + dist_col_any + dist_sfs +
                    elev + slope + rugg +
                    ttnoon + ttnoon_sq)

# Variables needed for the process
vv <- c("bird_id", "dt", all.vars(model))

bird_ids <- unique(vults$bird_id)

aic_df <- tibble(bird_id = character(length = length(bird_ids)),
                 AIC = double(length = length(bird_ids)))

# Subset the necessary variables
vults <- vults %>% 
   dplyr::select(all_of(vv))

# Prepare data frame to store filtering results
vults_filter <- data.frame()

# Compile model
compile("R/functions/TMB/ssm_ou_err_covts.cpp")
dyn.load(dynlib("R/functions/TMB/ssm_ou_err_covts"))


for(i in seq_along(bird_ids)){
   
   gc()
   
   id_sel <- bird_ids[i]
   
   vults_mod <- vults %>% 
      filter(bird_id == id_sel)
   
   # Standardize response and covariates
   vults_mod <- vults_mod %>% 
      mutate(height = scale(height, center = F)) %>% 
      mutate(across(.cols = -one_of(c("bird_id", "height", "dt")), ~scale(.x, center = TRUE)),
             intcp = 1)
   
   
   # Fit TMB model with obs error and covts ----------------------------------
   
   # Prepare data for TMB
   data.bundle <- list("z" = vults_mod$height, 
                       "dtime" = vults_mod$dt,
                       "X" = dplyr::select(vults_mod, -c(bird_id, height, dt)) %>% as.matrix(),
                       "Y" = dplyr::select(vults_mod, intcp, slope, rugg) %>% as.matrix(),
                       "B" = 0.01) 
   
   param <- list("lbeta" = log(1), "lsigma" = log(1),# "df_raw" = 20, 
                 "ztrans" = c(0, data.bundle$z),
                 "alpha" = rep(0, ncol(data.bundle$X)), "gamma" = c(0, 0, 0))
   
   gc()
   
   Obj <- MakeADFun(data = data.bundle, parameters = param, random = c("ztrans"), DLL = "ssm_ou_err_covts", silent = T)
   
   # Neg Log Likelihood minimization
   Opt <- NULL
   
   try(
   Opt <- optim(par = Obj$par, fn = Obj$fn, gr = Obj$gr, method = Obj$method,
                control = list(maxit = 1e3, reltol = 1e-5, trace = 0), hessian = TRUE)
   )
   
   if(!is.null(Opt)){
      
      fit_summ <- sdreport(Obj, getReportCovariance = FALSE)
      aic_m <- TMBAIC(Opt)
      
      # Run latent states simulation
      sims <- map(1:500, ~Obj$simulate()["zlat"])
      sims <- do.call("cbind", lapply(sims, "[[", 1))
      
      # Remove first observation (this doesn't match any observation)
      sims <- sims[-1,]
      
      # Rescale to original height scale
      sims <- sims * attr(data.bundle$z, "scaled:scale")
      
      # Find whether the bird was at a risk height
       pred_df <- tibble(zlat_hat = apply(sims, 1, mean),
                        zlat_sd = apply(sims, 1, sd),
                        zlat_ub = apply(sims, 1, quantile, prob = 0.975),
                        zlat_lb = apply(sims, 1, quantile, prob = 0.025),
                        z = data.bundle$z * attr(data.bundle$z, "scaled:scale"),
                        t = cumsum(data.bundle$dtime),
                        #risk = if_else(zlat_lb > 200, 0, 1),
                        bird_id = id_sel)
       
       # pred_df %>%
       #    slice(1:100) %>%
       #    ggplot() +
       #    geom_pointrange(aes(x = t, y = zlat_hat, ymin = zlat_lb, ymax = zlat_ub, col = factor(risk))) +
       #    geom_point(aes(x = t, y = z), col = "green")
       # 
       # pred_df %>%
       #    group_by(risk) %>%
       #    summarize(prop = n()/nrow(.))
       
       vults_filter <- rbind(vults_filter, pred_df)
       
   } else {
      
      aic_m = NA_real_
      
      pred_df <- tibble(zlat_hat = rep(NA, nrow(vults_mod)),
                        zlat_sd = rep(NA, nrow(vults_mod)),
                        zlat_ub = rep(NA, nrow(vults_mod)),
                        zlat_lb = rep(NA, nrow(vults_mod)),
                        z = data.bundle$z * attr(data.bundle$z, "scaled:scale"),
                        t = cumsum(data.bundle$dt),
                        # risk = if_else(zlat_lb > 200, 0, 1),
                        bird_id = id_sel)
      
   }

   aic_df$bird_id[i] <- id_sel
   aic_df$AIC[i] <- aic_m
   
   if(i %in% c((1:6)*10)){
      saveRDS(aic_df, "output/aic_height.rds")
   }
   
}


saveRDS(cbind(vults, vults_filter), "output/data_height_ssm_filtered.rds")

saveRDS(aic_df, "output/aic_height.rds")

# aic_df <- readRDS("output/aic_height.rds")

# 
# # Explore fit -------------------------------------------------------------
# 
# summary(fit_summ)
# 
# fit_summ$par.fixed
# exp(fit_summ$par.fixed)
# 
# sc_hgt <- attr(data.bundle$z, "scaled:scale")
# 
# pred_df <- tibble(zlat_hat = fit_summ$par.random * sc_hgt,
#                   zlat_sd = sqrt(fit_summ$diag.cov.random) * sc_hgt,
#                   zlat_ub = zlat_hat + 1.96 * zlat_sd,
#                   zlat_lb = zlat_hat - 1.96 * zlat_sd,
#                   z = data.bundle$z * sc_hgt,
#                   t = cumsum(data.bundle$dt),
#                   risk = if_else(zlat_lb > 300, 0, 1))
# 
# pred_df %>% 
#    slice(1:100) %>% 
#    ggplot() +
#    geom_pointrange(aes(x = t, y = zlat_hat, ymin = zlat_lb, ymax = zlat_ub, col = factor(risk))) +
#    geom_point(aes(x = t, y = z), col = "red")
# 
