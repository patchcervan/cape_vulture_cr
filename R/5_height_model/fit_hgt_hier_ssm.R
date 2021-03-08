# 29-10-2020

# In this script we simulate from an OU model with measurement error

library(tidyverse)
library(TMB)

rm(list = ls())

# setwd("/media/pachorris/DATA/Documentos/mis_trabajos/Academic/cape_vulture_cr")


# Load data ---------------------------------------------------------------

vults <- read_rds("data/working/data_height_ready.rds")


# Prepare variables -------------------------------------------------------

# Remove repeated observations
vults <- vults %>% 
   group_by(bird_id) %>% 
   distinct(datetime, .keep_all = T) %>% 
   ungroup()

# Remove records with no height or no dt
vults <- vults %>% 
   filter(!is.na(height))

# Recalculate dt
vults <- vults %>% 
   group_by(bird_id) %>% 
   mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours"))) %>% 
   ungroup()


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

# Standardize response and covariates
vults <- vults %>% 
   mutate(height = scale(height, center = F)) %>% 
   mutate(across(.cols = -one_of(c("bird_id", "height", "dt")), ~scale(.x, center = TRUE)),
          intcp = 1)

# Prepare data for TMB

# Remove last dt of each group
dts <- vults %>% 
   group_by(bird_id) %>%
   slice(-tail(row_number(), 1)) %>%
   pull(dt)

data.bundle <- list("z" = vults$height, 
                    "dtime" = dts,
                    "X" = dplyr::select(vults, -c(bird_id, height, dt)) %>% as.matrix(),
                    "Y" = dplyr::select(vults, intcp, slope, rugg) %>% as.matrix(),
                    "B" = 0.1,
                    "N" = nrow(vults),
                    "K" = ncol(dplyr::select(vults, -c(bird_id, height, dt))),
                    "L" = ncol(dplyr::select(vults, intcp, slope, rugg)),
                    "J" = n_distinct(vults$bird_id),
                    "n_j" = vults %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n),
                    "group" = rep(0:(n_distinct(vults$bird_id)-1), times = vults %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n))) 

# Define initial values
param <- list("mu_alpha" = rep(0, data.bundle$K), "mu_gamma" = rep(0, data.bundle$L),
              "lsig_alpha" = rep(log(0.5), data.bundle$K), "lsig_gamma" = rep(log(0.5), data.bundle$L),
              "alpha" = matrix(0, ncol = data.bundle$K, nrow = data.bundle$J),
              "gamma" = matrix(0, ncol = data.bundle$L, nrow = data.bundle$J),
              "lsigma" = log(1), "lbeta" = log(0.1), 
              "ztrans" = data.bundle$z)
   

# Fit TMB model with obs error and covts ----------------------------------

compile("R/functions/TMB/hier_ssm_ou_err_covts.cpp")
dyn.load(dynlib("R/functions/TMB/hier_ssm_ou_err_covts"))

Obj <- MakeADFun(data = data.bundle, parameters = param, random = c("ztrans", "alpha", "gamma"), DLL = "hier_ssm_ou_err_covts", silent = F)
   
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
