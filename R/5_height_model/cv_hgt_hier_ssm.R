# 29-10-2020

# In this script we simulate from an OU model with measurement error

library(tidyverse)
library(TMB)

rm(list = ls())

# setwd("/media/pachorris/DATA/Documentos/mis_trabajos/Academic/cape_vulture_cr")


# Load data ---------------------------------------------------------------

vults <- read_rds("data/working/data_height_ready_noextrm.rds")


# Prepare variables -------------------------------------------------------

# Remove repeated observations
vults <- vults %>% 
   group_by(bird_id) %>% 
   distinct(datetime, .keep_all = T) %>% 
   ungroup()

# Remove records with no height
vults <- vults %>% 
   filter(!is.na(height))

# Recalculate dt
vults <- vults %>% 
   group_by(bird_id) %>% 
   mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours"))) %>% 
   ungroup()


# Add log of distances since the effect probably saturates
vults <- vults %>% 
   mutate(log_dist_col = log(dist_col),
          log_dist_col_any = log(dist_col_any),
          log_dist_sfs = log(dist_sfs),
          log_dist_slp = log(dist_slp))


# Prepare train and validation --------------------------------------------

# Add group index
vults <- vults %>% 
   group_by(age_fct, zone_fct) %>% 
   mutate(cv_group = cur_group_id()) %>% 
   ungroup()

vults %>% 
   group_by(cv_group, bird_id, zone_fct, age_fct) %>% 
   summarize(n = n()) %>% 
   group_by(cv_group, zone_fct, age_fct) %>% 
   summarize(n = n())

# What bird belongs to what group
bird_ids <- vults %>% 
   group_by(bird_id) %>% 
   summarize(cv_group = unique(cv_group)) %>% 
   ungroup()

# Add group size
bird_ids <- left_join(bird_ids,
                      vults %>% 
                         group_by(cv_group, bird_id, zone_fct, age_fct) %>% 
                         summarize(n = n()) %>% 
                         group_by(cv_group, zone_fct, age_fct) %>% 
                         summarize(n = n()) %>% 
                         ungroup(),
                      by = "cv_group")

# Birds can be in different groups if they grew older, therefore we must create
# a unique identifier for bird id and group
bird_ids <- bird_ids %>% 
   mutate(cv_id = paste(bird_id, cv_group, sep = "_"))

# Sample one third of birds 10 times
set.seed(934875)

f <- function(df){
   df %>% 
      group_by(cv_group) %>% 
      sample_frac(size = 0.3) %>% 
      ungroup() %>% 
      pull(cv_id)
}

cv_ids <- map(1:10, ~f(bird_ids))

# Set group names
for (i in seq_along(cv_ids)){
   attr(cv_ids[[i]], "id") <- paste0("cv", i)
}


# Define models to compare ------------------------------------------------

# Helper function to formulate models from elements
form <- function(data, mod_elem, include, intr = NULL){
   
   # Extract variables
   elem <- unlist(mod_elem[include])
   
   # Define interactions
   fixed <- elem # In case there are no interactions
   
   # Define interactions
   if(!is.null(intr)){
      for(i in seq_along(intr)){
         v1 <- mod_elem[[as.character(intr[[i]][[2]])]]
         v2 <- as.character(intr[[i]][[3]])[1]
         if(!(v2 %in% names(data))) stop("variable not in data frame")
         ref <- as.character(intr[[i]][[3]])[2]
         
         if(!is.na(ref)){
            lv <- unique(vults[[v2]])
            lv <- lv[lv != ref]
            
            out <- paste(rep(v1, each = length(lv)), lv , sep = ":")
         } else{
            out <- paste(v1, v2, sep = ":")
         }
         
         fixed <- c(fixed, out)
      }
   }
   
   # Define random effects
   rdm <- c("(1|bird_id)", paste("(0 +", elem,"|bird_id)"))
   
   # Write formula
   reformulate(c(1, fixed, rdm), "height")
}

# Define model elements
mod_elem <- list(
   dist = c("dist_col", "dist_sfs", "dist_col_any"),
   dist2 = c("log_dist_col", "dist_sfs", "dist_col_any"),
   log_dist = c("log_dist_col", "log_dist_sfs", "log_dist_col_any"),
   dist3 = c("dist_col", "dist_sfs", "dist_col_any", "log_dist_col"),
   topo1 = c("elev", "slope", "dist_slp", "rugg"),
   topo2 = c("elev", "slope", "rugg"),
   topo3 = c("elev", "dist_slp", "rugg"),
   hab = c("closed", "crops", "urban", "water", "prot_area"),
   hab2 = c("NDVI_mean", "prot_area"),
   time = c("ttnoon", "ttnoon_sq")
)

# Define models
int1 <- list(dist ~ age_fct(ad)) # basic interactions
int2 <- list(log_dist ~ age_fct(ad)) # basic interactions with log dist
int3 <- list(dist2 ~ age_fct(ad)) # basic interactions with log dist
int4 <- list(dist3 ~ age_fct(ad)) # basic interactions with log dist

# Final models to test
models <- list(
   mod1 = form(vults, mod_elem, c("dist", "topo1", "hab", "time")),#, intr = c(int1, list(topo1 ~ zone_fct(z_1)))),
   mod2 = form(vults, mod_elem, c("dist", "topo2", "hab", "time")),#, intr = c(int1, list(topo2 ~ zone_fct(z_1)))),
   mod3 = form(vults, mod_elem, c("dist", "topo3", "hab", "time"))#, intr = c(int1, list(topo3 ~ zone_fct(z_1))))#,
   # mod4 = form(vults, mod_elem, c("dist", "topo2", "hab"), intr = c(int1, list(topo2 ~ zone_fct(z_1), hab ~ zone_fct(z_1)))),
   # mod5 = form(vults, mod_elem, c("dist", "topo2", "hab2"), intr = c(int1, list(topo2 ~ zone_fct(z_1)))),
   # mod6 = form(vults, mod_elem, c("dist", "topo2", "hab2"), intr = c(int1, list(topo2 ~ zone_fct(z_1), hab2 ~ zone_fct(z_1)))),
   # mod7 = form(vults, mod_elem, c("log_dist", "topo2", "hab"), intr = c(int2, list(topo2 ~ zone_fct(z_1)))),
   # mod8 = form(vults, mod_elem, c("log_dist", "topo2", "hab"), intr = c(int2)),
   # mod9 = form(vults, mod_elem, c("dist2", "topo2", "hab"), intr = c(int3, list(topo2 ~ zone_fct(z_1)))),
   # mod10 = form(vults, mod_elem, c("dist3", "topo2", "hab"), intr = c(int4, list(topo2 ~ zone_fct(z_1))))
)

# Set model names
for (i in seq_along(models)){
   attr(models[[i]], "model") <- paste0("mod", i)
}


# FIT MODELS --------------------------------------------------------------

# At this point we can get rid of most of the working environment to free up memory
rm(list = ls()[!ls() %in% c("cv_ids", "models")])
gc()

# Load function to fit and predict
source("R/functions/fitCVssf.R")

for(m in seq_along(models)){
   
   cv_results <- future_map(cv_ids, ~fitCVssm(train_data = "data/working/data_height_ready_noextrm.rds",
                                              test_data = "data/working/data_height_test.rds",
                                              test_ids = .x, model = models[[m]], seed = 478476),
                            .options = future_options(seed = 478476))
   
   cv_results <- do.call("rbind", cv_results)
   
   # Save cross-validation results
   saveRDS(cv_results, paste0("output/cv_results_", m, ".rds"))
}

# ITERATE FROM HERE -------------------------------------------------------

i = 1
mod_sel <- models[[i]]


# Prepare data for TMB ----------------------------------------------------

# Variables needed for the process
vv <- c("bird_id", "dt", all.vars(mod_sel))

# Subset the necessary variables
mod_data <- vults %>% 
   dplyr::select(all_of(vv))

# Standardize response and covariates
mod_data <- mod_data %>% 
   mutate(height = scale(height, center = F)) %>% 
   mutate(across(.cols = -one_of(c("bird_id", "height", "dt")), ~scale(.x, center = TRUE)),
          intcp = 1)

# Remove last dt of each group
dts <- mod_data %>% 
   group_by(bird_id) %>%
   slice(-tail(row_number(), 1)) %>%
   pull(dt)

data.bundle <- list("z" = mod_data$height, 
                    "dtime" = dts,
                    "X" = dplyr::select(mod_data, -c(bird_id, height, dt)) %>% as.matrix(),
                    # "Y" = dplyr::select(mod_data, intcp, slope, rugg) %>% as.matrix(),
                    "B" = 0.1,
                    "N" = nrow(mod_data),
                    "K" = ncol(dplyr::select(mod_data, -c(bird_id, height, dt))),
                    # "L" = ncol(dplyr::select(mod_data, intcp, slope, rugg)),
                    "J" = n_distinct(mod_data$bird_id),
                    "n_j" = mod_data %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n),
                    "group" = rep(0:(n_distinct(mod_data$bird_id)-1), times = mod_data %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n)), 
                    "lambda" = 1) 

# Define initial values
param <- list("mu_alpha" = rep(0, data.bundle$K),
              "lsig_alpha" = rep(log(0.5), data.bundle$K),
              "delta_alpha" = matrix(0, ncol = data.bundle$K, nrow = data.bundle$J),
              "mu_lsig_err" = 0,
              "sig_lsig_err" = 0,
              "delta_lsig_err" = rnorm(data.bundle$J, 0, 0.01),
              "lsigma" = log(1), "lbeta" = log(0.1),
              "ztrans" = data.bundle$z)

# From previous fit
# fit <- readRDS("output/fit_hgt_smm_mod1.rds")
# param <- as.list(fit$par.fixed)
# param <- append(param, list("ztrans" = data.bundle$z))

# Fit TMB model with obs error and covts ----------------------------------

compile("R/functions/TMB/hier_ssm_ou.cpp")
dyn.load(dynlib("R/functions/TMB/hier_ssm_ou"))

openmp(3)

Obj <- MakeADFun(data = data.bundle, parameters = param, random = c("ztrans", "delta_alpha", "delta_lsig_err"), DLL = "hier_ssm_ou", silent = F)
   
# Neg Log Likelihood minimization
lb <- c(rep(-5, data.bundle$K), rep(-5, data.bundle$K), -3, -3, -3, -3)
ub <- c(rep(5, data.bundle$K), rep(2, data.bundle$K), 2, 2, 1, 1)
   
Opt <- NULL

Opt <- optim(Obj$par, Obj$fn, Obj$gr,
             method = "L-BFGS-B", #Obj$method,
             lower = lb, upper = ub,
             control = list(maxit = 1e5, factr = 1e9, trace = 0), hessian = TRUE)

fit_summ <- sdreport(Obj, getReportCovariance = FALSE)

summary(fit_summ, "fixed")

saveRDS(fit_summ, "output/fit_hgt_smm_mod1.rds")

fit_summ <- readRDS("hpc/output/hgt_ssm_summ.rds")
Obj <- readRDS("hpc/output/ssm_obj.rds")
Opt <- readRDS("hpc/output/ssm_opt.rds")



TMBhelper::TMBAIC(Opt)
      
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
