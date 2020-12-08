# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(glmmTMB)

setwd("/home/crvfra001/vults")

# Load data ---------------------------------------------------------------

vults_mod <- readRDS("data/vults_mod.rds")

# Define model formula
formula <- case_num ~ -1 + srtm + slope + vrm3 +
    dist_col + dist_sfs + dist_col_any +
    dist_col:subad + dist_col:juv +
    dist_sfs:subad + dist_sfs:juv +
    dist_col_any:subad + dist_col_any:juv +
    prot_area +
    sl_ + sl_:(ttnoon + ttnoon_sq) +
    (0 + srtm|bird_id) + (0 + slope|bird_id) + (0 + vrm3|bird_id) +
    (0 + dist_col|bird_id) + (0 + dist_sfs|bird_id) + (0 + dist_col_any|bird_id) +
    (0 + sl_|bird_id) + (0 + sl_:ttnoon|bird_id) + (0 + sl_:ttnoon_sq|bird_id) +
    (1|step_id_)

ssf_model <- glmmTMB(formula, family = poisson, data = vults_mod, doFit = FALSE)

# Set random effect with very large variance for step_id (see Muff et al. 2020)
nrm <- length(ssf_model$parameters$theta)
str_theta <- ssf_model$parameters$theta
str_theta[nrm] <- log(1e3)

# Define starting values
ini_val <- list(beta = ssf_model$parameters$beta,
                theta = str_theta)

# remove names just in case
ini_val <- lapply(ini_val, unname)


ssf_fit_rm <- glmmTMB(formula, family = poisson, data = vults_mod,
                      map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                      start = ini_val
                      # ,control = glmmTMBControl(optimizer = optim,
                      #                          optArgs=list(method="BFGS"))
)

saveRDS(ssf_fit_rm, "ssf_fit_rm.rds")
