# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(rstan)

options(mc.cores = parallel::detectCores() - 1)
rstan_options(auto_write = TRUE,
              javascript = FALSE)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- read_rds("data/working/data_height_ready.rds")


# Prepare variables -------------------------------------------------------

# Subset individuals?
set.seed(93487)

vults <- vults %>%
    filter(bird_id %in% sample(unique(vults$bird_id), 10))

# Remove records with no height
vults <- vults %>% 
    filter(!is.na(height))

# Change variable names to make them more readable
vults <- vults %>% 
  rename(elev = srtm0,
         rugg = vrm3)

# Define variables that can be standardized
vars_to_st <- c("height", "elev", "slope", "rugg", "dist_col", "dist_col_any", "dist_sfs", "NDVI_mean")

# Standardize covariates
vults <- vults %>%
  mutate(across(.cols = all_of(vars_to_st), ~scale(.x, center = F)))

# Create covariate matrix for mean height
X <- vults %>% 
    mutate(intcp = 1) %>% 
    dplyr::select(intcp, elev, slope, rugg, dist_col, dist_col_any, dist_sfs)

# Create covariate matrix for error
Y <- vults %>% 
  mutate(intcp = 1) %>% 
  dplyr::select(intcp, slope, rugg)

# Prepare dt. Stan doesn't allow NAs in data. Change NA by zero (these will not be used in the model anyway)
dt <- vults$dt
dt[is.na(dt)] <- 0

# Create data bundle
data.bundle <- list(N = nrow(vults),
                    K = ncol(X),
                    L = ncol(Y),
                    J = n_distinct(vults$bird_id),
                    n_j = as.numeric(table(vults$bird_id)),
                    group = rep(1:n_distinct(vults$bird_id), as.numeric(table(vults$bird_id))),
                    z = vults$height[,1], 
                    dt = dt,
                    X = as.matrix(X),
                    Y = as.matrix(Y),
                    hmu_mu_alpha = rep(0, ncol(X)),
                    hsig_mu_alpha = c(1, rep(0.5, ncol(X)-1)),
                    hsig_sig_alpha =  rep(0.5, ncol(X)),
                    hmu_mu_beta = rep(0, ncol(Y)),
                    hsig_mu_beta = c(1, rep(0.5, ncol(Y)-1)),
                    hsig_sig_beta =  rep(0.5, ncol(Y)),
                    # hmu_mu_sig_err = 20/attr(vults$height, "scaled:scale"),
                    # hsig_mu_sig_err = 50/attr(vults$height, "scaled:scale"),
                    # hsig_sig_err = 50/attr(vults$height, "scaled:scale"),
                    hmu_phi = 1,
                    hsig_phi = 1)

# Save data bundle
# saveRDS(data.bundle, "model_tests/output/hier_ssm_data_stan.rds")

# Fit model ---------------------------------------------------------------

inilat <- data.bundle$z
inilat[inilat < 0] <- min(abs(inilat))

# Define initial values
init = function() list(
    mu_alpha = rnorm(data.bundle$K, 0, 0.5),
    sig_alpha = rexp(data.bundle$K, 1),
    delta_alpha = matrix(rnorm(data.bundle$K * data.bundle$J), nrow = data.bundle$J, ncol = data.bundle$K),
    mu_beta = rnorm(data.bundle$L, 0, 0.5),
    sig_beta = rexp(data.bundle$L, 1),
    delta_beta = matrix(rnorm(data.bundle$L * data.bundle$J), nrow = data.bundle$J, ncol = data.bundle$L),
    # mu_sig_err = rexp(1, 10),
    # sig_sig_err = rexp(1, 10),
    # delta_err = rexp(data.bundle$J, 10),
    tphi = rnorm(data.bundle$J, 0, 0.5),
    tnu = rnorm(data.bundle$J, 20, 3),
    tshape = rnorm(data.bundle$J, 0, 0.5),
    zlat = rexp(data.bundle$z, 10)
)


# Define parameters to plot
paramToPlot <- c("mu_alpha", "sig_alpha", "mu_beta", "sig_beta") 
param <- c("zlat", "phi", "nu", "shape", paramToPlot)

# Compile model
stan_mod <- stan_model(file = "model_tests/stan/hier_ssm_AR1_gamma_t_err_covts.stan")

# Run model
start_time <- Sys.time()
fit <- sampling(stan_mod,
                data = data.bundle,
                pars = param, init = init,
                iter = 1500, chains = 6, warmup = 1000, thin = 1,
                # cores = getOption("mc.cores", 6),
                control = list(adapt_delta = 0.85),
                refresh = max(2000/50,1),
                save_warmup = F)
end_time <- Sys.time()
end_time - start_time

stan_trace(fit, pars = paramToPlot, inc_warmup = F)
print(fit, pars = paramToPlot, probs = c(0.025, 0.5, 0.975),
      digits_summary = 3, use_cache = F)

print(fit, pars = "shape", probs = c(0.025, 0.5, 0.975),
      digits_summary = 3, use_cache = F)

print(fit, pars = "nu", probs = c(0.025, 0.5, 0.975),
      digits_summary = 3, use_cache = F)

pairs(fit, pars = c("phi[9]", "shape[9]"))

# Save model
saveRDS(fit, "model_tests/output/hier_ssm_AR1_gamma_t_err_covts_fit.rds")

# Read in model
fit <- readRDS("model_tests/output/hier_ssm_exp_t_fit_stan_hpc.rds")

print(fit, pars = param, probs = c(0.025, 0.5, 0.975),
      digits_summary = 3, use_cache = F)


saveRDS(vults, "output/vults_height.rds")

vults <- readRDS("output/vults_height.rds")







# Define model formula
formula <- risk ~ 1 + risk_t1 + srtm + slope + vrm3 +
    dist_col + dist_sfs + dist_col_any +
    prot_area + ttnoon + ttnoon_sq + 
    (0 + risk_t1|bird_id) + (0 + srtm|bird_id) + (0 + slope|bird_id) + (0 + vrm3|bird_id) +
    (0 + dist_col|bird_id) + (0 + dist_sfs|bird_id) + (0 + dist_col_any|bird_id) +
    (0 + ttnoon|bird_id) + (0 + ttnoon_sq|bird_id) 

height_model <- glmmTMB(formula, family = binomial, data = vults, doFit = FALSE)

# Set random effect with very large variance for step_id (see Muff et al. 2020)
nrm <- length(height_model$parameters$theta)
str_theta <- height_model$parameters$theta

# Define starting values
ini_val <- list(beta = height_model$parameters$beta,
                theta = str_theta)

# # From previous fit?
# ini_val <- list(beta = mod_sum[[9]]$coefficients$cond[,"Estimate"],
#                 theta = sapply(mod_sum[[9]]$varcor[[1]], attr, "stddev"))

# # From previous fit, in case there are new variables not present in previous
# ini_val$beta <- mod_sum[[10]]$coefficients$cond[,"Estimate"]
# ini_val$theta[-3] <- sapply(mod_sum[[10]]$varcor[[1]], attr, "stddev")
# # 
# # ini_val$beta[c(4:6)] <- c(0.2, 0.2, 0.2)
# ini_val$theta[3] <- log(10)

# remove names just in case
ini_val <- lapply(ini_val, unname)


startime <- Sys.time()
height_fit_rm <- glmmTMB(formula, family = binomial, data = vults,
                      start = ini_val
                      # ,control = glmmTMBControl(optimizer = optim,
                      #                          optArgs=list(method="BFGS"))
)
endtime <- Sys.time()
print(endtime - startime)

summary(height_fit_rm)

x <- -6:6

y <- -0.15*x + 0.13*x^2

plot(x, y)

# Save model fit


saveRDS(ssf_fit_rm, "output/height_fit_rm.rds")


# Explore results ---------------------------------------------------------

ssf_fit_rm <- read_rds("output/ssf_fit_rm.rds")
summary(ssf_fit_rm)


mod_sum <- read_rds("data/working/model_summ.rds")

mod_sum <- c(mod_sum, mod25a_habclass = summary(ssf_fit_rm))

write_rds(mod_sum, "data/working/model_summ.rds")

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr