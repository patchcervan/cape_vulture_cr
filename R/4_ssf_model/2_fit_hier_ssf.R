# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- read_rds("data/working/data_ssf_ready.rds")

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv")


# Prepare variables -------------------------------------------------------

# Subset individuals?
# set.seed(93487)
# vults <- vults %>% 
#     filter(bird_id %in% unique(vults$bird_id)[sample.int(70, 25)])

# Create a unique id for each data point and make step codes unique
vults_mod <- vults %>%
    mutate(id = row_number(),
           step_id_ = paste(bird_id, step_id_, sep = "_"))

# Remove birds for which age in unknown
vults_mod <- vults_mod %>%
    filter(!is.na(age))

# Change habitat codes
vults_mod <- vults_mod %>%
    left_join(dplyr::select(hab_meta, "Map code", "class_code"), by = c("land_use" = "Map code")) %>%
    rename(land_cov = class_code)

# There are a few NA NDVI corresponding mostly to locations over the ocean.
# I will assign the mean NDVI value
vults_mod %>%
    filter(is.na(NDVI_mean)) %>%
    group_by(land_cov) %>%
    summarize(n = n())

vults_mod <- vults_mod %>%
    mutate(NDVI_mean = if_else(is.na(NDVI_mean), mean(NDVI_mean, na.rm = T), NDVI_mean))


# Make dummy variables from factors (I also keep the factors)
vults_mod <- vults_mod %>%
    mutate(i = 1,
           land_cov_fct = land_cov) %>%
    spread(land_cov, i, fill = 0) %>%
    mutate(i = 1,
           zone = factor(zone, levels = 1:4, labels = paste("z", 1:4, sep = "_")),
           zone_fct = zone) %>%
    spread(zone, i, fill = 0) %>%
    mutate(i = 1,
           res = factor(res, levels = c(1, 8, 24), labels = paste("res", c(1, 8, 24), sep = "_")),
           res_fct = res) %>%
    spread(res, i, fill = 0) %>%
    mutate(i = 1,
           age = factor(age, levels = c("juv", "subad", "ad")),
           age_fct = age) %>%
    spread(age, i, fill = 0)



# Fit model ---------------------------------------------------------------

# Standardize covariates and make response and categorical variables factors
vults_mod <- vults_mod %>%
    filter(age_fct != "unknown") %>%
    dplyr::select(case_, srtm = srtm0, slope, dist_slp, vrm3, dist_col, dist_roost, dist_sfs, dist_col_any,
                  land_cov_fct, closed, crops, urban, water,
                  prot_area, NDVI_mean,
                  zone_fct, z_1, z_2, z_3, z_4, age_fct, juv, subad, ad, res_fct, res_1, res_24, bird_id, step_id_,
                  sl_, ttnoon, ttnoon_sq) %>%
    mutate(across(.cols = c("srtm", "slope", "dist_slp", "vrm3", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean", "sl_"), scale)) %>%
    mutate(case_num = if_else(case_ == TRUE, 1, 0)) %>%
    mutate(case_fct = factor(case_num))

# Remove original data set to free memory
rm(vults)

saveRDS(vults_mod, "output/vults_mod.rds")

vults_mod <- readRDS("output/vults_mod.rds")

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
ssf_fit_rm <- glmmTMB(formula, family = poisson, data = vults_mod,
                      map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                      start = ini_val
                      # ,control = glmmTMBControl(optimizer = optim,
                      #                          optArgs=list(method="BFGS"))
)
endtime <- Sys.time()
print(endtime - startime)
print(i)

# Save model fit


saveRDS(ssf_fit_rm, "output/ssf_fit_rm.rds")


# Explore results ---------------------------------------------------------

ssf_fit_rm <- read_rds("output/ssf_fit_rm.rds")
summary(ssf_fit_rm)


mod_sum <- read_rds("data/working/model_summ.rds")

mod_sum <- c(mod_sum, mod25a_habclass = summary(ssf_fit_rm))

write_rds(mod_sum, "data/working/model_summ.rds")

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr