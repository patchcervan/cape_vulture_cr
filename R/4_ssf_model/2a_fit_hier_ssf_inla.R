# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(INLA)



# Load data ---------------------------------------------------------------

# Vulture data
vults <- read_rds("data/working/data_ssf_ready.rds")

# Habitat metadata
hab_meta <- read_csv("data/raw/copernicus_codes.csv")



# Prepare variables -------------------------------------------------------

# Subset individuals?
set.seed(93487)
vults <- vults %>% 
    filter(bird_id %in% unique(vults$bird_id)[sample.int(70, 20)])

# Create a unique id for each data point and make step codes unique
vults_mod <- vults %>% 
    mutate(id = row_number(),
           step_id_ = paste(bird_id, step_id_, sep = "_"))

# Remove birds for which age in unknown
vults_mod <- vults_mod %>% 
      filter(!is.na(age))

# Change habitat codes
vults_mod <- vults_mod %>% 
    left_join(dplyr::select(hab_meta, "Map code", "custom_code"), by = c("land_use" = "Map code")) %>% 
    rename(land_cov = custom_code)


# Make dummy variables from factors (I also keep the factors)
vults_mod <- vults_mod %>%  
      mutate(i = 1,
             land_cov_fct = land_cov) %>% 
      spread(land_cov, i, fill = 0) %>% 
      mutate(i = 1,
             zone = factor(zone, labels = paste("z", 1:4, sep = "_")),
             zone_fct = zone) %>% 
      spread(zone, i, fill = 0) %>% 
      mutate(i = 1,
             res = factor(res, labels = paste("res", c(1,24), sep = "_")),
             res_fct = res) %>% 
      spread(res, i, fill = 0) %>% 
      mutate(i = 1,
             age = factor(age),
             age_fct = age) %>% 
      spread(age, i, fill = 0)


# Fit model ---------------------------------------------------------------

# Standardize covariates and make response and categorical variables factors
vults_mod <- vults_mod %>% 
    dplyr::select(case_, srtm, slope, vrm3, dist_col, dist_roost,
                  land_cov_fct, bare, crops, forest_c, forest_o, grass, shrub, urban, water,
                  prot_area, NDVI_mean,
                  zone_fct, z_1, z_2, z_3, z_4, age_fct, juv, subad, ad, res_fct, res_1, res_24, bird_id, step_id_, 
                  sl_, ttnoon, ttnoon_sq) %>% 
    mutate(across(.cols = c("srtm", "slope", "vrm3", "dist_col", "dist_roost", "NDVI_mean", "sl_"), scale)) %>% 
    mutate(case_num = if_else(case_ == TRUE, 1, 0)) %>% 
    mutate(case_fct = factor(case_num))

# Remove original data set to free memory
rm(vults)

# Define model
formula <- case_num ~ -1 + srtm + slope + vrm3 + 
  crops + forest_c + forest_o + grass + shrub + urban + water + dist_col + prot_area +
  (srtm + slope + vrm3 + crops + forest_c + forest_o + grass + shrub + urban + water + dist_col + prot_area):z_2 +
  (srtm + slope + vrm3 + crops + forest_c + forest_o + grass + shrub + urban + water + dist_col + prot_area):z_3 +
  (srtm + slope + vrm3 + crops + forest_c + forest_o + grass + shrub + urban + water + dist_col + prot_area):z_4 +
  dist_col:subad + dist_col:juv +
  sl_ + sl_:(ttnoon + ttnoon_sq) +
    f(step_id_, model = "iid", 
      hyper = list(theta = list(initial = log(1e-6), fixed = T))) +
    f(bird_id1, crops, model = "iid",
      hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id2, forest_c, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id3, forest_o, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id4, grass, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id5, shrub, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id6, urban, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id7, water, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(3, 0.05)))) +
  f(bird_id8, dist_col, model = "iid",
    hyper = list(theta = list(initial = log(1), fixed = F, prior = "pc.prec", param = c(1, 0.05))))

# I need to replicate as many bird_id variables as random effects want to include
nvars <- 8 - 1 # minus one because we cbind another column
idcols <- cbind(vults_mod$bird_id, replicate(nvars, vults_mod$bird_id))
idcols <- as_tibble(idcols, .name_repair = "minimal")
names(idcols) <- paste0("bird_id", 1:(nvars + 1))

vults_mod <- cbind(vults_mod, idcols)

startime <- Sys.time()
ssf_fit_rm <- inla(formula,
                  control.fixed = list(
                      mean = 0, prec = 0.5),
                     family = "Poisson", data = vults_mod, num.threads = 4)
endtime <- Sys.time()
endtime - startime


# Save results
write_rds(ssf_fit_rm, file = "output/ssf_fit_rm_inla.rds")


# Explore results ---------------------------------------------------------
ssf_fit_rm <- read_rds("output/ssf_fit_rm_inla.rds")
summary(ssf_fit_rm)

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr