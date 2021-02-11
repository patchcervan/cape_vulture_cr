# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(dplyr)
library(magrittr)
library(glmmTMB)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_ssf_ready.rds")

# Remove birds for which age in unknown
vults <- vults %>%
    filter(age_fct != "unknown")

# Create a unique id for each data point and make step codes unique
vults <- vults %>%
    mutate(id = row_number(),
           step_id_ = paste(bird_id, step_id_, sep = "_"))

# Change variable names to make them more readable
vults <- vults %>% 
    rename(elev = srtm0,
           rugg = vrm3)

# Make response variable numeric
vults <- vults %>% 
    mutate(case = if_else(case_ == TRUE, 1, 0))

# Remove steps for which elevation is NA
steps_rm <- vults %>% 
    filter(is.na(elev)) %>% 
    summarize(steps = unique(step_id_)) %>% 
    pull(steps)

vults <- vults %>% 
    filter(!(step_id_ %in% steps_rm))

# Specify model -----------------------------------------------------------

# Define model elements
mod_elem <- list(
    dist = c("dist_col", "dist_sfs", "dist_col_any"),
    topo1 = c("elev", "slope", "dist_slp", "rugg"),
    topo2 = c("elev", "slope", "rugg"),
    topo3 = c("elev", "dist_slp", "rugg"),
    hab = c("closed", "crops", "urban", "water", "prot_area")
)

# Helper function to formulate model from elements
form <- function(mod_elem, include, age = NULL, zone = NULL){
    
    # Extract variables
    elem <- unlist(mod_elem[include])
    
    # Define movement kernel variables
    mov <- c("sl_", "sl_:(ttnoon + ttnoon_sq)")
    
    # Define interactions
    fixed <- elem # In case there are no interactions
    
    if(!is.null(age)){
        inter_age <- paste(rep(unlist(mod_elem[age]), each = 2), c("subad", "juv"), sep = ":")
        fixed <- c(fixed, inter_age)
    }
    
    if(!is.null(zone)){
        inter_zone <- paste(rep(unlist(mod_elem[zone]), each = 3), c("z_2", "z_3", "z_4"), sep = ":")
        fixed <- c(fixed, inter_zone)
    }
    
    # Define random effects
    rdm <- c(paste("(0 +", elem,"|bird_id)"), "(1|step_id_)")
    
    # Write formula
    reformulate(c(-1, fixed, rdm), "case")
}

# Specify model
model <- form(mod_elem, c("dist", "topo1", "hab", "mov"), age = "dist")


# Standardize covariates --------------------------------------------------

# Extract variables needed for the model
vv <- all.vars(model)

vults <- vults %>% 
    dplyr::select(all_of(vv))

# Define variables that can be standardized
vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean", "sl_")

# Standardize covariates
vults <- vults %>%
    mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))


# Fit model ---------------------------------------------------------------

# Set random effect with very large variance for step_id (see Muff et al. 2020)
ssf_model <- glmmTMB(model, family = poisson, data = vults, doFit = FALSE)
nrm <- length(ssf_model$parameters$theta)
str_theta <- ssf_model$parameters$theta
str_theta[nrm] <- log(1e3)

# Define starting values
ini_val <- list(beta = ssf_model$parameters$beta,
                theta = str_theta)

# remove names just in case
ini_val <- lapply(ini_val, unname)

# Fit
ssf_fit_rm <- glmmTMB(model, family = poisson, data = vults,
                      map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                      start = ini_val
                      # ,control = glmmTMBControl(optimizer = optim,
                      #                          optArgs=list(method="BFGS"))
)

saveRDS(ssf_fit_rm, "output/ssf_fit_rm.rds")


# Explore results ---------------------------------------------------------

ssf_fit_rm <- readRDS("hpc/output/ssf_fit_rm.rds")
summary(ssf_fit_rm)


mod_sum <- read_rds("data/working/model_summ.rds")

mod_sum <- c(mod_sum, mod25a_habclass = summary(ssf_fit_rm))

write_rds(mod_sum, "data/working/model_summ.rds")

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr