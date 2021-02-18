# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_ssf_ready.rds")

# Remove birds for which age is unknown
vults <- vults %>%
    filter(age_fct != "unknown")

# Subset individuals?
# set.seed(93487)
# vults <- vults %>% 
#     filter(bird_id %in% unique(vults$bird_id)[sample.int(70, 25)])

# Create a unique id for each data point and make step codes unique
vults <- vults %>%
    mutate(id = row_number(),
           step_id_ = paste(bird_id, step_id_, sep = "_"))

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

# Make response variable numeric
vults <- vults %>% 
    mutate(case = if_else(case_ == TRUE, 1, 0))

# We also need a numeric resolution variable
vults <- mutate(vults,
                res = as.numeric(str_remove(res_fct, "res_")))

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
    dist2 = c("log_dist_col", "dist_sfs", "dist_col_any"),
    topo2 = c("elev", "slope", "rugg"),
    hab = c("closed", "crops", "urban", "water", "prot_area"),
    mov = c("sl_")
)

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
    rdm <- c(paste("(0 +", elem,"|bird_id)"), "(1|step_id_)")
    
    # Write formula
    reformulate(c(-1, fixed, rdm), "case")
}

# Define models
int3 <- list(dist2 ~ age_fct(ad), mov ~ ttnoon, mov ~ ttnoon_sq) # basic interactions with log dist


# Final models to test
model <- form(vults, mod_elem, c("dist2", "topo2", "hab", "mov"), intr = c(int3, list(mov ~ res, topo2 ~ zone_fct(z_1))))


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
rm(ssf_model) # To free memory
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