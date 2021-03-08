# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_ssf_ready.rds")

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


# Define models to compare ------------------------------------------------

# Define model elements
mod_elem <- list(
    dist = c("dist_col", "dist_sfs", "dist_col_any"),
    dist2 = c("log_dist_col", "dist_sfs", "dist_col_any"),
    log_dist = c("log_dist_col", "log_dist_sfs", "log_dist_col_any"),
    topo1 = c("elev", "slope", "dist_slp", "rugg"),
    topo2 = c("elev", "slope", "rugg"),
    topo3 = c("elev", "dist_slp", "rugg"),
    topo4 = c("elev", "slope", "log_dist_slp", "rugg"),
    hab = c("closed", "crops", "urban", "water", "prot_area"),
    hab2 = c("NDVI_mean", "prot_area"),
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
int1 <- list(dist ~ age_fct(ad), mov ~ ttnoon, mov ~ ttnoon_sq) # basic interactions
int2 <- list(log_dist ~ age_fct(ad), mov ~ ttnoon, mov ~ ttnoon_sq) # basic interactions with log dist
int3 <- list(dist2 ~ age_fct(ad), mov ~ ttnoon, mov ~ ttnoon_sq) # basic interactions with log dist

# Final models to test
models <- list(
    mod1 = form(vults, mod_elem, c("dist", "topo1", "hab", "mov"), intr = c(int1, list(mov ~ res, topo1 ~ zone_fct(z_1)))),
    mod2 = form(vults, mod_elem, c("dist", "topo2", "hab", "mov"), intr = c(int1, list(mov ~ res, topo2 ~ zone_fct(z_1)))),
    mod3 = form(vults, mod_elem, c("dist", "topo3", "hab", "mov"), intr = c(int1, list(mov ~ res, topo3 ~ zone_fct(z_1)))),
    mod4 = form(vults, mod_elem, c("dist", "topo4", "hab", "mov"), intr = c(int1, list(mov ~ res, topo4 ~ zone_fct(z_1)))),
    mod5 = form(vults, mod_elem, c("dist", "topo2", "hab2", "mov"), intr = c(int1, list(mov ~ res, topo2 ~ zone_fct(z_1)))),
    mod6 = form(vults, mod_elem, c("dist", "topo2", "hab2", "mov"), intr = c(int1, list(mov ~ res, topo2 ~ zone_fct(z_1), hab2 ~ zone_fct(z_1)))),
    mod7 = form(vults, mod_elem, c("log_dist", "topo2", "hab", "mov"), intr = c(int2, list(mov ~ res, topo2 ~ zone_fct(z_1)))),
    mod8 = form(vults, mod_elem, c("log_dist", "topo2", "hab", "mov"), intr = c(int2, list(mov ~ res))),
    mod9 = form(vults, mod_elem, c("dist2", "topo2", "hab", "mov"), intr = c(int3, list(mov ~ res, topo2 ~ zone_fct(z_1)))),
    mod10 = form(vults, mod_elem, c("dist", "topo2", "hab", "mov"), intr = c(int1, list(dist ~ res, topo2 ~ res, hab ~res,
                                                                                        mov ~ res, topo2 ~ zone_fct(z_1))))
)

# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- paste0("mod", i)
}

model <- models$mod7

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

# # From previous fit?
# ini_val$beta[c(1,2,3,13:18)] <- c(-2,-1,0, 2,2, 2,2, -2,-2)

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