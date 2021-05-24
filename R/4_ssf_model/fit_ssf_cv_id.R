# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)
library(furrr)

# # Set future maxsize to 850MB
# options(future.globals.maxSize = 850*1024^2)
# 
# future::plan("multisession")


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_ssf_ready.rds")

# test data
test_data <- readRDS("data/working/data_ssf_test.rds")

# Remove birds for which age is unknown
vults <- vults %>%
    filter(age_fct != "unknown")

test_data <- test_data %>%
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

test_data <- test_data %>% 
    rename(elev = srtm0,
           rugg = vrm3,
           dist_slp = dist_slp_m) %>% 
    mutate(log_dist_col = log(dist_col),
           log_dist_col_any = log(dist_col_any),
           log_dist_sfs = log(dist_sfs),
           log_dist_slp = log(dist_slp))

# Make response variable numeric
vults <- vults %>% 
    mutate(case = if_else(case_ == TRUE, 1, 0))

test_data <- test_data %>% 
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


# Prepare train and validation --------------------------------------------

# Add group index
vults <- vults %>% 
    group_by(age_fct, zone_fct) %>% 
    mutate(cv_group = cur_group_id()) %>% 
    ungroup()

test_data <- test_data %>% 
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

cv_ids <- map(1:20, ~f(bird_ids))

# Set group names
for (i in seq_along(cv_ids)){
    attr(cv_ids[[i]], "id") <- paste0("cv", i)
}


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
    mod9 = form(vults, mod_elem, c("dist2", "topo2", "hab", "mov"), intr = c(int3, list(mov ~ res, topo2 ~ zone_fct(z_1))))
)

# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- names(models)[i]
}


# FIT MODELS --------------------------------------------------------------

# Prepare training data ---------------------------------------------------
# test id
i <- 20
# Model
m <- 2

test_ids <- cv_ids[[i]]
model <- models[[m]]

# Extract training data
train <- vults %>% 
    mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
    filter(!cv_id %in% test_ids)

# Extract variables needed for the model
vv <- all.vars(model)

train <- train %>% 
    dplyr::select(all_of(vv))

# Define variables that can be standardized
vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean", "sl_")

# Standardize covariates
train <- train %>%
    mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))


# Fit model ---------------------------------------------------------------

# Specify model
ssf_model <- glmmTMB(model, family = poisson, data = train, doFit = FALSE)

# Set random effect with very large variance for step_id (see Muff et al. 2020)
nrm <- length(ssf_model$parameters$theta)
str_theta <- ssf_model$parameters$theta
str_theta[nrm] <- log(1e3)

# Define starting values
ini_val <- list(beta = ssf_model$parameters$beta,
                theta = str_theta)

# remove names just in case
ini_val <- lapply(ini_val, unname)

# Fit model
rm(ssf_model) # to free memory

ssf_fit <- glmmTMB(model, family = poisson, data = train,
                      map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                      start = ini_val)

saveRDS(ssf_fit, paste0("output/ssf_fit_test", i, "_mod", m,".rds"))