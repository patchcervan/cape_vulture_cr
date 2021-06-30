# 16-02-2021

# In this script we fit a hierarchical binomial model to the height data of the 
# vulture data set using the UCT HPC cluster

rm(list = ls())

library(tidyverse)
library(glmmTMB)
# library(splines)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_height_ready_noextrm.rds")

# Remove records with no height
vults <- vults %>% 
    filter(!is.na(height))



# Prepare variables -------------------------------------------------------

# Add log of distances since the effect probably saturates
vults <- vults %>% 
    mutate(log_dist_col = log(dist_col),
           log_dist_col_any = log(dist_col_any),
           log_dist_sfs = log(dist_sfs),
           log_dist_slp = log(dist_slp))

# Create a variable that is one if the bird is flying at rotor height and
# zero otherwise
vults <- vults %>%
    mutate(risk = if_else(height > 300, 0, 1))

# Proportion of points at risk
vults %>% 
    group_by(risk) %>% 
    summarize(n = n()) %>% 
    mutate(total = sum(.$n),
           prop = n / total)

# Minimum height measurement gives us idea of error magnitude
min(vults$height, na.rm = T)

# make a lagged risk variable
vults <- vults %>% 
    group_by(bird_id) %>% 
    mutate(risk_t1 = lag(risk)) %>% 
    ungroup()

# We also define a variable phi that takes on the values:
# 1 if risk_t1 = 1 and -1 if risk_t1 = 0. This will be used
# to define the autocorrelation function
vults <- vults %>% 
    mutate(phi = if_else(risk_t1 == 1, 1, -1))

# We also need a numeric resolution variable
vults <- mutate(vults,
                res = as.numeric(str_remove(res_fct, "res_")))

# Recalculate dt
vults <- vults %>% 
    group_by(bird_id) %>% 
    mutate(dt = as.numeric(difftime(datetime, lag(datetime), units = "hours"))) %>% 
    ungroup()

# We further set risk_t1 = 0, phi = 0 and dt = 100 for the first values of each bird
# so that autocorrelation does not come into the equation
vults <- vults %>% 
    mutate(risk_t1 = if_else(is.na(dt), 0, risk_t1),
           phi = if_else(is.na(dt), 0, phi),
           dt = if_else(is.na(dt), 100, dt))

# Also the autocorrelation should be reduced with time therefore we set
# an exponential decay with dt
vults <- vults %>% 
    mutate(phi = phi * exp(-dt))

# vults %>% 
#     dplyr::select(datetime, dt, risk, risk_t1, phi) %>% 
#     print(n = 20)


# Define models to compare ------------------------------------------------

form <- function(fixed, intrc=NULL, rdm=NULL, other=NULL, resp){
    
    mod <- fixed
    
    if(!is.null(intrc)){
        intrc <- sapply(intrc, paste, collapse = ":")
        mod <- c(mod, intrc)
    } 
    if(!is.null(rdm)){
        rdm <- paste0("(0 +", rdm, "|bird_id)")
        mod <- c(mod, rdm)
    } 
    
    if(!is.null(other)){
        mod <- c(mod, other)
    } 
    
    return(reformulate(mod, response = resp))
}

# function for quoting terms
qq <- function(...) {
    sapply(match.call()[-1], deparse)
}

# Define models
models <- list(
    mod1 = form(fixed = qq(1, dist_col, dist_col_any, dist_sfs, 
                           elev, slope, rugg, closed, crops, urban, water, prot_area),
                intrc = NULL,
                rdm = qq(1, dist_col, dist_col_any, dist_sfs, 
                         elev, slope, rugg, closed, crops, urban, water, prot_area),
                resp = qq(risk)),
    mod2 = form(fixed = qq(1, dist_col, dist_col_any, dist_sfs,
                           elev, slope, rugg, closed, crops, urban, water, prot_area,
                           ttnoon, ttnoon_sq,),
                intrc = NULL,
                rdm = qq(1, dist_col, dist_col_any, dist_sfs,
                         elev, slope, rugg, closed, crops, urban, water, prot_area,
                         ttnoon, ttnoon_sq),
                resp = qq(risk)),
    mod3 = form(fixed = qq(1, phi, dist_col, dist_col_any, dist_sfs, 
                           elev, slope, rugg, closed, crops, urban, water, prot_area,
                           ttnoon, ttnoon_sq),
                intrc = NULL,
                rdm = qq(1, dist_col, dist_col_any, dist_sfs, 
                         elev, slope, rugg, closed, crops, urban, water, prot_area,
                         ttnoon, ttnoon_sq),
                resp = qq(risk)),
    mod4 = form(fixed = qq(1, phi, log_dist_col, dist_col, dist_col_any, dist_sfs, 
                           elev, slope, rugg, closed, crops, urban, water, prot_area,
                           ttnoon, ttnoon_sq),
                intrc = NULL,
                rdm = qq(1, log_dist_col, dist_col, dist_col_any, dist_sfs, 
                         elev, slope, rugg, closed, crops, urban, water, prot_area,
                         ttnoon, ttnoon_sq),
                resp = qq(risk)),
    mod5 = form(fixed = qq(1, phi, dist_col, dist_col_any, dist_sfs, 
                           elev, dist_slp, slope, rugg, closed, crops, urban, water, prot_area,
                           ttnoon, ttnoon_sq),
                intrc = NULL,
                rdm = qq(1, dist_col, dist_col_any, dist_sfs, 
                         elev, dist_slp, slope, rugg, closed, crops, urban, water, prot_area,
                         ttnoon, ttnoon_sq),
                resp = qq(risk)),
    mod6 = form(fixed = qq(1, phi, dist_col, dist_col_any, dist_sfs, 
                           elev, log_dist_slp, slope, rugg, closed, crops, urban, water, prot_area,
                           ttnoon, ttnoon_sq),
                intrc = NULL,
                rdm = qq(1, dist_col, dist_col_any, dist_sfs, 
                         elev, log_dist_slp, slope, rugg, closed, crops, urban, water, prot_area,
                         ttnoon, ttnoon_sq),
                resp = qq(risk))
)

# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- paste0("mod", i)
}

model <- models$mod4


# Standardize covariates --------------------------------------------------

# Extract variables needed for the model
vv <- all.vars(model)

# Define variables that can be standardized
vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean")

# Standardize covariates
vults <- vults %>%
    mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))


# Fit model ---------------------------------------------------------------

# Subset?
vults <- vults %>% 
    filter(bird_id %in% sample(unique(vults$bird_id), 10))

height_model <- glmmTMB(model, family = binomial, data = vults, doFit = FALSE)

# Define starting values
ini_val <- list(beta = height_model$parameters$beta,
                theta = height_model$parameters$theta)


# remove names just in case
ini_val <- lapply(ini_val, unname)

# Fit
height_fit <- glmmTMB(model, family = binomial, data = vults,
                         start = ini_val)


# Save model fit
saveRDS(height_fit, "output/height_fit.rds")

height_fit_summ <- summary(height_fit)

saveRDS(height_fit_summ, "output/hgt_fit_summ.rds")


# Explore results ---------------------------------------------------------

height_fit <- readRDS("output/height_fit.rds")

x <- -7:7

y <- -0.20*x + 0.04*x^2

plot(x, y)
