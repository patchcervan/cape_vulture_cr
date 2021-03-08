# 18-10-2020

# In this script we fit a hierarchical binomial model to the height data of the 
# vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- readRDS("data/working/data_height_ready.rds")

# Remove records with no height
vults <- vults %>% 
    filter(!is.na(height))


# Prepare variables -------------------------------------------------------

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
    time = c("ttnoon", "ttnoon_sq"),
    risk_t1 = c("risk_t1")
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
    rdm <- c(paste("(0 +", elem,"|bird_id)"))
    
    # Write formula
    reformulate(c(1, fixed, rdm), "risk")
}

# Define models
models <- list(
    mod1 = form(vults, mod_elem, c("dist", "topo1", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo1 ~ zone_fct(z_1))),
    mod2 = form(vults, mod_elem, c("dist", "topo2", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo2 ~ zone_fct(z_1))),
    mod3 = form(vults, mod_elem, c("dist", "topo3", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo3 ~ zone_fct(z_1))),
    mod4 = form(vults, mod_elem, c("dist", "topo4", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo4 ~ zone_fct(z_1))),
    mod5 = form(vults, mod_elem, c("dist", "topo4", "hab2", "risk_t1"), intr = list(risk_t1 ~ res, topo4 ~ zone_fct(z_1))),
    mod6 = form(vults, mod_elem, c("dist2", "topo4", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo4 ~ zone_fct(z_1))),
    mod7 = form(vults, mod_elem, c("log_dist", "topo4", "hab", "risk_t1"), intr = list(risk_t1 ~ res, topo4 ~ zone_fct(z_1))),
    mod8 = form(vults, mod_elem, c("log_dist", "topo4", "risk_t1"), intr = list(risk_t1 ~ res, topo4 ~ zone_fct(z_1))),
    mod9 = form(vults, mod_elem, c("log_dist", "topo4", "risk_t1"), intr = list(risk_t1 ~ res)),
    mod10 = form(vults, mod_elem, c("dist2", "topo4", "time", "risk_t1"), intr = list(risk_t1 ~ res))
)

# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- paste0("mod", i)
}

vults <- vults %>% 
    mutate(day = lubridate::date(datetime)) %>% 
    group_by(bird_id, day) %>% 
    mutate(times = as.numeric(round(difftime(datetime, lag(datetime), units = "hours"), 1)),
           times = if_else(is.na(times), 0, times),
           times = cumsum(times)) %>% 
    # filter(times > 24) %>% 
    # dplyr::select(datetime, day, times)
    ungroup() %>% 
    mutate(times = glmmTMB::numFactor(times))

model <- formula(risk ~ 1 + log_dist_col + log_dist_sfs + log_dist_col_any + elev + 
                     slope + log_dist_slp + rugg + closed + crops + urban + water + 
                     prot_area + elev:z_3 + elev:z_2 + elev:z_4 + slope:z_3 + 
                     slope:z_2 + slope:z_4 + log_dist_slp:z_3 + log_dist_slp:z_2 + 
                     log_dist_slp:z_4 + rugg:z_3 + rugg:z_2 + rugg:z_4 + 
                     (0 + log_dist_col | bird_id) + (0 + log_dist_sfs | bird_id) + 
                     (0 + log_dist_col_any | bird_id) + (0 + elev | bird_id) + 
                     (0 + slope | bird_id) + (0 + log_dist_slp | bird_id) + 
                     (0 + rugg | bird_id) + (0 + closed | bird_id) + (0 + crops | bird_id) + 
                     (0 + urban | bird_id) + (0 + water | bird_id) + (0 + prot_area | bird_id) +
                     ou(0 + times | bird_id/day))



# Standardize covariates --------------------------------------------------

# Extract variables needed for the model
vv <- all.vars(model)

# Define variables that can be standardized
vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean")

# Standardize covariates
vults <- vults %>%
    mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))


# Fit model ---------------------------------------------------------------

# model <- models$mod7

height_model <- glmmTMB(model, family = binomial, data = vults, doFit = FALSE)

# Define starting values
ini_val <- list(beta = height_model$parameters$beta,
                theta = height_model$parameters$theta)


# remove names just in case
ini_val <- lapply(ini_val, unname)

# Fit
height_fit_rm <- glmmTMB(model, family = binomial, data = vults,
                         start = ini_val)


# Save model fit
saveRDS(height_fit_rm, "output/height_fit_rm.rds")


# Explore results ---------------------------------------------------------

height_fit_rm <- read_rds("output/height_fit_rm.rds")

summary(height_fit_rm)

x <- -7:7

y <- -0.20*x + 0.04*x^2

plot(x, y)
