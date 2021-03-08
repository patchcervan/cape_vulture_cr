# 18-10-2020

# In this script we fit a hierarchical binomial model to the height data of the 
# vulture data set

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


# Prepare train and validation --------------------------------------------

# Add group index
vults <- vults %>% 
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

cv_ids <- map(1:10, ~f(bird_ids))

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


# FIT MODELS --------------------------------------------------------------

# Load function to fit and predict
source("R/functions/fitCVheight.R")

for(m in seq_along(models)){
    
    cv_results <- future_map(cv_ids, ~fitCVheight(data_train = "data/working/data_height_ready.rds",
                                                  data_test = "data/working/data_height_test.rds",
                                                  test_ids = .x, model = models[[m]],
                                                  save_fit = FALSE, output_file = NULL, plotAUC = FALSE))
    
    cv_results <- do.call("rbind", cv_results)
    
    # Save cross-validation results
    saveRDS(cv_results, paste0("output/cv_results_height_", m, ".rds"))
}


# Explore results ---------------------------------------------------------

# Load from cluster
files <- list.files("hpc/output/", pattern = "cv_results_height")

cv_results <- data.frame()

for(i in seq_along(files)){
    cv_results <- rbind(cv_results,
                        readRDS(paste0("hpc/output/", files[i])))
}

print(cv_results, n = Inf)

cv_results %>% 
    filter(!is.na(AIC)) %>% 
    dplyr::select(-formula) %>% 
    gather(metric, value, -c("mod", "test")) %>% 
    ggplot() +
    geom_boxplot(aes(x = mod, y = value)) +
    geom_jitter(aes(x = mod, y = value), col = "red", alpha = 0.5, width = 0.1) +
    facet_wrap("metric", scales = "free")
# Warning messages are fine it is that the columns inherited attributes from previous
# objects

cv_results %>% 
    dplyr::select(-formula) %>% 
    gather(metric, value, -c("mod", "test")) %>% 
    ggplot() +
    geom_point(aes(x = mod, y = value, col = test), alpha = 0.5) +
    geom_line(aes(x = mod, y = value, col = test, group = test), alpha = 0.5) +
    facet_wrap("metric", scales = "free")
# Warning messages are fine it is that the columns inherited attributes from previous
# objects

# check with linear model
summary(lm(cv_sel_coef ~ mod, data = cv_results))