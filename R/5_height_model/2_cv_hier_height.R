# 16-02-2021

# In this script we fit a hierarchical binomial model to the height data of the 
# vulture data set

rm(list = ls())

library(tidyverse)
library(glmmTMB)
# library(splines)
library(furrr)

# Set future maxsize to 850MB
options(future.globals.maxSize = 850*1024^2)

future::plan("multisession", workers = 2)


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
set.seed(693875)
#set.seed(34562)

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


# FIT MODELS --------------------------------------------------------------

# Load function to fit and predict
source("R/functions/fitCVheight.R")

for(m in 5:6){
    
    cv_results <- future_map(cv_ids, ~fitCVheight(data_train = "data/working/data_height_ready_noextrm.rds",
                                                  data_test = "data/working/data_height_ready_noextrm.rds",
                                                  test_ids = .x, model = models[[m]],
                                                  save_fit = FALSE, output_file = NULL, plotAUC = TRUE))
    
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