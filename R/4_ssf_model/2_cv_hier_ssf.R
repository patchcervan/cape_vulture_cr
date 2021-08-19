# 18-10-2020

# In this script we fit a hierarchical SSF model to the vulture data set

# THIS SCRIPT USES A LOT OF RAM, SO YOU MIGHT WANT TO CONSIDER RUNNING ON 
# HIGH PERFORMANCE CLUSTER (RSTUDIO WILL CRASH WITH 32GB)

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
vults <- readRDS("data/working/data_ssf_ready_5pp.rds")

# Create a unique id for each data point and make step codes unique
vults <- vults %>%
    mutate(id = row_number(),
           step_id_ = paste(bird_id, step_id_, sep = "_"))

# Change variable names to make them more readable
# vults <- vults %>%
#     rename(slope = res01_slope)
#            rugg = vrm3,
#            dist_slp = dist_slp_m)

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
#set.seed(693875)
set.seed(34562)

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

# Helper function to formulate models from elements
form <- function(fxd, intn, rdm, data){
    # Fixed effects
    fxds <- unlist(fxd)
    
    # Interactions
    intns <- vector("list", length = length(intn))
    
    for(i in seq_along(intn)){
        if(any(str_detect(unlist(intn[[i]][[2]]), "\\("))){
            main <- unlist(intn[[i]][[1]])
            ref <- str_extract(intn[[i]][[2]], "(?<=\\().*?(?=\\))")
            intvars <- str_remove(intn[[i]][[2]], "\\(.*\\)")
            intvars <- levels(data[,intvars, drop = T])
            intvars <- intvars[intvars != ref]
            intns[[i]] <- apply(arrange(expand.grid(main, intvars), Var1), 1, paste, collapse = ":")
            
        } else {
            intns[[i]] <- paste(unlist(intn[[i]][1]), unlist(intn[[i]][-1]), sep = ":")
        } 
    }
    
    # Random effects
    if(identical(rdm, "all")){
        rdms <- paste0("(0+", c(fxds, unlist(intns)), "|bird_id)")
    } else {
        rdms <- vector("list", length = length(rdm))
        for(i in seq_along(rdms)){
            tt <- do.call(paste, args = c(rdm[[i]], sep = "|"))
            rdms[[i]] <- paste0("(0+", tt, ")")
        }
    }
    
    reformulate(c(-1, fxds, unlist(intns), unlist(rdms), "(1|step_id_)"), response = "case")
}

# Define model elements
mod_elem <- list(
    dist = c("dist_col", "dist_sfs", "dist_col_any"),
    dist2 = c("log_dist_col", "dist_sfs", "dist_col_any"),
    log_dist = c("log_dist_col", "log_dist_sfs", "log_dist_col_any"),
    dist3 = c("dist_col", "dist_sfs", "dist_col_any", "log_dist_col"),
    topo1 = c("elev", "slope", "dist_slp", "rugg"),
    topo2 = c("elev", "slope", "rugg"),
    topo3 = c("elev", "dist_slp", "rugg"),
    hab = c("closed", "crops", "urban", "water", "prot_area"),
    hab2 = c("NDVI_mean", "prot_area"),
    mov = c("sl_")
)

# Define models
models <- list(
    mod1 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod2 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod3 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("dist_col_any", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("dist_sfs", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod4 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id")),
                           list(c("dist_col:ttnoon", "dist_col:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod5 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")), 
                            list(mod_elem["topo2"], "zone_fct(z_1)"),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod6 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("log_dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id")),
                           list(c("dist_col:ttnoon", "dist_col:ttnoon_sq"), c("bird_id")),
                           list(c("log_dist_col:ttnoon", "log_dist_col:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod7 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")], "log_sl"),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("res")),
                            list("log_sl", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("log_sl:ttnoon", "log_sl:ttnoon_sq"), c("bird_id")),
                           list(c("dist_col:ttnoon", "dist_col:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod8 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res", "juv"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id")),
                           list(c("dist_col:ttnoon", "dist_col:ttnoon_sq"), c("bird_id"))),
                data = vults),
    mod9 = form(fxd = c("log_dist_col", mod_elem[c("dist", "topo2", "hab", "mov")]),
                intn = list(list(c("log_dist_col", mod_elem["dist"]), c("juv")),
                            list(c("log_dist_col", mod_elem["dist"]), c("res")),
                            list(c(mod_elem["topo2"]), c("res")),
                            list("dist_col", c("ttnoon", "ttnoon_sq", "ttnoon:juv", "ttnoon_sq:juv")),
                            list("sl_", c("ttnoon", "ttnoon_sq", "res"))),
                rdm = list(list(unlist(c("log_dist_col", mod_elem[c("dist", "topo2", "mov")])), c("bird_id")),
                           list(c("sl_:ttnoon", "sl_:ttnoon_sq"), c("bird_id")),
                           list(c("dist_col:ttnoon", "dist_col:ttnoon_sq"), c("bird_id"))),
                data = vults)
)

# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- paste0("mod", i)
}


# FIT MODELS --------------------------------------------------------------

# At this point we can get rid of most of the working environment to free up memory
rm(list = ls()[!ls() %in% c("cv_ids", "models")])
gc()

# Load function to fit and predict
source("R/functions/fitCVssf.R")

for(m in seq_along(models)){
    
    # Seeds
    # 84546
    # 5412
    
    cv_results <- future_map(cv_ids, ~fitCVssf(train_data = "data/working/data_ssf_ready_5pp.rds",
                                               test_data = "data/working/data_ssf_ready_5pp.rds",
                                               test_ids = .x, model = models[[m]], seed = 5412,
                                               save_fit = FALSE, 
                                               output_file = paste0("output/cv_results/ssf_cv/cv_coef_mod_", attr(models[[m]], "model"), "_", attr(.x, "id"), ".rds")),
                             .options = future_options(seed = 5412))

cv_results <- do.call("rbind", cv_results)
    
    # Save cross-validation results
    saveRDS(cv_results, paste0("output/cv_results_", m, ".rds"))
}


# Explore results ---------------------------------------------------------

resultsdir <- "hpc/output/"

# Load from cluster
files <- list.files(resultsdir, pattern = "cv_results_[1-9]t1.rds")

cv_results <- data.frame()

for(i in seq_along(files)){
    cv_results <- rbind(cv_results,
                        readRDS(paste0(resultsdir, files[i])))
}

# Load the other batch
resultsdir <- "hpc/output/"
files <- list.files(resultsdir, pattern = "cv_results_[1-6]t2.rds")

for(i in seq_along(files)){
    cv_results <- rbind(cv_results,
                        readRDS(paste0(resultsdir, files[i])))
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
    filter(!is.na(AIC)) %>% 
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


# Explore single fit ------------------------------------------------------

# Read in cv results
cv_results <- readRDS("hpc/output/cv_results_mod_2_test_1.rds")
cv_results

# Load fit
ssf_fit <- readRDS("hpc/output/ssf_fit_test1_mod2.rds")

summary(ssf_fit)
