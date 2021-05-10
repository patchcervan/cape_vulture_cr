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
                data = vults)#,
    # mod5 = form(fxd = c("ns(dist_col, 5)", "dist_col_any", "dist_sfs", "ns(sl_, 5)", mod_elem[c("topo2", "hab")]),
    #             intn = list(list(c("ns(dist_col, 5)", "dist_col_any", "dist_sfs"), c("juv")),
    #                         list("ns(dist_col, 5)", c("ttnoon", "ttnoon:juv")),
    #                         list("ns(sl_, 5)", c("ttnoon", "res"))),
    #             rdm = list(list(unlist(c("ns(dist_col, 5)", "dist_col_any", "dist_sfs", mod_elem[c("topo2", "hab")])), c("bird_id")),
    #                        list(c("ns(sl_, 5):ttnoon"), c("bird_id"))),
    #             data = vults),
    # mod6 = form(fxd = c("ns(dist_col, 5)", "ns(dist_col_any, 5)", "ns(dist_sfs, 5)", "ns(sl_, 5)", mod_elem[c("topo2", "hab")]),
    #             intn = list(list(c("ns(dist_col, 5)", "ns(dist_col_any, 5)", "ns(dist_sfs, 5)"), c("juv")),
    #                         list("ns(sl_, 5)", c("ttnoon", "res"))),
    #             rdm = list(list(unlist(c("ns(dist_col, 5)", "ns(dist_col_any, 5)", "ns(dist_sfs, 5)", mod_elem[c("topo2", "hab")])), c("bird_id")),
    #                        list(c("ns(sl_, 5):ttnoon"), c("bird_id"))),
    #             data = vults)
)
# Set model names
for (i in seq_along(models)){
    attr(models[[i]], "model") <- paste0("mod", i)
}


# Select model to fit -----------------------------------------------------

model <- models$mod8


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

ssf_fit_rm <- readRDS("output/ssf_fit_rm.rds")
ssf_fit_summ <- summary(ssf_fit_rm)

saveRDS(ssf_fit_summ, "output/ssf_fit_summ_mod11.rds")

mod_sum <- readRDS("output/ssf_fit_summ_mod11.rds")
mod_sum

mod_sum <- c(mod_sum, mod25a_habclass = summary(ssf_fit_rm))

write_rds(mod_sum, "data/working/model_summ.rds")

VarCorr(ssf_fit_rm)

# Standard errors for fixed and random effects
ssf_fit_rm$sdr