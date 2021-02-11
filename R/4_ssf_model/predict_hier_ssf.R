# 3-11-2020

# In this script we estimate resource selection from a step-selection model

# We predict from used vs. available from GPS tracks

rm(list = ls())

library(tidyverse)
library(raster)
library(sf)
library(glmmTMB)

# Load model fit
ssf_fit_rm <- read_rds("output/ssf_fit_rm.rds")

# summary(ssf_fit_rm)

# Extract bird codes
fit_bird_id <- unique(ssf_fit_rm$frame$bird_id)


# Load data ---------------------------------------------------------------

# Vulture data
vults <- read_rds("data/working/data_ssf_ready.rds")

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv")



# Prepare variables -------------------------------------------------------

# Subset individuals?
set.seed(12335)
vults <- vults %>% 
      filter(!bird_id %in% fit_bird_id) %>% 
      filter(bird_id %in% sample(unique(vults$bird_id), 5))

# Create a unique id for each data point and make step codes unique
vults_mod <- vults %>% 
      mutate(id = row_number(),
             step_id_ = paste(bird_id, step_id_, sep = "_"))

# Remove birds for which age in unknown
vults_mod <- vults_mod %>% 
      filter(!is.na(age))

# Change habitat codes
vults_mod <- vults_mod %>% 
      left_join(dplyr::select(hab_meta, "Map code", "class_code"), by = c("land_use" = "Map code")) %>% 
      rename(land_cov = class_code)


# Make dummy variables from factors (I also keep the factors)
vults_mod <- vults_mod %>%  
   mutate(i = 1,
          land_cov_fct = land_cov) %>% 
   spread(land_cov, i, fill = 0) %>% 
   mutate(i = 1,
          zone = factor(zone, levels = 1:4, labels = paste("z", 1:4, sep = "_")),
          zone_fct = zone) %>% 
   spread(zone, i, fill = 0) %>% 
   mutate(i = 1,
          res = factor(res, levels = c(1, 8, 24), labels = paste("res", c(1, 8, 24), sep = "_")),
          res_fct = res) %>% 
   spread(res, i, fill = 0) %>% 
   mutate(i = 1,
          age = factor(age, levels = c("juv", "subad", "ad")),
          age_fct = age) %>% 
   spread(age, i, fill = 0)

vults_mod <- vults_mod %>% 
   rename(srtm = srtm0)

# vults_mod$subad <- 0
# vults_mod$z_3 <- 0
# vults_mod$z_4 <- 0

# Standardize covariates --------------------------------------------------

# Extract model covariates
keep_vars <- names(ssf_fit_rm$frame)

# Extract scaling factors
cnt <- lapply(ssf_fit_rm$frame, attr, "scaled:center")
sds <- lapply(ssf_fit_rm$frame, attr, "scaled:scale")

cnt <- cnt[!sapply(cnt, is.null)]
sds <- sds[!sapply(sds, is.null)]

# Scale new data frame
sc_vars <- vults_mod %>% 
      dplyr::select(names(cnt))

sc_vars <- sweep(sc_vars, 2, unlist(cnt), "-")
sc_vars <- sweep(sc_vars, 2, unlist(sds), "/")

vults_mod <- cbind(vults_mod %>% 
                         mutate(case_num = if_else(case_ == TRUE, 1, 0)) %>% 
                         dplyr::select(keep_vars) %>% 
                         dplyr::select(-c(names(cnt))),
                   sc_vars)


# Predict from model ------------------------------------------------------

# pred_in <- predict(ssf_fit_rm)
pred_out <- predict(ssf_fit_rm, newdata = vults_mod, se.fit = F, re.form = NA, allow.new.levels = T)

pred_df <- vults_mod %>% 
      dplyr::select(bird_id, case_num) %>% 
      mutate(pred = exp(pred_out))

sel_df <- pred_df %>% 
      group_by(bird_id, case_num) %>% 
      summarize(tot_sel = sum(pred))

sel_df <- sel_df %>% 
      spread(case_num, tot_sel) %>% 
      rename(abs = 2,
             pre = 3) %>% 
      mutate(total = abs + pre,
             abs = abs / total,
             pre = pre / total)

mod_pred <- read_rds("data/working/model_pred.rds")

mod_pred <- c(mod_pred, list(mod25_habclass = sel_df))

write_rds(mod_pred, "data/working/model_pred.rds")
