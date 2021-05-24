library(tidyverse)
library(splines)
library(glmmTMB)

rm(list = ls())



# Explore data ------------------------------------------------------------

vults <- readRDS("data/working/data_ssf_ready.rds")

explVars <- function(data, .var, ...){
      groups_ <- enquos(...)
      new_df <- data %>% 
            mutate(var_cut = cut(data[,.var, drop = T], 
                                  breaks = quantile(data[,.var, drop = T], seq(0, 1, length.out=10)),
                                  include.lowest = T)) %>%
            group_by(!!!groups_) %>% 
            group_by(var_cut, .add = TRUE) %>% 
            summarize(n = n())
      
      return(new_df)
            
}

dists <- explVars(data = vults,
                   .var = "dist_col", case_, age_fct)

dists %>% 
      filter(case_ == TRUE) %>% 
      ggplot() +
      geom_col(aes(x = var_cut, y = n)) +
      facet_grid(~age_fct)


dists <- explVars(data = vults,
                  .var = "dist_col", case_, age_fct, ttnoon)

dists %>% 
      filter(case_ == TRUE) %>% 
      ggplot() +
      geom_col(aes(x = var_cut, y = n, fill = ttnoon)) +
      facet_grid(ttnoon~age_fct)


dists <- explVars(data = vults,
                  .var = "dist_col_any", case_, age_fct)

dists %>% 
   filter(case_ == TRUE) %>% 
   ggplot() +
   geom_col(aes(x = var_cut, y = n)) +
   facet_grid(~age_fct)


dists <- explVars(data = vults,
                  .var = "dist_col_any", case_, age_fct, ttnoon)

dists %>% 
   filter(case_ == TRUE) %>% 
   ggplot() +
   geom_col(aes(x = var_cut, y = n, fill = ttnoon)) +
   facet_grid(ttnoon~age_fct)

dists <- explVars(data = vults,
                  .var = "dist_sfs", case_, age_fct, ttnoon)

dists %>% 
   filter(case_ == TRUE) %>% 
   ggplot() +
   geom_col(aes(x = var_cut, y = n, fill = ttnoon)) +
   facet_grid(ttnoon~age_fct)

slopes <- explVars(data = vults %>% filter(!is.na(slope)),
                  .var = "slope", case_, age_fct)

slopes %>% 
      filter(case_ == TRUE) %>% 
      ggplot() +
      geom_col(aes(x = var_cut, y = n)) +
      facet_grid(~age_fct)

vults %>% 
      group_by(age_fct) %>% 
      add_count(name = "age_count") %>% 
      group_by(land_cov_fct, age_fct, age_count) %>% 
      summarize(n = n()) %>%
      mutate(prop_age = n / age_count) %>% 
      ggplot() +
      geom_col(aes(x = land_cov_fct, y = prop_age)) +
      facet_grid(~age_fct)


# Explore fit -------------------------------------------------------------

# Extract model elements
ssf_fit <- readRDS("hpc/output/ssf_fit_rm.rds")

fixed <- fixef(ssf_fit)$cond
frame <- ssf_fit$frame

fixed_dist <- fixed[str_detect(names(fixed), "ns\\(dist_col")]

dist_spl <- frame$`ns(dist_col, 5)`

rm(ssf_fit)

# Extract scaling factors
vults <- readRDS("data/working/data_ssf_ready.rds")

dist_sc <- sd(vults$dist_col)

summary(vults$dist_col / dist_sc)

rm(vults)

# Create new variable
newdist <- seq(0, 7, 0.1)

# Create basis for new variable
newdist_spl <- predict(dist_spl, newdist)

# Create new data frame
newdata <- as.data.frame(newdist_spl)
names(newdata) <- names(fixed)[1:5]
   
ttnoon <- -7
age <- "ad"

newdata <- newdata %>% 
   mutate(across(everything(), .fns = ~.x * (age == "juv"), .names = "{.col}:juv"),
          across(everything(), .fns = ~.x * ttnoon, .names = "{.col}:ttnoon"))


# Predict
pred <- exp(as.matrix(newdata) %*% fixed_dist)

plot(newdist, pred)
