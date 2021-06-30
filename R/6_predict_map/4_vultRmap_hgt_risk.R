library(tidyverse)
library(vultRmap)

rm(list = ls())

# Simulate for one colony -------------------------------------------------

# Load colony data
col_all <- read.csv("../vultRmap_data_aux/colony_data.csv")

# Load supplementary feeding sites data
sfs <- read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

# Subset those colonies and roosts we have data for
col_to_pred <- col_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))


# Define age --------------------------------------------------------------

age <- "ad"


# Extract variables from model --------------------------------------------

# Extract model coefficients
hgt_coef <- sampleHgtCoef(hgt_fit_summary, 40, seed = 87634)

# Load general habitat
hab <- range_covts

hgt_risk <- estHgtRisk()