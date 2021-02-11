# This script just transfers distance to slopes from one data set to 
# another, because computing dist to slp is very costly.

# WE CAN'T ACTUALLY DO THIS BECAUSE THE RANDOM POINTS ARE RANDOM AND THEREFORE DIFFERENT
# FOR EACH DATA SET!!!

rm(list = ls())

library(tidyverse)

d1 <- readRDS("data/working/data_ssf_ready.rds")
d2 <- readRDS("data/working/data_ssf_ready_dist_slp.rds")

# only if this is true we can proceed
all.equal(dplyr::select(d1, t1_, t2_, x1_, x2_, y1_, y2_),
          dplyr::select(d2, t1_, t2_, x1_, x2_, y1_, y2_))

# Transfer distances
d1$dist_slp <- d2$dist_slp

# Save new data
saveRDS(d1, "data/working/data_ssf_ready_dist_slp.rds")