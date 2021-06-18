# 18-06-2021

# In this script we compare SSF coefficients for different number of available points

rm(list = ls())

library(tidyverse)
library(glmmTMB)


# Load data ---------------------------------------------------------------

summ <- list(p5 = summ5pp <- vultRmap::ssf_fit_summary,
             p7 = readRDS("hpc/output/ssf_fit_summ_7pp.rds"),
             p10 = readRDS("hpc/output/ssf_fit_summ_10pp.rds"))

do.call("rbind", 
        lapply(summ, function(x) x$coefficients$cond[,"Estimate"]))

do.call("rbind", 
        lapply(summ, function(x) x$coefficients$cond[,"Std. Error"]))
