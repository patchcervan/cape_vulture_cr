library(tidyverse)
library(mgcv)
library(furrr)

rm(list = ls())

rasterdir <- "output/pred_raster_sims/"


# Load data ---------------------------------------------------------------

colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Subset those colonies and roosts we have data for.
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))

#col_to_pred <- col_to_pred  %>%
#slice(90:nrow(col_to_pred))


# Define age --------------------------------------------------------------

age <- "ad"


# Create function to fit GAM in parallel ----------------------------------

col_sel_id <- col_to_pred$id[1]

f <- function(col_sel_id, col_to_pred){
   
   col_sel <- col_to_pred %>% 
      filter(id == col_sel_id)
   
   # Load sims
   # sims <- rbind(readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k1.rds")),
   #               readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k2.rds")))
   
   sims <- readRDS(file = paste0(rasterdir, "hgt_pred/hgt_risk_", col_sel$id, "_", age, ".rds"))
   
   # Load habitat data
   hab <- readRDS(paste0(rasterdir, "df_", col_sel_id, ".rds"))
   
   # Count the number of visits to each cell
   counts <- sims %>% 
      mutate(lon = round(lon, 3),
             lat = round(lat, 3)) %>% 
      group_by(lon, lat) %>% 
      summarize(count = sum(hgt_risk))
   
   # Associate counts with covariates
   hab <- hab %>% 
      mutate(lon = round(x, 3), 
             lat = round(y, 3)) %>% 
      left_join(counts %>% 
                   dplyr::select(lon, lat, count), by = c("lon", "lat"))
   
   hab <- hab %>% 
      mutate(count = if_else(is.na(count), 0, count))
   
   hab %>% 
      filter(count < 10) %>% 
      pull(count) %>% 
      hist()
   
   hab$countp <- hab$count + 0.1
   
   # Fit GAM
   fit <- gam(count ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                 closed+ crops+ urban+ water+ prot_area, family = poisson(link = "log"), data = hab)
   fit <- gam(count ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                 closed+ crops+ urban+ water+ prot_area, family = quasipoisson(link = "log"), data = hab)
   
   fit <- gam(count ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                 closed+ crops+ urban+ water+ prot_area, family = nb, data = hab)
   fit <- gam(list(countp ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                 closed+ crops+ urban+ water+ prot_area, ~1), family = gammals, data = hab)
   fit <- gam(count ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                 closed+ crops+ urban+ water+ prot_area, family = gaussian(link = "log"), data = hab)
   
   require(parallel)  
   nc <- 4   ## cluster size, set for example portability
   if (detectCores()>1) { ## no point otherwise
      cl <- makeCluster(nc) 
      ## could also use makeForkCluster, but read warnings first!
   } else cl <- NULL
   
   system.time(fit <- bam(count ~ s(lon,lat)+ s(dist_col)+ dist_sfs+ dist_col_any+ elev+ slope+ rugg+
                            closed+ crops+ urban+ water+ prot_area, family = quasipoisson(link = "log"), data = hab, cluster=cl))
   
   fv <- predict(b3,cluster=cl) ## parallel prediction
   
   poispred <- fit$fitted.values
   nbpred <- fit$fitted.values
   gammpred <- fit$fitted.values[,1]
   gauspred <- fit$fitted.values
   quaspoispred <- fit$fitted.values
   
   hab$pred <- quaspoispred
   
   plot(hab$count[hab$count<2000], hab$pred[hab$count<2000], asp = 1, cex = 0.5)
   points(hab$count[hab$count<2000], hab$pred[hab$count<2000], asp = 1, cex = 0.5, col = "red")
   abline(a=0, b=1)
   
   # summary(fit)
   
   diag_deviance <- 1 - fit$deviance/fit$null.deviance
   
   hab$gamfit <- fit$fitted.values
   
   hab %>% 
      dplyr::select(lon, lat, count, gamfit) %>%
      saveRDS(file = paste0(rasterdir, "col_gam/gam_hgt_", col_sel$id, "_", age, ".rds"))
   
   return(diag_deviance)
   
}

future::plan("multisession", workers = 2)

expl_dev <- future_map_dbl(col_to_pred$id, ~f(.x, col_to_pred))

future::plan("sequential")

saveRDS(expl_dev, paste0("output/expl_dev_gam_hgt", age, ".rds"))
