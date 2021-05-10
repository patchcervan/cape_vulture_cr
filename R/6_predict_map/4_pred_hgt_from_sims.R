library(tidyverse)
library(glmmTMB)
library(furrr)

rm(list = ls())

rasterdir <- "output/pred_raster_sims/"


# Load data ---------------------------------------------------------------

# Load model fit
hgt_fit <- readRDS("output/height_fit_rm.rds")

# Load colony data
colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Subset those colonies and roosts we have data for
col_to_pred <- colony_all %>% 
      filter(!is.na(avg_ad)) %>%
      filter((type == "breed" & avg_ad > 9) | (type == "roost" & (avg_ad + avg_juv) > 50))

#col_to_pred <- col_to_pred  %>%
#slice(90:nrow(col_to_pred))


# Define age --------------------------------------------------------------

age <- "ad"


# Extract variables from model --------------------------------------------

# Extract model coefficients
hgt_coefs <- fixef(hgt_fit)$cond


# Create function to predict in parallel ----------------------------------

f <- function(col_sel_id, .col_to_pred){
      
      col_sel <- .col_to_pred %>% 
            filter(id == col_sel_id)
      
      # Load sims
      sims <- rbind(readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k1.rds")),
                    readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims50k2.rds")))
      
      # sims <- readRDS(paste0(rasterdir, "1_pred_map_col/", col_sel_id, "_", age, "_sims100k.rds"))
      
      # Load habitat data
      hab <- readRDS(paste0(rasterdir, "df_", col_sel_id, ".rds"))
      
      ## FIX COLONY COORDINATES
      sims <- sims %>% 
            mutate(lon = round(lon, 3),
                   lat = round(lat, 3)) %>% 
            left_join(hab %>% 
                            dplyr::select(x, y, dist_col) %>% 
                            mutate(lon = round(x, 3), 
                                   lat = round(y, 3)),
                      by = c("lon", "lat"))
      
      mincc <- sims %>% 
            filter(dist_col == min(dist_col, na.rm = T)) %>% 
            dplyr::select(lon, lat) %>% 
            distinct() %>% 
            unlist()
      
      sims <- sims %>% 
            mutate(lon = if_else(is.na(dist_col), mincc[1], lon),
                   lat = if_else(is.na(dist_col), mincc[2], lat)) %>% 
            dplyr::select(lon, lat, ttnoon, trip, dur)
      
      ##
      
      # Associate sims with covariates
      sims <- sims %>% 
            mutate(lon = round(lon, 3),
                   lat = round(lat, 3)) %>% 
            left_join(hab %>% 
                            mutate(lon = round(x, 3), 
                                   lat = round(y, 3)),
                      by = c("lon", "lat"))
      
      # Save coordinates for later
      cc <- sims %>% 
         dplyr::select(lon, lat)
      
      # Create data frame for prediction
      sims <- sims %>% 
            mutate(`(Intercept)` = 1,
                   phi = exp(-1),
                   ttnoon_sq = ttnoon^2) %>% 
            dplyr::select(all_of(names(hgt_coefs))) %>% 
            as.matrix()
      
      # Coefficients will change at each step because it is an autoregressive process
      # At each step we need to add the conditional probabilities that the bird
      # was at risk and that was not at risk in the previous step
      stepcoef1 <- stepcoef2 <- hgt_coefs
      stepcoef2[2] <- hgt_coefs[2]*-1
      
      pred <- vector(length = nrow(sims))
      for(i in 1:nrow(sims)){
      # for(i in 1:15){
            if(sims[i, "ttnoon"] == -7){
                  stepcoef1[2] <- 0 
                  stepcoef2[2] <- 0
                  p_t1 <- 0.5
            }
            
            lp1 <- sims[i,] %*% stepcoef1
            lp2 <- sims[i,] %*% stepcoef2
            
            p1 <- (exp(lp1) / (1 + exp(lp1))) * p_t1
            p2 <- (exp(lp2) / (1 + exp(lp2))) * (1-p_t1)
            
            pred[i] <- p_t1 <- p1+p2
            
            stepcoef1 <- stepcoef2 <- hgt_coefs
            stepcoef2[2] <- hgt_coefs[2]*-1
      }
      
      # Add coordinates and risk to the sims data frame and save
      sims %>% 
         as.data.frame() %>% 
         cbind(cc) %>% 
         mutate(hgt_risk = pred) %>% 
         saveRDS(file = paste0(rasterdir, "hgt_pred/hgt_risk_", col_sel$id, "_", age, ".rds"))
      
}

future::plan("multisession", workers = 8)

future_map(col_to_pred$id, ~f(.x, col_to_pred))

future::plan("sequential")
