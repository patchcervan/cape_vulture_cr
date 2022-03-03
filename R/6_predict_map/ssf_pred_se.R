# In this script we estimate the uncertainty in step-selection coefficients

library(vultRmap)
library(dplyr)
library(ggplot2)

# Set future maxsize to 650MB
options(future.globals.maxSize = 850*1024^2)

# Define minimum size of colony (number of adults) and
# roosts (total number of birds)
min_size_col <- 1
min_size_roost <- 50

# We will need to calculate distance to other colonies and restaurants
col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")
sfs <- utils::read.csv("../vultRmap_data_aux/sup_feeding_data.csv")

# For debugging
# col_all <- utils::read.csv("../vultRmap_data_aux/colony_data.csv")

# Subset colonies to we have counts for
col_to_pred <- col_all %>%
   dplyr::filter(!is.na(avg_ad)) %>%
   dplyr::filter((type == "breed" & avg_ad >= min_size_col) |
                    (type == "roost" & (avg_ad + avg_juv) >= min_size_roost))

# In case we want to exclude some colonies
# col_to_pred <- col_to_pred[-(1:3),]

# Sample random coefficients if necessary
ssf_coef <- vultRmap::sampleSsfCoef(nind = 1)[[1]]

# Define ages
ages <- c("ad", "juv")

# Define delta method function
Rcpp::sourceCpp("R/functions/eigenMatMult.cpp")

deltam <- function(X, Q){
library(Rcpp)
sourceCpp("R/functions/eigenMatMult.cpp")
   M <- eigenMapMatMult(X, Q)
   M2 <- eigenMapMatMult(M, t(X))
   out <- diag(M2)
   gc()
   return(out)
}

# Loop across colonies 

for(i in seq_len(nrow(col_to_pred))){
   
   col_sel <- col_to_pred[i,]
   
   # Loop across ages
   
   for(j in seq_along(ages)){
      
      age <- ages[j]
      
      # Prepare habitat
      hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon","lat")]), max_range = 1200,
                                  col_all = col_all, sfs = sfs, scale = "ssf")
      
      hab <- vultRmap::completeDataFrame(hab, names(ssf_coef), 0)
      
      # Correct age related predictors
      hab <- hab %>%
         dplyr::mutate(juv = 1 * (age == "juv"))
      
      # Predict from habitat
      SE <- vector("list", 15)
      
      if(nrow(hab) < 1.8e6){
         ws <- 10
      } else {
         ws <- 10 - ceiling((nrow(hab) - 1.8e6)/250e3)
      }
      
      # future::plan("multisession", workers = ws)
      
      for(t in 1:15){
         ttnoon <- -8 + t
         
         predt <- hab %>%
            dplyr::mutate(dplyr::across(.cols = dplyr::ends_with("ttnoon"), ~.x*ttnoon),
                          dplyr::across(.cols = dplyr::ends_with("ttnoon_sq"), ~.x*ttnoon^2)) %>%
            dplyr::select(dplyr::all_of(names(ssf_coef)))
         
         n <- 50
         nr <- nrow(predt)
         mm <- split(predt, rep(1:n, each = ceiling(nr/n), length.out = nr))
         mm <- lapply(mm, as.matrix)
         
         SE[[t]] <- furrr::future_map(mm, ~deltam(.x, ssf_fit_summary$vcov$cond))
         SE[[t]] <- do.call("c", SE[[t]])
         
      }
      
      # future::plan("sequential")
      
      # Take daily mean
      SE_mean <- sqrt(Reduce("+", SE)/15)
      
      saveRDS(SE_mean, file = paste0("output/col_ssf_se/", col_sel$id, "_", age, "_ssf_se.rds"))
      
      gc()
      
   }
}



# Explore results ---------------------------------------------------------

ages <- c("ad", "juv")

se_total <- rep(0, length = nrow(vultRmap::range_covts))
ncols <- rep(0, length = nrow(vultRmap::range_covts))

for(i in seq_len(nrow(col_to_pred))){
   # i = 1
   col_sel <- col_to_pred[i,]
   
   for(j in seq_along(ages)){
      age <- ages[j]
      
      se <- readRDS(paste0("hpc/output/col_ssf_se/", col_sel$id, "_", age, "_ssf_se.rds"))
      
      hab <- vultRmap::prepColHab(col_cc = unlist(col_sel[, c("lon","lat")]), max_range = 1200,
                                  col_all = col_all, sfs = sfs, scale = "ssf")
      
      se_df <- data.frame(lon = hab$lon,
                          lat = hab$lat,
                          dist_col = hab$dist_col,
                          se = se)
      
      se_df <- se_df %>%
         dplyr::mutate(lon = round(lon, 3),
                       lat = round(lat, 3))
      
      col_se <- calcSEColony(.col_sel = col_sel, .se_df = se_df, .age = age)
      
      se_total <- se_total + col_se$se^2
      ncols <- ncols + col_se$ncol
      
   }
   
}


col_se %>% 
   # mutate(se = if_else(se == 0, quantile(se, 0.95), se)) %>% 
   ggplot() +
   geom_raster(aes(x = lon, y = lat, fill = se)) +
   scale_fill_viridis_c()

data.frame(lon = vultRmap::range_covts$lon,
           lat = vultRmap::range_covts$lat,
           se_mean = sqrt(se_mean)) %>% 
   ggplot() +
   geom_raster(aes(x = lon, y = lat, fill = se_mean)) +
   scale_fill_viridis_c()

col_se %>% 
   filter(!is.na(dist_col)) %>% 
   ggplot() +
   geom_point(aes(x = dist_col, y = se))

library(mgcv)

col_se <- col_se %>% 
   mutate(ang = calcAng(lat - col_sel$lat, lon - col_sel$lon),
          dist = sqrt((lat - col_sel$lat)^2 + (lon - col_sel$lon)^2))

gam_mod <- gam(se ~ te(dist, ang, bs = c("cr", "cc"), k = c(3, 10)), data = col_se)

pred_se <- predict(gam_mod,
                   col_se %>% 
                      filter(is.na(dist_col)))

pred_se <- predict(gam_mod,
                   col_se %>% 
                      filter(is.na(dist_col)) %>%
                      dplyr::select(-dist_col) %>% 
                      rename(dist_col = dist))

pred_se <- col_se %>% 
   filter(is.na(dist_col)) %>% 
   mutate(pred_se = pred_se) %>% 
   dplyr::select(lon, lat, pred_se)

col_se %>% 
   left_join(pred_se, by = c("lon", "lat")) %>% 
   mutate(se_pred = if_else(is.na(dist_col), pred_se, se)) %>% 
   ggplot() +
   geom_raster(aes(x = lon, y = lat, fill = se_pred)) +
   scale_fill_viridis_c()

col_se %>% 
   left_join(pred_se, by = c("lon", "lat")) %>% 
   mutate(se_pred = if_else(is.na(dist_col), pred_se, se)) %>% 
   slice_sample(n=1000) %>% 
   ggplot() +
   geom_point(aes(x = dist, y = dist_col))

col_se %>% 
   mutate(se_pred = gam_mod$fitted.values) %>% 
   ggplot() +
   geom_raster(aes(x = lon, y = lat, fill = se_pred)) +
   scale_fill_viridis_c()

col_se %>% 
   mutate(se_pred = gam_mod$fitted.values) %>% 
   filter(!is.na(dist_col)) %>% 
   ggplot() +
   geom_point(aes(x = dist_col, y = se_pred))
