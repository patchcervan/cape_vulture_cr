library(tidyverse)
library(raster)
library(sf)
library(glmmTMB)


rm(list = ls())


# Define raster output directory
rasterdir <- "output/pred_raster_sims/"


# Load data and model results ---------------------------------------------

# Colony and roost data
colony_all <- read_csv("data/working/colony_all_w_da.csv")

# Load restaurant data
sfs <- read_csv("data/working/restaurants.csv")

# Habitat metadata
hab_meta <- read_csv("data/working/copernicus_codes.csv", col_types = cols())

# Load Rcpp distance calculation function to speed up computation
Rcpp::sourceCpp("R/functions/minDist_cpp.cpp")

# Model fit
ssf_fit <- readRDS("output/ssf_fit_dist_tnoon.rds")


# Define prediction variables --------------------------------------------

# Variables necessary for prediction
vv <- all.vars(ssf_fit$call$formula)

# Of these which ones we can extract from rasters (we need to use the name that
# appears in the stored files - elev = srtm0, rugg = vrm3, land_cov = LC100)
rcovts <- c("srtm0", "slope", "vrm3", "LC100_global_v3.0.1_2019")

# distances to focal sites
dcovts <- c("dist_col", "dist_sfs", "dist_col_any")

# extracted from shapefiles
scovts <- c("prot_area")

covts <- list(rcovts = rcovts, dcovts = dcovts, scovts = scovts)


# Prepare data and variables ----------------------------------------------

# Make colonies a spatial object
colony_all <- st_as_sf(colony_all, coords = c("lon", "lat"), crs = 4326, remove = F)

# Make sfs a spatial object
sfs <- st_as_sf(sfs, coords = c("longitude", "latitude"), crs = 4326, remove = F)


# Select target colonies ---------------------------------------------------

# Subset those colonies and roosts for which we have data.
# Further subset roosts with more than 50 birds
col_to_pred <- colony_all %>% 
   filter(!is.na(avg_ad)) %>%
   filter((type == "breed" & avg_ad > 0) | (type == "roost" & (avg_ad + avg_juv) > 50))

# col_to_pred %>% dplyr::select(-names_old) %>% print(n = Inf)


# Extract model elements ------------------------------------------------------

ssf_coefs <- fixef(ssf_fit)$cond
ssf_frame <- ssf_fit$frame

rm(ssf_fit)

# Extract variables needed for the model
vv <- names(ssf_coefs)

# Extract scaling factors
sds <- lapply(ssf_frame, attr, "scaled:scale")
sds <- sds[!sapply(sds, is.null)]


# Predict for the colonies ----------------------------------------------------

for(k in 1:nrow(col_to_pred)){
      # k=1
      
      col_sel <- col_to_pred[k,]
      
      # Create a ~220 km buffer
      colbuff <- st_buffer(col_sel, dist = 4)
      
      # plot(st_geometry(colbuff), axes = T, asp = 1)
      # plot(st_geometry(col_sel), add = T)
      
      # Find map codes ----------------------------------------------------------
      
      source("R/functions/findMapCodes.R")
      
      map_codes <- findMapCodes(colbuff)
      
      
      # Create basic covts. stack -----------------------------------------------
      
      source("R/functions/createTempCovtRaster.R")
      
      # Remove those maps that are already prepared and prepare the rest
      for(j in seq_along(map_codes)){
         
         try(
            createTempCovtRaster(map_code = map_codes[j], covts=covts$rcovts, output_res=0.01,
                                 covtsdir = "data/working/covts_rasters/topo_res01/res01_",
                                 outputdir = paste0(rasterdir, "temp_covts/"),
                                 overwrite = F)
         )
      }
      
      
      # Create a sample space around colony -------------------------------------
      
      # Load the multiple stacks corresponding to map_codes
      r <- map(map_codes, ~stack(paste0(rasterdir, "temp_covts/covts_", .x, ".gri")))
      
      # Create a list that will host the different raster layers. Save names for later
      rr <- vector("list", length = nlayers(r[[1]]))
      varnames <- names(r[[1]])
      
      # Iterate through raster layers and merge them across map codes
      if(length(r) > 1){
         for(i in 1:nlayers(r[[1]])){
            s <- lapply(r, "[[", i)
            s <- do.call(merge, s)
            s <- crop(s, colbuff)
            s <- mask(s, colbuff)
            rr[[i]] <- s
         }
      } else {
         rr <- r[[1]]
         rr <- crop(rr, colbuff)
         rr <- mask(rr, colbuff)
      }
      
      rm(r, s)
      
      # Stack the different raster layers and fix names
      rr <- stack(rr)
      names(rr) <- varnames
      
      # Detect ocean pixels
      v <- getValues(rr$land_cover)
      v[v == 200] <- NA
      rr$land_cover <- setValues(rr$land_cover, v)
      
      
      # Calculate distances -----------------------------------------------------
      
      source("R/functions/calcModelDists.R")
      
      cc <- as.data.frame(coordinates(rr))
      cc <- st_as_sf(cc, coords = c("x", "y"), crs = 4326, remove = T)
      
      cc <- calcModelDists(cc, col_target = col_sel, col_any = colony_all, sfs = sfs, buffer = 5e3)
      
      cc <- st_drop_geometry(cc)
      
      for(i in 1:ncol(cc)){
            
            # Allocate values to raster
            rdist <- rr[[1]]
            rdist <- setValues(rdist, cc[,i])
            
            names(rdist) <- names(cc)[i]
            
            # combine with the other covariates
            rr <- stack(rr, rdist)
            
            rm(rdist)
      }
      
      # mask to simulation area
      rr <- mask(rr, colbuff)
      
      
      # Save to temp covts ------------------------------------------------------
      writeRaster(rr, paste0(rasterdir, "temp_covts/", col_sel$id, "_covts", ".grd"),
                  format = "raster", overwrite = T)
      
      
      # Create a data frame for simulation --------------------------------------
      
      # Define a zone for the colony
      col_sel <- col_sel %>% 
         mutate(zone = case_when(lon > 24 & lat > -27.5 ~ 1,
                                 lon > 24 & lat < -27.5 ~ 2,
                                 lon < 24 & lat < -27.5 ~ 3,
                                 lon < 24 & lat > -27.5 ~ 4))
      
      # Load covts stack
      rcovts <- stack(paste0(rasterdir, "temp_covts/", col_sel$id, "_covts.gri"))
      rcovts <- as.data.frame(rcovts, xy = T)
      
      # Remove NA values and fix names
      rcovts <- rcovts %>% 
         rename_with(.fn = ~"elev", .cols = contains("srtm")) %>% 
         rename_with(.fn = ~"slope", .cols = contains("slope")) %>% 
         rename_with(.fn = ~"rugg", .cols = contains("vrm3")) %>% 
         filter(complete.cases(.)) %>% 
         # Set provisional covariate values
         mutate(sl_ = 0,
                res = 1,
                ttnoon = 1, # a one allows the other variables in the interactions to take on their values
                ttnoon_sq = 1,
                log_dist_col = log(dist_col),
                log_dist_col_any = log(dist_col_any),
                log_dist_sfs = log(dist_sfs),
                z_2 = if_else(col_sel$zone == 2, 2, 0),
                z_3 = if_else(col_sel$zone == 3, 3, 0),
                z_4 = if_else(col_sel$zone == 4, 4, 0),
                juv = 1) # We set juvenile to 1. We will remove the juv terms later when dealing with adults (this saves space in disk)
      
      # Fix habitat codes
      rcovts <- rcovts %>% 
         left_join(dplyr::select(hab_meta, "Map code", class_code),
                   by = c("land_cover" = 'Map code'))
      
      rcovts <- rcovts %>% 
         mutate(hab_fct = factor(class_code)) %>% 
         dplyr::select(-c(land_cover, class_code)) %>% 
         mutate(i = 1) %>%
         spread(hab_fct, i, fill = 0)
      
      # save coordinates for later
      cc <- rcovts %>% 
         dplyr::select(x, y)
      
      # Scale new data frame
      sc_vars <- rcovts %>% 
         dplyr::select(names(sds)) %>% 
         sweep(2, unlist(sds), "/")
      
      rcovts <- cbind(dplyr::select(rcovts, -c(names(sds))),
                      sc_vars)
      
      # Assign values of covariates and interactions
      names_coef <- names(ssf_coefs)
      cov_values <- list( )
      
      for(i in seq_along(names_coef)){
         varname <- names_coef[i]
         varnames <- unlist(strsplit(varname, ":"))
         if(!varname %in% names(rcovts)){
            rcovts <- rcovts %>% 
               mutate(!!varname := 0)
         }
         if(length(varnames) > 1){
            cov_values[[i]] <- apply(rcovts[,varnames], 1, "prod")
         } else {
            cov_values[[i]] <- rcovts[,varname]
         }
      }
      
      cov_values <- matrix(data = unlist(cov_values), nrow = nrow(rcovts), ncol = length(names_coef)) %>% 
         as.data.frame()
      names(cov_values) <- names_coef
      
      # cbind(cc, cov_values) %>% 
      #    ggplot() +
      #    geom_raster(aes(x, y, fill = dist_col))
      
      saveRDS(cbind(cc, cov_values), paste0(rasterdir, "df_", col_sel$id, ".rds"))
}

