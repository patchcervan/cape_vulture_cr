fitCVheight <- function(data_train = "data/working/data_height_ready.rds", data_test = "data/working/data_height_test.rds",
                        test_ids, model, save_fit = FALSE, output_file = NULL, plotAUC = FALSE){
   
   # Prepare training data ---------------------------------------------------
   
   # Extract training data
   train <- readRDS(data_train) %>% 
      filter(!is.na(height)) %>% 
      rename(elev = srtm0,
             rugg = vrm3,
             dist_slp = dist_slp_m) %>% 
      mutate(log_dist_col = log(dist_col),
             log_dist_col_any = log(dist_col_any),
             log_dist_sfs = log(dist_sfs),
             log_dist_slp = log(dist_slp),
             risk = if_else(height > 300, 0, 1)) %>% 
      group_by(bird_id) %>% 
      mutate(risk_t1 = lag(risk)) %>% 
      ungroup() %>% 
      # Numeric resolution variable
      mutate(res = as.numeric(str_remove(res_fct, "res_"))) %>% 
      # Define CV groups
      group_by(age_fct, zone_fct) %>% 
      mutate(cv_group = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
      # Keep only training CV groups
      filter(!cv_id %in% test_ids)
   
   # Extract variables needed for the model
   vv <- all.vars(model)
   
   train <- train %>% 
      dplyr::select(all_of(vv))
   
   # Define variables that can be standardized
   vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean")
   
   # Standardize covariates
   train <- train %>%
      mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))
   
   
   # Fit model ---------------------------------------------------------------
   
   # Specify model
   height_model <- glmmTMB(model, family = binomial, data = train, doFit = FALSE)
   
   # Set random effects
   nrm <- length(height_model$parameters$theta)
   str_theta <- height_model$parameters$theta
   
   # Define starting values
   ini_val <- list(beta = height_model$parameters$beta,
                   theta = str_theta)
   
   # remove names just in case
   ini_val <- lapply(ini_val, unname)
   
   # Try to fit model
   rm(height_model) # to free memory
   height_fit <- NULL
   
   try({
      height_fit <- glmmTMB(model, family = binomial, data = train,
                            start = ini_val)
   })
   
   
   # Predict for the test groups ---------------------------------------------
   
   # Extract test data
   test <- readRDS(data_test) %>% 
      filter(!is.na(height)) %>% 
      rename(elev = srtm0,
             rugg = vrm3,
             dist_slp = dist_slp_m) %>% 
      mutate(log_dist_col = log(dist_col),
             log_dist_col_any = log(dist_col_any),
             log_dist_sfs = log(dist_sfs),
             log_dist_slp = log(dist_slp),
             risk = if_else(height > 300, 0, 1)) %>% 
      group_by(bird_id) %>% 
      mutate(risk_t1 = 0) %>% 
      ungroup() %>% 
      # Numeric resolution variable
      mutate(res = as.numeric(str_remove(res_fct, "res_"))) %>% 
      # Define CV groups
      group_by(age_fct, zone_fct) %>% 
      mutate(cv_group = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
      # Keep only training CV groups
      filter(cv_id %in% test_ids)
   
   # To reduce autocorrelation in the validation set, we sample a number of steps to validate on
   set.seed(1283764)

   test <- test %>% 
      group_by(bird_id) %>% 
      add_tally() %>% 
      sample_n(size = if_else(n > 500, 500L, n)) %>% 
      ungroup()
   
   # Extract variables needed for the model
   vv <- all.vars(model)
   
   test <- test %>% 
      dplyr::select(all_of(vv))
   
   # Extract scaling factors
   sds <- lapply(train, attr, "scaled:scale")
   
   sds <- sds[!sapply(sds, is.null)]
   
   # Scale new data frame
   sc_vars <- test %>% 
      dplyr::select(names(sds)) %>% 
      sweep(2, unlist(sds), "/")
   
   test <- cbind(test %>%
                         dplyr::select(-c(names(sds))),
                      sc_vars)
   
   # If model wasn't fit
   out <- tibble(mod = attr(model, "model"),
                 test = attr(test_ids, "id"),
                 AIC = NA,
                 BIC = NA,
                 logLik = NA,
                 cv_auc = NA,
                 formula = paste(model, collapse = ""))
   
   # Predict if model was fitted
   if(!is.null(height_fit) & height_fit$fit$convergence == 0){
      
      # Predict from model
      pred_out <- predict(height_fit, newdata = test, se.fit = F, re.form = NA, allow.new.levels = T)
      
      # Transform to probability scale
      pred_prob <- exp(pred_out) / (1 + exp(pred_out))
      
      require(pROC)
      
      cv_roc <- roc(test$risk, pred_prob)
      
      if(isTRUE(plotAUC)){
         exportfile <- paste0("output/ROC_", attr(model, "model"), "_", attr(test_ids, "id"), ".png" )
         png(file = exportfile)
         plot(cv_roc)
         dev.off()
      }
      
      # Fill in results data frame
      out <- tibble(mod = attr(model, "model"),
                    test = attr(test_ids, "id"),
                    AIC = summary(height_fit)$AIC["AIC"],
                    BIC = summary(height_fit)$AIC["BIC"],
                    logLik = summary(height_fit)$AIC["logLik"],
                    cv_auc = as.numeric(cv_roc$auc),
                    formula = paste(model, collapse = ""))
   }
   
   return(out)
   
}
