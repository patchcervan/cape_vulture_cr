fitCVssf <- function(data, test_ids, model){
   
   # Prepare training data ---------------------------------------------------
   
   # Extract training data
   train_data <- data %>% 
      mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
      filter(!cv_id %in% test_ids)
   
   # Extract variables needed for the model
   vv <- all.vars(model)
   
   train_data <- train_data %>% 
      dplyr::select(all_of(vv))
   
   # Define variables that can be standardized
   vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean", "sl_")
   
   # Standardize covariates
   train_data <- train_data %>%
      mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))
   
   
   # Fit model ---------------------------------------------------------------
   
   # Specify model
   ssf_model <- glmmTMB(model, family = poisson, data = train_data, doFit = FALSE)
   
   # Set random effect with very large variance for step_id (see Muff et al. 2020)
   nrm <- length(ssf_model$parameters$theta)
   str_theta <- ssf_model$parameters$theta
   str_theta[nrm] <- log(1e3)
   
   # Define starting values
   ini_val <- list(beta = ssf_model$parameters$beta,
                   theta = str_theta)
   
   # remove names just in case
   ini_val <- lapply(ini_val, unname)
   
   # From previous fit? We might need to help models 1 and 2.
   # if(attr(model, "model") == "mod1"){
   #    ini_val <- list(beta = c(-12.61,-2.61,0,1.56,0.17,0,0.08,-0.28,-0.33,-1.23,-0.92,
   #                             -0.11,0.78,3.31,1.01,0.28,0.62,-3.51,-4.31,-0.04,-0.08,
   #                             0.05,0.34,-0.24,0.04,-0.22,0.07,0.79,0,0,0,-0.09,0.11,0.14),
   #                    theta = c(11.04,4.1,6.75,1.99,1.02,0,0.15,0.33,0.34,0.77,0.74,0.69,0.72,1000))
   # } else if(attr(model, "model") == "mod2"){
   #    ini_val <- list(beta = c(-12.61,-2.61,0,1.56,0.17,0.08,-0.28,-0.33,-1.23,-0.92,
   #                             -0.11,0.78,3.31,1.01,0.28,0.62,-3.51,-4.31,-0.04,-0.08,
   #                             0.05,0.34,-0.24,0.04,-0.22,0.07,0.79,-0.09,0.11,0.14),
   #                    theta = c(11.04,4.1,6.75,1.99,1.02,0.15,0.33,0.34,0.77,0.74,0.69,0.72,1000))
   # }
   
   # Try to fit model
   rm(ssf_model) # to free memory
   ssf_fit_rm <- NULL
   
   try({
      ssf_fit_rm <- glmmTMB(model, family = poisson, data = train_data,
                            map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                            start = ini_val)
   })
   
   
   # Predict for the test groups ---------------------------------------------
   
   # Extract test data
   test_data <- data %>% 
      mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
      filter(cv_id %in% test_ids,
             !is.na(elev))
   
   # To reduce autocorrelation in the validation set, we sample a number of steps to validate on
   set.seed(1283764)
   test_steps <- test_data %>% 
      group_by(cv_id) %>% 
      mutate(n_steps = n_distinct(step_id_)) %>% 
      group_by(step_id_, .add = T) %>% 
      summarize(n_steps = unique(n_steps)) %>% 
      group_by(cv_id) %>% 
      sample_n(size = if_else(n_steps < 500, n_steps, 500L)) %>% 
      pull(step_id_)
   
   test_data <- test_data %>% 
      filter(step_id_ %in% test_steps)
   
   # Extract variables needed for the model
   vv <- all.vars(model)
   
   test_data <- test_data %>% 
      dplyr::select(all_of(vv))
   
   # Extract scaling factors
   sds <- lapply(train_data, attr, "scaled:scale")
   
   sds <- sds[!sapply(sds, is.null)]
   
   # Scale new data frame
   sc_vars <- test_data %>% 
      dplyr::select(names(sds)) %>% 
      sweep(2, unlist(sds), "/")
   
   test_data <- cbind(test_data %>%
                         dplyr::select(-c(names(sds))),
                      sc_vars)
   
   # Make step length zero to remove any effects of movement
   test_data <- test_data %>%
      mutate(sl_ = 0)
   
   # If model wasn't fit
   out <- tibble(mod = attr(model, "model"),
                 test = attr(test_ids, "id"),
                 AIC = NA,
                 BIC = NA,
                 logLik = NA,
                 cv_sel_coef = NA,
                 formula = paste(model, collapse = ""))
   
   # Predict if model was fitted
   if(!is.null(ssf_fit_rm) & length(ssf_fit_rm) > 1){
      if(ssf_fit_rm$fit$convergence == 0){
         
         # Predict from model
         pred_out <- predict(ssf_fit_rm, newdata = test_data, se.fit = F, re.form = NA, allow.new.levels = T)
         
         # Add bird id and whether the point is used or available and calculate preference
         # and selection
         pred_df <- test_data %>% 
            dplyr::select(bird_id, case) %>% 
            mutate(pred = exp(2*pred_out),
                   sel = pred / sum(pred))
         
         # Calculate the selection coefficients of used points
         sel_coef <- pred_df %>% 
            filter(case == 1) %>% 
            summarize(sel_coef = sum(log(sel))) %>% 
            pull(sel_coef)
         
         # Fill in results data frame
         out <- tibble(mod = attr(model, "model"),
                       test = attr(test_ids, "id"),
                       AIC = summary(ssf_fit_rm)$AIC["AIC"],
                       BIC = summary(ssf_fit_rm)$AIC["BIC"],
                       logLik = summary(ssf_fit_rm)$AIC["logLik"],
                       cv_sel_coef = sel_coef,
                       formula = paste(model, collapse = ""))
      }
   }
   
   return(out)
   
}
