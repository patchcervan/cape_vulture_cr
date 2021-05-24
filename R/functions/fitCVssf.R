fitCVssf <- function(train_data = "data/working/data_ssf_ready.rds", test_data = "data/working/data_ssf_test.rds",
                     test_ids, model, save_fit = FALSE, output_file = NULL, seed = NULL){
   
   # train_data = "data/working/data_ssf_ready.rds"; test_data = "data/working/data_ssf_test.rds"; test_ids = cv_ids[[1]]; model = models[[2]]
   
   require(splines)
   
   # Prepare training data ---------------------------------------------------
   
   # Extract training data
   train <- readRDS(train_data) %>% 
      mutate(id = row_number(),
             step_id_ = paste(bird_id, step_id_, sep = "_")) %>% 
      # Numeric resolution variable
      mutate(res = as.numeric(str_remove(res_fct, "res_"))) %>% 
      # Numeric response variable
      mutate(case = if_else(case_ == TRUE, 1, 0)) %>% 
      mutate(log_dist_col = log(dist_col),
             log_dist_col_any = log(dist_col_any),
             log_dist_sfs = log(dist_sfs),
             log_dist_slp = log(dist_slp)) %>% 
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
   
   # Conduct some basic checks
   if(train %>% 
      dplyr::select(all_of(vv)) %>% 
      is.na() %>% any()) warning("There are some missing values in the training set")
   
   # Define variables that can be standardized
   vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean", "sl_")
   
   # Standardize covariates
   train <- train %>%
      mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = F)))
   
   
   # Fit model ---------------------------------------------------------------
   
   # Specify model
   ssf_model <- glmmTMB(model, family = poisson, data = train, doFit = FALSE)
   
   # Set random effect with very large variance for step_id (see Muff et al. 2020)
   nrm <- length(ssf_model$parameters$theta)
   str_theta <- ssf_model$parameters$theta
   str_theta[nrm] <- log(1e3)
   
   # Define starting values
   ini_val <- list(beta = ssf_model$parameters$beta,
                   theta = str_theta)
   
   # remove names just in case
   ini_val <- lapply(ini_val, unname)
   
   # Try to fit model
   rm(ssf_model) # to free memory
   ssf_fit <- NULL
   
   try({
      ssf_fit <- glmmTMB(model, family = poisson, data = train,
                            map = list(theta = factor(c(seq(1, nrm-1, 1), NA))),
                            start = ini_val)
   })
   
   # Save fit
   if(!(is.null(ssf_fit)) & isTRUE(save_fit)){
      saveRDS(fixef(ssf_fit), file = output_file)
   }
   
   # ssf_fit <- readRDS("hpc/output/ssf_fit_test1_mod2.rds")
   
   # Predict for the test groups ---------------------------------------------
   
   # Extract test data
   test <- readRDS(test_data) %>%
      mutate(id = row_number(),
             step_id_ = paste(bird_id, step_id_, sep = "_")) %>% 
      # Numeric resolution variable
      mutate(res = as.numeric(str_remove(res_fct, "res_"))) %>% 
      mutate(dist_col = as.numeric(dist_col),
             log_dist_col = log(dist_col),
             log_dist_col_any = log(dist_col_any),
             log_dist_sfs = log(dist_sfs),
             log_dist_slp = log(dist_slp)) %>% 
      # Numeric response variable
      mutate(case = if_else(case_ == TRUE, 1, 0)) %>% 
      # Define CV groups
      group_by(age_fct, zone_fct) %>% 
      mutate(cv_group = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
      # Keep only test CV groups
      filter(cv_id %in% test_ids)
   
   # To give more or less the same weight to each test bird we sample a number of steps to validate on
   if(!is.null(seed)){
      set.seed(seed)
   }
   
   test_steps <- test %>% 
      group_by(cv_id) %>% 
      mutate(n_steps = n_distinct(step_id_)) %>% 
      group_by(step_id_, .add = T) %>% 
      summarize(n_steps = unique(n_steps)) %>% 
      group_by(cv_id) %>% 
      sample_n(size = if_else(n_steps < 500, n_steps, 500L)) %>% 
      pull(step_id_)
   
   test <- test %>% 
      filter(step_id_ %in% test_steps)
   
   # Extract variables needed for the model
   # and remove any missing values
   vv <- all.vars(model)
   
   test <- test %>% 
      mutate(#hourday = lubridate::hour(t1_),
             #ttnoon = hourday - 10,
             #ttnoon_sq = (ttnoon)^2,
             #res = 8,
             step_id_ = "new_step") %>% 
      # mutate(sl_ = 0, ttnoon = 0, ttnoon_sq = 0, res = 0, step_id_ = "new_step") %>% 
      dplyr::select(all_of(vv), cv_id) %>% 
      filter(complete.cases(.))
   
   # Extract scaling factors
   sds <- lapply(train, attr, "scaled:scale")
   
   sds <- sds[!sapply(sds, is.null)]
   
   # Scale new data frame
   sc_vars <- test %>% 
      dplyr::select(names(sds)) %>% 
      sweep(2, unlist(sds), "/")
   
   test <- cbind(dplyr::select(test, -c(names(sds))),
                 sc_vars)
   
   # Make step length zero to remove any effects of movement
   # test <- test %>%
   #    mutate(sl_ = 0)
   
   # If model wasn't fit
   out <- tibble(mod = attr(model, "model"),
                 test = attr(test_ids, "id"),
                 AIC = NA,
                 BIC = NA,
                 logLik = NA,
                 cv_pi = NA,
                 formula = paste(model, collapse = ""))
   
   # Predict if model was fitted
   if(!is.null(ssf_fit) & length(ssf_fit) > 1){
      if(ssf_fit$fit$convergence == 0){
         
         # Predict from model
         pred_out <- predict(ssf_fit, newdata = test, se.fit = F, re.form = NA, allow.new.levels = T)
         
         # Add bird id and whether the point is used or available. Calculate preference
         # and selection for each bird
      
         pred_df <- test %>% 
            dplyr::select(bird_id, cv_id, case) %>% 
            mutate(pred = exp(pred_out)) %>% 
            group_by(cv_id) %>% 
            mutate(denom = sum(pred),
                   sel = pred / denom) %>% 
            ungroup()
         
         # Categorize selection
         nbreaks <- 10
         pred_df <- pred_df %>%
            group_by(cv_id) %>%
            mutate(sel_fct = cut(sel, breaks = quantile(sel, seq(0, 1, 1/nbreaks), na.rm = TRUE),
                                 labels = 1:nbreaks, include.lowest = T)) %>% 
            ungroup()
         

         # pred_df %>%
         #    pull(sel_fct) %>%
         #    as.numeric() %>%
         #    hist()
         # 
         # pred_df %>%
         #    filter(case == 1) %>%
         #    pull(sel_fct) %>%
         #    as.numeric() %>%
         #    hist()
         # 
         # pred_df %>%
         #    group_by(sel_fct, cv_id, case) %>%
         #    summarize(count = n()) %>%
         #    ungroup(case) %>%
         #    mutate(total = sum(count),
         #           freq = count/total) %>%
         #    filter(case == 1) %>%
         #    group_by(cv_id) %>%
         #    summarize(cv_pi = cor(as.numeric(sel_fct), freq, method = "spearman")) %>%
         #    print(n = Inf) %>%
         #    summarize(avg_cv_pi = mean(cv_pi))
            
         
         # Estimate rank correlation
         cv_pi <- pred_df %>% 
            filter(case == 1) %>% 
            group_by(sel_fct) %>% 
            summarize(freq = n()/nrow(.)) %>% 
            summarize(cv_pi = cor(as.numeric(sel_fct), freq, method = "spearman")) %>% 
            pull(cv_pi)
         
         # Fill in results data frame
         out <- tibble(mod = attr(model, "model"),
                       test = attr(test_ids, "id"),
                       AIC = summary(ssf_fit)$AIC["AIC"],
                       BIC = summary(ssf_fit)$AIC["BIC"],
                       logLik = summary(ssf_fit)$AIC["logLik"],
                       cv_pi = cv_pi,
                       formula = paste(model, collapse = ""))
      }
   }
   
   return(out)
   
}
