fitCVssm <- function(train_data = "data/working/data_height_ready.rds", test_data = "data/working/data_height_test.rds",
                     test_ids, model, save_fit = FALSE, output_file = NULL, seed = NULL){
   
   train_data = "data/working/data_height_ready_noextrm.rds";
   test_data = "data/working/data_height_test.rds";
   test_ids = cv_ids[[1]]; model = models[[1]]; seed = 478476
   output_file = paste0("output/cv_hgt/mod_", attr(model, "model"), "cv_id_", attr(test_ids, "id"))
   
   # Prepare training data ---------------------------------------------------
   
   # Extract training data
   train <- readRDS(train_data)
   
   # Basic processing:
   
   # Remove repeated records
   train <- train %>% 
      group_by(bird_id) %>% 
      distinct(datetime, .keep_all = T) %>% 
      ungroup() %>% 
      # Remove records with no height
      filter(!is.na(height)) %>% 
      # Recalculate dt 
      group_by(bird_id) %>% 
      mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours"))) %>% 
      ungroup() %>% 
      # Change variable names to make them match model specification
      mutate(id = row_number()) %>% 
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
   vv <- c("dt", all.vars(model))
   
   train <- train %>% 
      dplyr::select(all_of(vv))
   
   # Conduct some basic checks
   if(train %>% 
      dplyr::select(all_of(vv[!vv == "dt"])) %>%  # dt has NA but will be removed later
      is.na() %>% any()) warning("There are some missing values in the training set")
   
   # Define variables that can be standardized
   vars_to_st <- c("elev", "slope", "dist_slp", "rugg", "dist_col", "dist_col_any", "dist_roost", "dist_sfs", "NDVI_mean")
   
   # Standardize covariates
   train <- train %>%
      mutate(height = scale(height, center = F)) %>% 
      mutate(across(.cols = vv[vv %in% vars_to_st], ~scale(.x, center = T)))
   
   # Add intercept
   train <- train %>% 
      mutate(intcp = 1)
   
   # Remove last dt of each group
   dts <- train %>% 
      group_by(bird_id) %>%
      slice(-tail(row_number(), 1)) %>%
      pull(dt)
   
   
   # Fit model ---------------------------------------------------------------
   
   data.bundle <- list("z" = train$height, 
                       "dtime" = dts,
                       "X" = dplyr::select(train, -c(bird_id, height, dt)) %>% as.matrix(),
                       "B" = 0.01,
                       "N" = nrow(train),
                       "K" = ncol(dplyr::select(train, -c(bird_id, height, dt))),
                       "J" = n_distinct(train$bird_id),
                       "n_j" = train %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n),
                       "group" = rep(0:(n_distinct(train$bird_id)-1), times = train %>% group_by(bird_id) %>% summarize(n = n()) %>% pull(n)), 
                       "lambda" = 1) 
   
   # Define initial values
   param <- list("mu_alpha" = rep(0, data.bundle$K),
                 "lsig_alpha" = rep(log(0.5), data.bundle$K),
                 "mu_lsig_err" = 0,
                 "sig_lsig_err" = 1,
                 "lsigma" = log(1), 
                 "lbeta" = log(0.1),
                 "delta_alpha" = matrix(rnorm(data.bundle$K*data.bundle$J, 0, 0.01), ncol = data.bundle$K, nrow = data.bundle$J),
                 "delta_lsig_err" = rnorm(data.bundle$J, 0, 0.01),
                 "ztrans" = data.bundle$z,
                 "df_raw" = rep(0, data.bundle$J))
   
   # From previous fit
   # fit <- readRDS("output/fit_hgt_smm_mod1.rds")
   # param <- as.list(fit$par.fixed, fit$par.random)
   
   # Fit TMB model with obs error and covts ----------------------------------
   
   compile("R/functions/TMB/hier_ssm_ou_errt.cpp")
   dyn.load(dynlib("R/functions/TMB/hier_ssm_ou_errt"))
   
   openmp(5)
   
   Obj <- MakeADFun(data = data.bundle, parameters = param, random = c("ztrans", "delta_alpha", "delta_lsig_err"), DLL = "hier_ssm_ou_errt", silent = F)
   
   # Neg Log Likelihood minimization
   lb <- c(rep(-5, data.bundle$K), rep(-3, data.bundle$K), -3, -3, -1, -1, rep(-3, data.bundle$J))
   ub <- c(rep(5, data.bundle$K), rep(2, data.bundle$K), 1, 1, 3, 3, rep(10, data.bundle$J))
   
   Opt <- NULL
   
   try({
      system.time(
         Opt <- optim(Obj$par, Obj$fn, Obj$gr,
                      method = "L-BFGS-B", #Obj$method,
                      lower = lb, upper = ub,
                      control = list(maxit = 1e5, factr = 1e9, trace = 0), hessian = TRUE)
      )
   })
   
   # if(!is.null(Opt)){
   #    fit_summ <- sdreport(Obj, getReportCovariance = FALSE)
   # }
   
   
   # Save fit
   if(!is.null(Opt) && isTRUE(save_fit)){
      saveRDS(Opt, "output/cv_hgt/opt_mod1_t.rds")
      saveRDS(Obj, "output/cv_hgt/obj_mod1_t.rds")
      # saveRDS(fit_summ, file = output_file)
   }
   
   # ssf_fit <- readRDS("hpc/output/ssf_fit_test1_mod2.rds")
   
   Opt <- readRDS("output/cv_hgt/opt_mod1.rds")
   Obj <- readRDS("output/cv_hgt/obj_mod1.rds")
   
   
   # Predict for the test groups ---------------------------------------------
   
   # Extract test data
   test <- readRDS(test_data)
   
   # Basic processing:
   
   # Remove repeated records
   test <- test %>% 
      group_by(bird_id) %>% 
      distinct(datetime, .keep_all = T) %>% 
      ungroup() %>% 
      # Remove records with no height
      filter(!is.na(height)) %>% 
      # Remove records with no dt
      filter(!is.na(dt)) %>% 
      # Recalculate dt 
      group_by(bird_id) %>% 
      mutate(dt = as.numeric(difftime(lead(datetime), datetime, units = "hours"))) %>% 
      ungroup() %>% 
      # Change variable names to make them match model specification
      mutate(id = row_number()) %>% 
      mutate(log_dist_col = log(dist_col),
             log_dist_col_any = log(dist_col_any),
             log_dist_sfs = log(dist_sfs),
             log_dist_slp = log(dist_slp)) %>% 
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
   
   test <- test %>% 
      group_by(cv_id) %>% 
      add_tally() %>% 
      sample_n(size = if_else(n > 100, 100L, n)) %>% 
      ungroup()
   
   test <- test %>% 
      dplyr::select(all_of(vv[!vv == "dt"]), cv_id) %>% # we don't need dt for the predictions 
      filter(complete.cases(.))
   
   # Extract centering factors
   cts <- lapply(train, attr, "scaled:center")
   
   cts <- cts[!sapply(cts, is.null)]
   
   # Center new data frame
   ct_vars <- test %>% 
      dplyr::select(names(cts)) %>% 
      sweep(2, unlist(cts), "-")
   
   test <- cbind(dplyr::select(test, -c(names(cts))),
                 ct_vars)
   
   # Extract scaling factors
   sds <- lapply(train, attr, "scaled:scale")
   
   sds <- sds[!sapply(sds, is.null)]
   
   # Scale new data frame
   sc_vars <- test %>% 
      dplyr::select(names(sds)) %>% 
      sweep(2, unlist(sds), "/")
   
   test <- cbind(dplyr::select(test, -c(names(sds))),
                 sc_vars)
   
   # Add intercept and reorder variables
   test_resp <- test$height
   
   test <- test %>% 
      mutate(intcp = 1) %>% 
      dplyr::select(colnames(data.bundle$X))
   
   # If model wasn't fit
   out <- tibble(mod = attr(model, "model"),
                 test = attr(test_ids, "id"),
                 AIC = NA,
                 BIC = NA,
                 logLik = NA,
                 cv_pi = NA,
                 formula = paste(model, collapse = ""))
   
   # Predict if model was fitted
   if(!is.null(fit_summ) & length(fit_summ) > 1){
      if(ssf_fit$fit$convergence == 0){
         
         vcov <- solve(Opt$hessian)
         
         fit_coef <- Obj$env$last.par[1:ncol(data.bundle$X)]
         names(fit_coef) <- colnames(data.bundle$X)

         pred <- as.matrix(test) %*% matrix(fit_coef, ncol = 1)
         
         # Transform prediction
         B <- data.bundle$B
         pred[pred < B]*(B*tanh(pred/B-1) + B)
         
         plot(test_resp)
         points(pred, col = "red")
         mean(pred) * sds[["height"]]
         mean(train$height) * sds[["height"]]
         median(train$height) * sds[["height"]]
         hist(train$height * sds[["height"]])
         hist(train$height[train$height < 5] * sds[["height"]])
         
         for(i in seq){
            y(i) = (zprime(i) >= b)*zprime(i) + (zprime(i) < b)*(b*tanh(zprime(i)/b-1) + b);
         }
         
         # Predict from model
         # pred_out <- predict(ssf_fit, newdata = test, se.fit = F, re.form = NA, allow.new.levels = T)
         
         # Add bird id and whether the point is used or available. Calculate preference
         # and selection for each bird
      
         pred_df <- test %>% 
            dplyr::select(bird_id, cv_id, case) %>% 
            mutate(pred = exp(2*pred_out)) %>% 
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
