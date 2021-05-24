# Load model
ssf_fit_rm <- readRDS("hpc/output/ssf_fit_test20_mod2.rds")


# Predict for the test groups ---------------------------------------------

# test data
test_data <- readRDS("data/working/data_ssf_test.rds")

# Extract test data
test <- test_data %>% 
   filter(age_fct != "unknown") %>% 
   rename(elev = srtm0,
          rugg = vrm3,
          dist_slp = dist_slp_m) %>% 
   mutate(dist_col = as.numeric(dist_col),
          log_dist_col = log(dist_col),
          log_dist_col_any = log(dist_col_any),
          log_dist_sfs = log(dist_sfs),
          log_dist_slp = log(dist_slp)) %>% 
   mutate(case = if_else(case_ == TRUE, 1, 0)) %>% 
   group_by(age_fct, zone_fct) %>% 
   mutate(cv_group = cur_group_id()) %>% 
   ungroup() %>% 
   mutate(cv_id = paste(bird_id, cv_group, sep = "_")) %>% 
   filter(cv_id %in% test_ids,
          !is.na(elev))


# To reduce autocorrelation in the validation set, we sample a number of steps to validate on
set.seed(1283764)
test <- test %>% 
      group_by(cv_id, case_) %>% 
      mutate(n_steps = n()) %>% 
      sample_n(size = case_when(case_ == FALSE ~ if_else(n_steps < 5000, n_steps, 5000L),
                                case_ == TRUE ~ if_else(n_steps < 500, n_steps, 500L))) %>% 
      ungroup()

# Extract variables needed for the model
vv <- all.vars(model)

test <- test %>% 
      mutate(sl_ = 0, ttnoon = 0, ttnoon_sq = 0, res = 0, step_id_ = "new_step") %>% 
      dplyr::select(all_of(vv), cv_id)

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

# Make step length zero to remove any effects of movement
test <- test %>%
      mutate(sl_ = 0)

# If model wasn't fit
out <- tibble(mod = attr(model, "model"),
              test = attr(test_ids, "id"),
              AIC = NA,
              BIC = NA,
              logLik = NA,
              cv_pi = NA,
              formula = paste(model, collapse = ""))

# Predict if model was fitted
if(!is.null(ssf_fit_rm) & length(ssf_fit_rm) > 1){
   if(ssf_fit_rm$fit$convergence == 0){
      
      # Predict from model
      rm(vults, train, test_data) # to free memory
      pred_out <- predict(ssf_fit_rm, newdata = test, se.fit = F, re.form = NA, allow.new.levels = T)
      
      # Add bird id and whether the point is used or available. Calculate preference
      # and selection for each bird
      pred_df <- test %>% 
         dplyr::select(bird_id, cv_id, case) %>% 
         mutate(pred = exp(2*pred_out)) %>% 
         group_by(cv_id) %>% 
         mutate(denom = sum(pred)) %>%
         ungroup() %>% 
         mutate(sel = pred / denom)
      
      pred_df %>% 
         group_by(cv_id) %>% 
         summarize(avg_sel = mean(sel),
                   sd_sel = sd(sel))
      
      # Categorize selection
      nbreaks <- 10
      breaks <- quantile(pred_df$sel, seq(0, 1, 1/nbreaks), na.rm = TRUE)
      
      selcat <- cut(pred_df$sel, breaks = breaks, labels = 1:nbreaks)
      pred_df$sel_fct <- selcat
      
      pred_df %>%
         pull(sel_fct) %>%
         as.numeric() %>%
         hist()

      pred_df %>%
         filter(case == 1) %>%
         pull(sel_fct) %>%
         as.numeric() %>%
         hist()
      
      # Estimate rank correlation
      cv_df <- pred_df %>% 
         filter(case == 1) %>% 
         group_by(sel_fct) %>% 
         summarize(freq = n()/nrow(.))
      
      barplot(names = as.numeric(cv_df$sel_fct), height = cv_df$freq)
      cv_pi <- cor(as.numeric(cv_df$sel_fct), cv_df$freq, method = "spearman")
      
      
      # Fill in results data frame
      out <- tibble(mod = attr(model, "model"),
                    test = attr(test_ids, "id"),
                    AIC = summary(ssf_fit_rm)$AIC["AIC"],
                    BIC = summary(ssf_fit_rm)$AIC["BIC"],
                    logLik = summary(ssf_fit_rm)$AIC["logLik"],
                    cv_pi = cv_pi,
                    formula = paste(model, collapse = ""))
   }
}