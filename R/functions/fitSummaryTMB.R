fitSummaryTMB <- function(obj, report, X, random = NULL, osa = FALSE, prof = NULL, const_chk = FALSE){
      
      # prof = c("intcp", "elev", "slope")
      # obj = Obj
      
      
      # Parameter report --------------------------------------------------------
      
      # Prepare names
      covt_names <- names(X)
      fixpar_names <- names(report$par.fixed)
      fixpar_names[1:length(covt_names)] <- covt_names
      
      # Extract summary
      fit_summ <- summary(report)
      
      # Fix names
      row.names(fit_summ)[1:length(fixpar_names)] <- fixpar_names
      
      fit_summ[fixpar_names,]
      
      
      # Correlation between fixed parameters
      covar <- report$cov.fixed
      rho <- covar
      
      for(i in 1:nrow(rho)){
            for(j in 1:ncol(rho)){
                  rho[i,j] <- covar[i,j]/sqrt(covar[i,i]*covar[j,j])
            }
      }
      
      rho
      
      aic <- 2*Opt$value + length(Opt$par)
      
      out <- list(summary = fit_summ[fixpar_names,],
                  cor = rho,
                  aic = aic,
                  npar = length(Opt$par))
      
      if(osa == TRUE){
         osa_res <- oneStepPredict(obj, observation.name = "z", discrete = FALSE,
                                   data.term.indicator = "keep", trace = FALSE,
                                   parallel = TRUE)
         
         hist(osa_res$residual)
         
         qqnorm(osa_res$residual,
                main="", pch=16, cex=0.8)
         
         qqline(osa_res$residual, lwd = 1)
         
         acf(na.omit(osa_res$residual))
         
         out$osares <- osa_res$residual
         
      } else {
         rep <- obj$report()
         hist(rep$residual)
         qqnorm(rep$residual)
         abline(0,1)
         acf(na.omit(rep$residual))
         
         out$res <- rep$residual
      }
      
      
      # Check Laplace approx. consistency
      if(const_chk == TRUE){
            print(checkConsistency(obj, trace = F))
      }
      
      
      # Parameter profile
      if(!is.null(prof)){
            require(purrr)
            
            # Change objective function par names
            names(obj$env$last.par.best)[1:length(fixpar_names)] <- fixpar_names
            
            profs <- map(prof, ~tmbprofile(obj, .x, trace = F))
            profs <- map2(profs, prof, ~rename(.x, par_val = 1,
                                               obj_val = 2) %>%
                                mutate(par_name = .y))
            profs <- do.call(rbind, profs)
            
            print(ggplot(profs) +
                        geom_line(aes(x = par_val, y = obj_val)) +
                        facet_wrap("par_name", scales = "free"))
      }
      
      return(out)
      
}

# 
# # Process parameters
# exp(fit_summ["lphi",1]) / (1 + exp(fit_summ["lphi",1]))
# 
# exp(fit_summ[c("lsigma", "lsig_err"),])
# 
# # location error
# exp(fit_summ[c("lsig_err"), 1]) * attr(data.bundle$z, "scaled:scale")
# 
# # Regression coefficients
# fit_summ[str_detect(row.names(fit_summ), "gamma"), ]
# exp(fit_summ[str_detect(row.names(fit_summ), "lgamma"), ])
# 
# # Extract latent states
# zlat <- summary(report, "report")[-c(1:7),]
# 
# # Parameter profile
# prof_phi <- tmbprofile(Obj, "lsigma", trace = FALSE, ytol = 20)
# 
# prof_lgamma <- tmbprofile(Obj, "lgamma", trace = FALSE)
# plot(lgamma)