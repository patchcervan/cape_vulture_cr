evalTMBmodel <- function(data, param){
      # TRANSFORMED PARAMETERS
      alpha <- matrix(NA, data$J, data$K);
      sig_alpha = exp(param$lsig_alpha);
      beta = exp(param$lbeta);
      beta_sq = beta^2;
      sigma = exp(param$lsigma);
      sigma_sq = sigma^2;
      mu_t = vector(length = data$N);
      sig_err = vector(length = data$J);
      
      
      df = 18*exp(param$df_raw)/(1 + exp(param$df_raw)) + 2 ;   # this is a "convenient" way to force df > 2
      
      # latent states
      # zlat = invLink(ztrans, B) ;               // back transform flight height into R+
      
      for(j in 1:data$J){
            
            for(k in 1:data$K){
                  alpha[j,k] = param$mu_alpha[k] + sig_alpha[k] * param$delta_alpha[j,k];
            }
            
            sig_err[j] = exp(param$mu_lsig_err + param$sig_lsig_err*param$delta_lsig_err[j]);
            
      }
      
      
      for(int i = 0; i < N; i++){
            int g = group(i);
            
            vector<Type> xv = X.row(i);
            vector<Type> av = alpha.row(g);
            
            mu_t(i) = (xv*av).sum();
            
      }
      
      
      // LIKELIHOOD
      
      // Initialize negative log likelihood
      parallel_accumulator<Type> nll(this);
      
      // Random effects likelihood
      
      for(int j=0; j < J; j++){
            for(int k=0; k < K; k++){
                  nll -= dnorm(delta_alpha(j,k), Type(0.0), Type(1.0), true);
            }
            nll -= dnorm(delta_lsig_err(j), Type(0.0), Type(1.0), true);
      }
      
      
      // PROCESS MODEL
      
      int t = -1;       // Initialize time
      
      for(int j = 0; j < J; j++){
            t += 1;
            
            // For the initial state I use the steady-state variance
            nll -= dnorm(ztrans(t), mu_t(t), pow(sigma_sq/(Type(2.0)*beta), Type(0.5)), true);
            
            // For other steps the process model is:
                  for(int i = 1; i < n_j(j); i++){
                        t += 1;
                        
                        nll -= nll_OU(ztrans(t), ztrans(t-1), dtime(t-j-1), beta, mu_t(t), sigma_sq);
                        
                        // Simulation block for process model
                        SIMULATE {
                              Type Ex = ztrans(t-1)*exp(-beta*dtime(t-j-1)) + mu_t(t)*(Type(1.0) - exp(-beta*dtime(t-j-1))) ;
                              Type Vx = pow(sigma_sq/(Type(2.0)*beta) * (Type(1.0) - exp(-Type(2.0)*beta*dtime(t-j-1))), Type(0.5));
                              
                              ztrans(t) = rnorm(Ex, Vx);
                              zlat(t) = (ztrans(t) >= B)*ztrans(t) + (ztrans(t) < B)*(B*tanh(ztrans(t)/B-1) + B);
                        }
                        
                        
                        // OBSERVATION MODEL
                        
                        nll -= keep(t-j-1)*dt((z(t-j-1) - zlat(t))/sig_err(j), df(j), true);
                        
                        // Simulation block for observation model
                        SIMULATE {
                              z(t-j-1) = (rt(df(j)) + zlat(t))*sig_err(j);
                        }
                        
                  }
      }
}