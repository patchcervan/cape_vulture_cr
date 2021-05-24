
#include <TMB.hpp>
#include <math.h>
#include <cmath>       
using namespace density;


// FUNCTIONS

// Inverse-Link function R -> R+*
template<class Type>
vector<Type> invLink(vector<Type> zprime, Type b) {
   
   int Nz = zprime.size();
   
   vector<Type> y(Nz);
   
   y.setZero();
   
   for(int i = 0; i < Nz; i++){
      y(i) = (zprime(i) >= b)*zprime(i) + (zprime(i) < b)*(b*tanh(zprime(i)/b-1) + b);
   }
   
   return y ;
}

// 1D OU-position model
template<class Type>
Type nll_OU(Type x, Type x0, Type dtime, Type beta, Type mu, Type sigma_sq){

   Type r = 0.0;
   Type Ex = x0*exp(-beta*dtime) + mu*(Type(1.0) - exp(-beta*dtime)) ;
   Type Vx = pow(sigma_sq/(Type(2.0)*beta) * (Type(1.0) - exp(-Type(2.0)*beta*dtime)), Type(0.5));

   r += dnorm(x, Ex, Vx, true);

   return r;
}



template<class Type>
Type objective_function<Type>::operator() ()
{
   // DATA
   DATA_VECTOR(z);         // observed z coordinates (diff observed altitude - ground altitude)
   DATA_VECTOR(dtime);     // time increments
   DATA_MATRIX(X);         // covariates for mean altitude
   DATA_SCALAR(B);         // Threshold for the inverse-link function
   DATA_INTEGER(N);        // number of observations
   DATA_INTEGER(K);        // number of covariates for mean altitude
   DATA_INTEGER(J);        // number of individuals
   DATA_IVECTOR(n_j);      // number of observations per individual
   DATA_IVECTOR(group);    // indicator for individual group
   
   // For one-step-ahead residuals
   DATA_VECTOR_INDICATOR(keep, z);
   
   // PARAMETERS
   PARAMETER_VECTOR(mu_alpha);      // mean effect of covariates on mean altitude
   PARAMETER_VECTOR(lsig_alpha);    // sd of effect of covariates on mean altitude
   PARAMETER(mu_lsig_err);
   PARAMETER(sig_lsig_err);
   PARAMETER(lsigma);               // process variance
   PARAMETER(lbeta);                // process autocorrelation
   PARAMETER_MATRIX(delta_alpha);   // deviations from the mean effects on mean altitude for individuals
   PARAMETER_VECTOR(delta_lsig_err);
   PARAMETER_VECTOR(ztrans);        // transformed process
   //PARAMETER_VECTOR(mu0);
   // PARAMETER(df_raw);
   
   // TRANSFORMED PARAMETERS
   matrix<Type> alpha(J,K);
   vector<Type> sig_alpha = exp(lsig_alpha);
   Type beta = exp(lbeta);
   Type beta_sq = pow(beta, Type(2.0));
   Type sigma = exp(lsigma);
   Type sigma_sq = pow(sigma, Type(2.0));
   vector<Type> mu_t(N);
   vector<Type> sig_err(J);
   
   // Type mu0 = mu_alpha(0);
   // Type df = Type(18.0)*exp(df_raw)/(Type(1.0) + exp(df_raw)) + Type(2.0) ;   // this is a "convenient" way to force df > 2
   
   // latent states
   vector<Type> zlat = invLink(ztrans, B) ;               // back transform flight height into R+
   
   for(int j = 0; j < J; j++){

      for(int k = 0; k < K; k++){
         alpha(j,k) = mu_alpha(k) + sig_alpha(k) * delta_alpha(j,k);
      }
      
      sig_err(j) = exp(mu_lsig_err + sig_lsig_err*delta_lsig_err(j));
      
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
         
         nll -= keep(t-j-1)*dnorm(z(t-j-1), zlat(t), sig_err(j), true);
         
         // Simulation block for observation model
         SIMULATE {
            z(t-j-1) = rnorm(zlat(t), sig_err(j));
         }
         
      }
   }
   

   // REPORTS
   ADREPORT(zlat);
   REPORT(zlat);
   // ADREPORT(beta);
   // REPORT(beta);
   // ADREPORT(alpha);
   // REPORT(alpha);
   // ADREPORT(gamma);
   // REPORT(gamma);
   // ADREPORT(df);
   // REPORT(df);
   
   // Report simulated values
   SIMULATE{
      REPORT(ztrans);
      REPORT(zlat);
   //    REPORT(z);
   }
   
   return nll;
   
}

