
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
   DATA_MATRIX(Y);         // covariates for error variance
   DATA_SCALAR(B);         // Threshold for the inverse-link function
   DATA_INTEGER(N);        // number of observations
   DATA_INTEGER(K);        // number of covariates for mean altitude
   DATA_INTEGER(L);        // number of covariates for error
   DATA_INTEGER(J);        // number of individuals
   DATA_IVECTOR(n_j);      // number of observations per individual
   DATA_IVECTOR(group);    // indicator for individual group
   
   // For one-step-ahead residuals
   // DATA_VECTOR_INDICATOR(keep, z);
   
   // PARAMETERS
   PARAMETER_VECTOR(mu_alpha);      // mean effect of covariates on mean altitude
   PARAMETER_VECTOR(mu_gamma);      // mean effect of covariates on error variance
   PARAMETER_VECTOR(lsig_alpha);    // sd of effect of covariates on mean altitude
   PARAMETER_VECTOR(lsig_gamma);    // sd of effect of covariates on error variance
   PARAMETER_MATRIX(alpha);   // deviations from the mean effects on mean altitude for individuals
   PARAMETER_MATRIX(gamma);   // deviations from the mean effects on error variance for individuals 
   PARAMETER(lsigma);               // process variance
   PARAMETER(lbeta);                // process autocorrelation
   PARAMETER_VECTOR(ztrans);        // transformed process
   //PARAMETER_VECTOR(mu0);
   // PARAMETER(df_raw);
   
   // TRANSFORMED PARAMETERS
   // matrix<Type> alpha(J,K);
   // matrix<Type> gamma(J,L);
   vector<Type> sig_alpha = exp(lsig_alpha);
   vector<Type> sig_gamma = exp(lsig_gamma);
   Type beta = exp(lbeta);
   Type beta_sq = pow(beta, Type(2.0));
   Type sigma = exp(lsigma);
   Type sigma_sq = pow(sigma, Type(2.0));
   vector<Type> mu_t(N);
   vector<Type> lsig_err(N);
   vector<Type> sig_err = exp(lsig_err);
   // Type mu0 = mu_alpha(0);
   // Type df = Type(18.0)*exp(df_raw)/(Type(1.0) + exp(df_raw)) + Type(2.0) ;   // this is a "convenient" way to force df > 2
   
   // latent states
   vector<Type> zlat = invLink(ztrans, B) ;               // back transform flight height into R+
   
   // Pre-calculate correlations
   // vector<Type> phi = exp(-beta*dtime);
   
   
   // for(int j = 0; j < J; j++){
   //    
   //    for(int k = 0; k < K; k++){
   //       alpha(j,k) = mu_alpha(k) + sig_alpha(k) * delta_alpha(j,k);
   //       }
   //    for(int l = 0; l < L; l++){
   //       gamma(j,l) = mu_gamma(l) + sig_gamma(l) * delta_gamma(j,l);
   //    }
   //    
   // }
   

   for(int i = 0; i < N; i++){
      int g = group(i);
      
      vector<Type> xv = X.row(i);
      vector<Type> yv = Y.row(i);
      vector<Type> av = alpha.row(g);
      vector<Type> gv = gamma.row(g);
      
      mu_t(i) = (xv*av).sum();
      lsig_err(i) = (yv*gv).sum();
      
   }
   
   
   // LIKELIHOOD
   
   // Initialize negative log likelihood
   parallel_accumulator<Type> nll(this);
   
   // Random effects likelihood
   for(int j=0; j < J; j++){
      for(int k=0; k < K; k++){
         nll -= dnorm(alpha(j,k), mu_alpha(k), sig_alpha(k), true);
      }
      for(int l=0; l < L; l++){
         nll -= dnorm(gamma(j,l), mu_gamma(l), sig_gamma(l), true);
      }
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
         // SIMULATE {
         //    ztrans(t) = rnorm(ztrans(t-1)*phi(t-j-1) + mu_t(t-j-1)*(Type(1.0) - phi(t-j-1)),
         //           pow(sigma_sq/(Type(2.0)*beta) * (Type(1.0) - pow(phi(t-j-1), Type(2.0))), Type(0.5)));
         //    zlat(t) = (ztrans(t) >= B)*ztrans(t) + (ztrans(t) < B)*(B*tanh(ztrans(t)/B-1) + B);
         // }
         
         // OBSERVATION MODEL
         
         // nll -= keep(i)*dt((z(i) - zlat(i+1))/sig_err(i), df, true);
         // nll -= keep(t-j-1)*dnorm(z(t-j-1), zlat(t), sig_err(t-j-1), true);
         nll -= dnorm(z(t-j-1), zlat(t), sig_err(t-j-1), true);
         
         // Simulation block for observation model
         // SIMULATE {
         //    z(i) = rnorm(zlat(i+1), sig_err(i));
         // }
         
      }
   }
   

   // REPORTS
   ADREPORT(beta);
   REPORT(beta);
   ADREPORT(alpha);
   REPORT(alpha);
   ADREPORT(gamma);
   REPORT(gamma);
   ADREPORT(zlat);
   REPORT(zlat);
   // ADREPORT(df);
   // REPORT(df);
   
   // // Report simulated values
   // SIMULATE{
   //    REPORT(ztrans);
   //    REPORT(zlat);
   //    // REPORT(z);
   // }
   
   return nll;
   
}

