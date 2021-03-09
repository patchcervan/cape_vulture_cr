
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
   DATA_VECTOR(z);           // observed z coordinates (diff observed altitude - ground altitude)
   DATA_VECTOR(dtime);           // time increments
   DATA_MATRIX(X);            // altitude covariates for mean
   DATA_MATRIX(Y);            // altitude covariates for variance
   DATA_SCALAR(B);            // Threshold for the inverse-link function
   
   // For one-step-ahead residuals
   DATA_VECTOR_INDICATOR(keep, z);
   
   // PARAMETERS
   PARAMETER_VECTOR(alpha);
   PARAMETER_VECTOR(gamma);
   PARAMETER(lsigma);
   PARAMETER(lbeta);
   PARAMETER_VECTOR(ztrans);
   // PARAMETER(df_raw);
   
   // TRANSFORMED PARAMETERS
   Type beta = exp(lbeta);
   Type beta_sq = pow(beta, Type(2.0));
   Type sigma = exp(lsigma);
   Type sigma_sq = pow(sigma, Type(2.0));
   vector<Type> mu_t = X * alpha;
   Type mu0 = alpha(0);
   vector<Type> sig_err = exp(Y * gamma);
   
   // Type df = Type(18.0)*exp(df_raw)/(Type(1.0) + exp(df_raw)) + Type(2.0) ;   // this is a "convenient" way to force df > 2
   
   // latent states
   vector<Type> zlat = invLink(ztrans, B) ;               // back transform flight height into R+

   // Pre-calculate correlations
   vector<Type> phi = exp(-beta*dtime);
   
   // LIKELIHOOD
   
   int Nz = z.size() ;
   
   // Initialize negative log likelihood
   Type nll = 0.0;
   
   // PROCESS MODEL
   
   // For the initial state I use the steady-state variance
   nll -= dnorm(ztrans(0), mu0, pow(sigma_sq/(Type(2.0)*beta), Type(0.5)), true);
   
   // For other steps the process model is:
   for(int i = 1; i < ztrans.size(); i++){
      
      nll -= nll_OU(ztrans(i), ztrans(i-1), dtime(i-1), beta, mu_t(i-1), sigma_sq);
      
      // Simulation block for process model
      SIMULATE {
         ztrans(i) = rnorm(ztrans(i-1)*phi(i-1) + mu_t(i-1)*(Type(1.0) - phi(i-1)),
                pow(sigma_sq/(Type(2.0)*beta) * (Type(1.0) - pow(phi(i-1), Type(2.0))), Type(0.5)));
         zlat(i) = (ztrans(i) >= B)*ztrans(i) + (ztrans(i) < B)*(B*tanh(ztrans(i)/B-1) + B);
      }
   }
   
   // OBSERVATION MODEL
   for(int i = 0; i < Nz; i++){
      
      // nll -= keep(i)*dt((z(i) - zlat(i+1))/sig_err(i), df, true);
      nll -= keep(i)*dnorm(z(i), zlat(i+1), sig_err(i), true);
      // Simulation block for observation model
      // SIMULATE {
      //    z(i) = rnorm(zlat(i+1), sig_err(i));
      // }
   }
   
   
   
   // REPORTS
   ADREPORT(beta);
   REPORT(beta);
   ADREPORT(sig_err);
   REPORT(sig_err);
   ADREPORT(zlat);
   REPORT(zlat);
   // ADREPORT(df);
   // REPORT(df);
   
   // Report simulated values
   SIMULATE{
      REPORT(ztrans);
      REPORT(zlat);
      // REPORT(z);
   }
   
   return nll;
   
}

