
#include <TMB.hpp>
#include <math.h>
#include <cmath>       
using namespace density;


// FUNCTIONS

// Inverse-Link function R -> R+*
template<class Type>
vector<Type> invLink(vector<Type> zprime, Type b) {
   
   int Nz = zprime.size();
   
   vector<Type> z(Nz);
   
   z.setZero();
   
   for(int i = 0; i < (Nz-1); i++){
      z(i) = (zprime(i) >= b)*zprime(i) + (zprime(i) < b)*(b*tanh(zprime(i)/b-1) + b);
   }
   
   return z ;
}

// 1D OU-position model
template<class Type>
Type nll_OU(Type z, Type z0, Type dt, Type beta, Type mu, Type sigma_sq){
   
   Type r = 0.0;
   Type Ez = z0*exp(-beta*dt) + mu*(Type(1.0) - exp(-beta*dt)) ;
   Type Vz = pow(sigma_sq/(Type(2.0)*beta) * (Type(1.0) - exp(-Type(2.0)*beta*dt)), Type(0.5));
   
   r += dnorm(z, Ez, Vz, true);
   
   return r;
}


template<class Type>
Type objective_function<Type>::operator() ()
{
   // DATA
   DATA_VECTOR(z);           // observed z coordinates (diff observed altitude - ground altitude)
   DATA_VECTOR(dt);          // time intervals btw two records
   
   
   // PARAMETERS
   PARAMETER(lbeta);
   PARAMETER(lsigma); 
   PARAMETER(lmu);
   PARAMETER(lsig_err);
   PARAMETER_VECTOR(ztrans);
   
   // TRANSFORMED PARAMETERS
   Type sigma = exp(lsigma);
   Type sigma_sq = pow(sigma, Type(2.0));
   Type beta = exp(lbeta);
   Type mu = exp(lmu);
   Type sig_err = exp(lsig_err);
   vector<Type> zlat = invLink(ztrans, Type(4.0)) ;               // back transform flight height into R+*
   vector<Type> err = z - zlat;
   
   
   // LIKELIHOOD
   
   int Nz = z.size() ;
   
   // Initialize negative log likelihood
   Type nll = 0.0;
   
   // PROCESS MODEL
   
   // For the initial state I use the steady-state variance
   nll -= dnorm(ztrans(0), mu, pow(sigma_sq/(Type(2.0)*beta), Type(0.5)), true);
   
   // For other steps the process model is:
   for(int i = 0; i < (Nz - 1); i++){
      
      nll -= nll_OU(ztrans(i+1), ztrans(i), dt(i), beta, mu, sigma_sq);
      
   }
   
   // OBSERVATION MODEL
   for(int i = 0; i < Nz; i++){
      
      nll -= dnorm(err(i), Type(0.0), sig_err, true);
      
   }
   
   
   
   // REPORTS
   ADREPORT(sigma);
   REPORT(sigma);
   ADREPORT(beta);
   REPORT(beta);
   ADREPORT(mu);
   REPORT(mu);
   ADREPORT(sig_err);
   REPORT(sig_err);
   ADREPORT(zlat);
   REPORT(zlat);
   
   return nll;
   
}

