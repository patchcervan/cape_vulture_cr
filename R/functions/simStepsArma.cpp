#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
// Define distance function
NumericVector calcDist(NumericVector s, NumericVector x, NumericVector y) {
   int n = x.length();
   NumericVector d(n);
   
   d = sqrt(pow(s[1] - x, 2) + pow(s[2] - y, 2));
   return d;
}

// [[Rcpp::export]]
NumericMatrix simSteps(NumericVector coords0, NumericVector lonlat0, NumericMatrix coords, NumericMatrix lonlat,
                       NumericMatrix whab, int age, NumericVector sl_coefs,
                       int nsteps, double shape, double scale, double model_sc) 
{
   
   // Create a matrix to store simulations
   NumericMatrix sims(nsteps, 6);
   
   // Extract coordinates
   NumericVector x = coords( _ , 0);
   NumericVector y = coords( _ , 1);

   NumericVector lon = lonlat( _ , 0);
   NumericVector lat = lonlat( _ , 1);
   
   // Set initial point at colony in the morning
   double lon0 = lonlat0(0);
   double lat0 = lonlat0(1);

   // Initial location (important to keep track of projection)
   NumericVector x0(2);
   x0(0) = coords0(0);
   x0(1) = coords0(1);
   
   // Calculate distance from colony
   NumericVector dist_col = calcDist(x0, x, y);

   // Initialize step variables
   double dcol = 0.0;
   double ttnoon = -7;
   double ttnoon_sq = pow(ttnoon, 2);
   int trip = 0;
   int dur = 0;
   // bool atcol = TRUE;
   
   //Aux
   // NumericMatrix M(nsteps, x.size());
   IntegerVector idx;
   int nsls = x.size();
   NumericVector w(nsls);
   NumericMatrix sls_correct(nsls, 5);
   
   for(int i=0; i<nsteps; i++){

      sims(i,0) = lon0;
      sims(i,1) = lat0;
      sims(i,2) = dcol;
      sims(i,3) = ttnoon;
      sims(i,4) = trip;
      sims(i,5) = dur;

      NumericVector sls = calcDist(x0, x, y);
      
      NumericVector wgamma = dgamma(sls, shape, scale);
      
      // Apply model corrections for time of day (see names(sl_coefs))
      sls = sls/model_sc;
      
      sls_correct( _, 0) = sls*sl_coefs(0);
      sls_correct( _, 1) = sls*ttnoon*sl_coefs(1);
      sls_correct( _, 2) = sls*ttnoon_sq*sl_coefs(2);
      sls_correct( _, 3) = sls*sl_coefs(3);
      sls_correct( _, 4) = sls*age*sl_coefs(4);
      
      // sls_correct <- cbind(sls, sls*ttnoon, sls*ttnoon_sq, sls, sls) %*% sl_coefs
      
      w = wgamma * whab( ttnoon+7 , _ );
      idx = sample(nsls, 1, true, w, false);
      
      lon0 = lon(idx(0));
      lat0 = lat(idx(0));
      x0(0) = x(idx(0));
      x0(1) = y(idx(0));
      dcol = dist_col(idx(0));
      ttnoon += 1;
      
      // M(i,_) = wgamma;
      //
      if(dcol > 5000){
         dur += 1;
      } else {
         dur = 0;
         // atcol = TRUE;
      }
      
      if(dur == 1){
         trip += trip;
      }
      
      if(ttnoon > 7){
         
         ttnoon = -7;
 
         // if(!atcol && rbinom(1, 1, pback[floor(dur/15)])){
         //    state <- c(.col_sel$lon, .col_sel$lat)
         //    state_proj <- c(0,0)
         //    dur <- 0
         //    dist_col <- 0
         // }
         
         // atcol = FALSE;
      }
      
      ttnoon_sq = pow(ttnoon, 2);

   }
   
   return sims;
   
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R

ncells <- 1000
coords0 <- c(0, 0)
lonlat0 <- c(23.5, 15.2)
coords <- cbind(runif(ncells, 0.1, 1000), runif(ncells, 0.1, 1000))
lonlat <- cbind(runif(ncells, 0, 10), runif(ncells, 10, 20))
nsteps <- 1000
shape <- 0.2
scale <- 20
model_sc <- 23.5
age_fct <- "juv"
age <- ifelse(age_fct == "juv", 1, 0)
sl_coefs <- rnorm(5, 0, 1)
whab <- matrix(runif(nrow(coords)*15, 0.1, 1), nrow = 15)

simSteps(coords0, lonlat0, coords, lonlat, whab, age, sl_coefs, nsteps, shape, scale, model_sc)


# Diagnose

# Define distance function
# calcDist <- function(s, x, y){
#    sqrt((s[1] - x)^2 + (s[2] - y)^2)
# }
# 
# sls <- calcDist(coords0, coords[,1], coords[,2] )
# dgamma(sls, shape=shape, scale=scale)


*/
