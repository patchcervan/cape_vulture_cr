#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
// Define distance function
NumericVector calcDist(NumericVector s, NumericVector x, NumericVector y) {
   int n = x.length();
   NumericVector d(n);
   
   d = sqrt(pow(s[1] - x, 2) + pow(s[2] - y, 2));
   return d;
}

// [[Rcpp::export]]
NumericMatrix simSteps(NumericVector x0, NumericVector lonlat0, NumericMatrix coords, NumericMatrix lonlat,
                       DataFrame X, NumericMatrix coefs, double shape, double scale, double mov_sd,
                       int nsteps, bool juv, int res) {
   
   // Create a matrix to store simulations
   NumericMatrix sims(nsteps, 4);
   
   // Create armadillo coefficient matrix
   arma::mat beta = as<arma::mat>(coefs);
   
   // Compute number of cases
   int N = coords.nrow();
   
   // Extract coordinates
   NumericVector x = coords( _ , 0);
   NumericVector y = coords( _ , 1);
   
   NumericVector lon = lonlat( _ , 0);
   NumericVector lat = lonlat( _ , 1);
   
   // Create references to data frame
   NumericVector sl_ = X["sl_"];
   NumericVector sl_res = X["sl_:res"];
   NumericVector sl_ttnoon = X["sl_:ttnoon"];
   NumericVector sl_ttnoon_sq = X["sl_:ttnoon_sq"];
   NumericVector dist_col_ttnoon = X["dist_col:ttnoon"];
   NumericVector dist_col_ttnoon_sq = X["dist_col:ttnoon_sq"];
   NumericVector dist_col_juv_ttnoon = X["dist_col:juv:ttnoon"];
   NumericVector dist_col_juv_ttnoon_sq = X["dist_col:juv:ttnoon_sq"];
   // Save distance to colony so that it doesn´t change through iterations
   // NumericVector dist_col = clone(X[1]);
   NumericVector dist_col = X["dist_col"];
   
   // Initialize time of day
   int ttnoon = -7;
   double ttnoon_sq = pow(ttnoon, 2);
   int trip = 1;
   IntegerVector ccs(N);
   
   for(int i=0; i<nsteps; i++){

      sims(i,0) = lonlat0(0);
      sims(i,1) = lonlat0(1);
      sims(i,2) = ttnoon;
      sims(i,3) = trip;
      
      NumericVector sls = calcDist(x0, x, y);
      for(int i=0; i<N; i++){
         if(sls(i) == 0){
            sls(i) = 1000;
         }
         ccs(i) = i;
      }
      NumericVector wgamma = dgamma(sls, shape, scale);
      
      // Apply model corrections for time of day
      sl_ = sls/mov_sd;
      sl_res = sl_*res;
      sl_ttnoon = sl_*ttnoon;
      sl_ttnoon_sq = sl_*ttnoon_sq;
      dist_col_ttnoon = dist_col*ttnoon;
      dist_col_ttnoon_sq = dist_col*ttnoon_sq;
      dist_col_juv_ttnoon = dist_col*ttnoon*juv;
      dist_col_juv_ttnoon_sq = dist_col*ttnoon_sq*juv;
      
      // Predict
      arma::mat mX = as<arma::mat>(internal::convert_using_rfunction(X, "as.matrix")) ;
      arma::vec sel_values = exp(mX * beta);
      NumericVector sv = wrap(sel_values);
      
      NumericVector ws = wgamma * sv;
      double total_w = sum(ws);
      NumericVector ps = ws/total_w;
      
      IntegerVector cc = sample(N, 1, true, ps, false);
      
      x0(0) = x(cc(0));
      x0(1) = y(cc(0));
      lonlat0(0) = lon(cc(0));
      lonlat0(1) = lat(cc(0));
      ccs(i) = cc(0);
      ttnoon += 1;
      if(ttnoon > 7){
         ttnoon = -7;
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
s <- c(0,0); x = c(1,2); y = c(1,2)
X <- cbind(x, y, c(0,0), c(0,0))
X <- data.frame(x = x, y = y, sl_= c(1, 2), hab = 0, hab2 = 1)

calcDist(s, x, y)
simSteps(c(0, 0), c(col_sel$lon, col_sel$lat), coords, lonlat, 
         habcpp, matrix(ssf_coef, ncol = 1), mov_ker["shape"], mov_ker["scale"], mov_ker["model_sc"],
         100, FALSE, 1)
*/
