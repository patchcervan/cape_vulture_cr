#include <Rcpp.h>
using namespace Rcpp;

// This function calculates the minimum distance between a matrix
// of locations r and another matrix of locations x

// [[Rcpp::export]]
NumericVector minDist_cpp(NumericMatrix r, NumericMatrix x) {
    int n = r.nrow();
    int m = x.nrow();
    
    NumericVector d(n);
    
    for(int i = 0; i < n; ++i) {
        NumericVector cc = r(i,_);
        NumericVector dd(m);
        
        for(int j = 0; j < m; ++j){
            dd[j] = sqrt(sum(pow(cc - x(j,_), 2.0)));
        }
        
        d[i] = min(dd);
    }
    return d;
}
