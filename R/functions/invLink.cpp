#include <Rcpp.h>
using namespace Rcpp;

// This function calculates the minimum distance between a matrix
// of locations r and another matrix of locations x

// [[Rcpp::export]]
NumericVector invLink(NumericVector zprime, double b) {
    
    int Nz = zprime.size();
    
    NumericVector z(Nz);
    
    for(int i = 0; i < Nz; i++){
        if(zprime(i) >= b){
            z(i) = zprime(i);
        } else {
            z(i) = b*tanh(zprime(i)/b-1) + b;
        }
    }
    
    return z ;
}
