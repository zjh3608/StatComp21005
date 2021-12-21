#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix CCC (double a, double b, int n, int m, double X1) {
  NumericMatrix X(m, 2);
  X(0,1) = X1; 
  for (int i = 1; i < m;i++ ) {
    X(i,0) = rbinom(1, n, X(i-1,1))[0];
    X(i,1) = rbeta(1, X(i,0)+a, n-X(i,0)+b)[0];
  }
  return(X);
} 