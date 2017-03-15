#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector select_max(NumericVector x, NumericVector y) {
  int n = x.size();
  NumericVector res(n);
  for(int i = 0; i < n; ++i){
    if(x[i] > y[i]){
      res[i] = x[i];
      } else {
      res[i] = y[i];      
      }
    }
  return res;
  }


