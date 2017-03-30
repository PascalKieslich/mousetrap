#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector zstandardize(NumericMatrix mat, bool center, bool scale) {
  int n = mat.nrow(), m = mat.ncol();
  NumericMatrix n_mat(n, m); 
  int mean = 0, sd = 0;
  for(int i = 0; i < n; ++i){
    NumericVector vec = mat(i,_);
    int ind = 0;
    for(int j = 0; j < m; ++j){
      double v = vec[j];
      if(v == v){
        ind++;
        mean += v;
        sd += v * v;
        }
      }
    mean = mean/ind;
    sd   =  sqrt(sd/ind - mean * mean);
    for(int j = 0; j < ind; ++j){
      double v = mat(i,j);
      if(center == true) v -= mean;
      if(scale == true) v /= sd;
      n_mat(i,j) = v;
      }
    }
  return n_mat;
  }







