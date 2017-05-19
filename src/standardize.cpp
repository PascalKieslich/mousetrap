#include <Rcpp.h>
using namespace Rcpp;

// Calculate standard deviation of matrix
// [[Rcpp::export]]
double sd_mat(NumericMatrix mat){
  int ind = 0, n = mat.nrow(), m = mat.ncol();
  double mean = 0, sd = 0;
  int i, j;
  if(n > m){
    for(i = 0; i < n; ++i){
      for(j = 0; j < m; ++j){
        if(mat(i,j) == mat(i,j)){
          ind++;
          mean += mat(i,j);
          sd += mat(i,j) * mat(i,j);
          }
        }
      }
    } else {
    for(j = 0; j < m; ++j){
      for(i = 0; i < n; ++i){
        if(mat(i,j) == mat(i,j)){
          ind++;
          mean += mat(i,j);
          sd += mat(i,j) * mat(i,j);
          }
        }
      }    
    }
  mean = mean / ind;
  return sqrt((sd - ind * mean * mean) / (ind - 1));
  }

// Calculate standard deviation of matrix
// [[Rcpp::export]]
NumericVector moments_mat(NumericMatrix mat){
  int ind = 0, n = mat.nrow(), m = mat.ncol();
  double mean = 0, sd = 0;
  int i, j;
  if(n > m){
    for(i = 0; i < n; ++i){
      for(j = 0; j < m; ++j){
        if(mat(i,j) == mat(i,j)){
          ind++;
          mean += mat(i,j);
          sd += mat(i,j) * mat(i,j);
          }
        }
      }
    } else {
    for(j = 0; j < m; ++j){
      for(i = 0; i < n; ++i){
        if(mat(i,j) == mat(i,j)){
          ind++;
          mean += mat(i,j);
          sd += mat(i,j) * mat(i,j);
          }
        }
      }    
    }
  mean = mean / ind;
  NumericVector moments(2);
  moments[0] = mean;
  moments[1] = sqrt((sd - ind * mean * mean) / (ind - 1));
  return moments;
  }


// standardize each row to mean = 0 and sd = 1
// [[Rcpp::export]]
NumericVector scale_rows(NumericMatrix mat, bool center = true, bool scale = true) {
  int n = mat.nrow(), m = mat.ncol();
  NumericMatrix n_mat(n, m); 
  for(int i = 0; i < n; ++i){
    double mean = 0, sd = 0;
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
    sd   = sqrt((sd - ind * mean * mean)/(ind - 1));
    if(center == true) vec = vec - mean;
    if(scale == true) vec = vec / sd;
    n_mat(i,_) = vec;
    }
  return n_mat;
  }

// standardize rows to mean = 0 and sd = scale
// [[Rcpp::export]]
NumericVector trans_rows(NumericMatrix mat, double scale, bool center = true) {
    int n = mat.nrow(), m = mat.ncol();
    NumericMatrix n_mat(n, m); 
    for(int i = 0; i < n; ++i){
      double mean = 0, sd = 0;
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
      sd   = sqrt((sd - ind * mean * mean)/(ind - 1));
      if(center == true) vec = vec - mean;
      vec = (vec / sd) * scale;
      n_mat(i,_) = vec;
      }
    return n_mat;
    }
	
	
// [[Rcpp::export]]
NumericMatrix scale_mat(NumericMatrix mat, bool center = true, bool scale = true){
  NumericMatrix n_mat = clone(mat);
  int n = mat.nrow(), m = mat.ncol();
  int i, j;
  NumericVector moments = moments_mat(mat);
  if(n > m){
    for(i = 0; i < n; ++i){
      for(j = 0; j < m; ++j){
        if(n_mat(i,j) == n_mat(i,j)){
          if(center == true) n_mat(i,j) -= moments[0];
          if(scale == true) n_mat(i,j) /= moments[1];
          }
        }
      }
    } else {
    for(j = 0; j < m; ++j){
      for(i = 0; i < n; ++i){
        if(n_mat(i,j) == n_mat(i,j)){
          if(center == true) n_mat(i,j) -= moments[0];
          if(scale == true) n_mat(i,j) /= moments[1];   
          }
        }
      }
    }
  return n_mat;
  }

// [[Rcpp::export]]
NumericMatrix trans_mat(NumericMatrix mat, double scale, bool center = true){
  NumericMatrix n_mat = clone(mat);
  int n = mat.nrow(), m = mat.ncol();
  int i, j;
  NumericVector moments = moments_mat(mat);
  if(n > m){
    for(i = 0; i < n; ++i){
      for(j = 0; j < m; ++j){
        if(n_mat(i,j) == n_mat(i,j)){
          if(center == true) n_mat(i,j) -= moments[0];
          n_mat(i,j) /= (moments[1]/scale);
          }
        }
      }
    } else {
    for(j = 0; j < m; ++j){
      for(i = 0; i < n; ++i){
        if(n_mat(i,j) == n_mat(i,j)){
          if(center == true) n_mat(i,j) -= moments[0];
          n_mat(i,j) /= (moments[1]/scale); 
          }
        }
      }
    }
  return n_mat;
  }




