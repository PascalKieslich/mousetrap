#include <Rcpp.h>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// [[Rcpp::export]]
NumericVector getAngleP(NumericVector x, NumericVector y) {
  NumericVector angles(x.length());
  double len12, len13, len23, len12x, len12y, len13x, len13y, len23x, len23y, tmp, angle; 
  int n = x.length();
  angles[0] = -90;
  for(int i = 1; i < n - 1; i++){
    if(x[i-1] != x[i-1] || x[i] != x[i] || x[i+1] != x[i+1]){
      angles[i] = -90;
      continue;
      }
    len12x = (x[i-1] -   x[i]);
    len12y = (y[i-1] -   y[i]);   
    len13x = (x[i-1] -   x[i+1]);
    len13y = (y[i-1] -   y[i+1]);
    len23x = (x[i] - x[i+1]);
    len23y = (y[i] - y[i+1]);
    len12 = len12x*len12x+len12y*len12y;
    len13 = len13x*len13x+len13y*len13y;
    len23 = len23x*len23x+len23y*len23y;
    if(len12 > 0 && len23 > 0){
      tmp = (len12+len23-len13)/std::pow(4*len12*len23,.5);
      if(tmp >  1) tmp = 1;
      if(tmp < -1) tmp = -1;
      angle = std::acos(tmp);
      angles[i] = angle;
      } else {
      angles[i] = -100;      
      }
    }
  angles[n-1] = -90;
  return angles;
  }


// [[Rcpp::export]]
NumericVector getAngleV(NumericVector x, NumericVector y) {
  NumericVector angles(x.length());
  double len12, len13, len23, len12x, len12y, len13x, len13y, len23x, len23y, tmp, angle; 
  angles[0] = -90;
  for(int i = 1; i < x.length(); i++){
    if(x[i-1] != x[i-1] || x[i]   != x[i]){
      angles[i] = -90;
      continue;
      }
    len12x = 0;
    len12y = 1;   
    len13x = (x[i-1]     -   x[i]);
    len13y = ((y[i-1]+1) -   y[i]);
    len23x = (x[i-1] -       x[i]);
    len23y = (y[i-1] -       y[i]);
    len12 = len12x*len12x+len12y*len12y;
    len13 = len13x*len13x+len13y*len13y;
    len23 = len23x*len23x+len23y*len23y;
    if(len12 > 0 && len23 > 0){
      tmp = (len12+len23-len13)/std::pow(4*len12*len23,.5);
      if(tmp >  1) tmp = 1;
      if(tmp < -1) tmp = -1;
      angle = std::acos(tmp);
      if(len23x > 0){
        angles[i] = angle;
        } else {
        angles[i] = -angle;
        }
      } else {
      angles[i] = -100;      
      }
    }
  return angles;
}

// [[Rcpp::export]]
NumericMatrix getAnglesP(NumericMatrix xs, NumericMatrix ys){
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix angleMat(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    angleMat(i,_) = getAngleP(x,y);
    }
  return angleMat;
  }

// [[Rcpp::export]]
NumericMatrix getAnglesV(NumericMatrix xs, NumericMatrix ys){
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix angleMat(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    angleMat(i,_) = getAngleV(x,y);
    }
  return angleMat;
  }
