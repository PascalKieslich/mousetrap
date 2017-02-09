#include <Rcpp.h>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// [[Rcpp::export]]
NumericVector getVelocity(NumericVector x, NumericVector y) {
  double xd, yd;
  NumericVector Velocity(x.length());
  Velocity[0] = 0;
  for(int i = 1; i < x.length(); i++){
  	if(x[i-1] != x[i-1] || x[i] != x[i]) continue;
    xd = x[i] - x[i-1];
    yd = y[i] - y[i-1];
    Velocity[i] = sqrt(xd*xd + yd*yd);
    }
  return Velocity;
  }


// [[Rcpp::export]]
NumericVector getVelocity3d(NumericVector x, NumericVector y, NumericVector z) {
  double xd, yd, zd;
  NumericVector Velocity(x.length());
  Velocity[0] = 0;
  for(int i = 1; i < x.length(); i++){
    if(x[i-1] != x[i-1] || x[i] != x[i]) continue;
    xd = x[i] - x[i-1];
    yd = y[i] - y[i-1];
    zd = z[i] - z[i-1];
    Velocity[i] = sqrt(xd*xd + yd*yd + zd*zd);
  }
  return Velocity;
}



// [[Rcpp::export]]
NumericMatrix getVelocities(NumericMatrix xs, NumericMatrix ys) {
  NumericVector x(xs.ncol()), y(ys.ncol());
  NumericMatrix Velocities(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    Velocities(i,_) = getVelocity(x,y);
    }
  return Velocities;
  }


// [[Rcpp::export]]
NumericMatrix getVelocities3d(NumericMatrix xs, NumericMatrix ys, NumericMatrix zs) {
  NumericVector x(xs.ncol()), y(ys.ncol()), z(zs.ncol());
  NumericMatrix Velocities(xs.nrow(),xs.ncol());
  for(int i = 0; i < xs.nrow(); i++){
    x = xs(i,_);
    y = ys(i,_);
    z = zs(i,_);
    Velocities(i,_) = getVelocity3d(x,y,z);
  }
  return Velocities;
}

