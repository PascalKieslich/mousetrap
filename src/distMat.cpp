#include <Rcpp.h>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// [[Rcpp::export]]
NumericMatrix distMat(NumericMatrix x,
                      NumericMatrix y,
                      int power = 2) {
  int ni = x.ncol(), nt = x.nrow();
  double xd,yd,d = 0;
  NumericMatrix dist(nt,nt);
  for(int r = 0; r < nt; r++) {     // loop rows
    for(int c = r; c < nt; c++) {   // loop cols (only half due to symmetry)
      d = 0;                        //dummy value of final matrix entry
      if(power == 1){
      for(int i = 0; i < ni; i++) { // loop 1:n
        xd = std::abs(x(r,i) - x(c,i));
        yd = std::abs(y(r,i) - y(c,i));
        d += xd + yd;
        }
      } else if (power == 2){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = x(r,i) - x(c,i);
          yd = y(r,i) - y(c,i);
          d += sqrt( xd*xd  + yd*yd );    
          }
      } else {
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          for(int j = 1; j < power; j++){
            xd *= xd;
            yd *= yd;
            }
          d += exp( log(xd + yd) / double(power));
          }
        }
      dist(r,c) = d;   // fill distance matrix
      dist(c,r) = d;   // fill distance matrix
      }  
    }
  return dist;
  }

// [[Rcpp::export]]
NumericMatrix distMatV(NumericMatrix x,
                      NumericMatrix y,
                      int power = 2) {
  int ni = x.ncol(), nt = x.nrow();
  double xd,yd,d = 0;
  NumericMatrix dist(nt,nt);
  for(int r = 0; r < nt; r++) {     // loop rows
    for(int c = r; c < nt; c++) {   // loop cols (only half due to symmetry)
      d = 0;                        //dummy value of final matrix entry
      if(power == 1){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          d += xd + yd;
        }
      } else if (power == 2){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = x(r,i) - x(c,i);
          yd = y(r,i) - y(c,i);
          d += xd*xd  + yd*yd;    
        }
      } else {
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          for(int j = 1; j < power; j++){
            xd *= xd;
            yd *= yd;
          }
          d += xd + yd;
        }
      }
	  if(power == 2){
        d = sqrt(d);
        } else if(power > 2){
        d = exp( log(d) / double(power));
        }
      dist(r,c) = d;   // fill distance matrix
      dist(c,r) = d;   // fill distance matrix
    }  
  }
  return dist;
}

// [[Rcpp::export]]
NumericMatrix distMat3d(NumericMatrix x,
                         NumericMatrix y,
                         NumericMatrix z,
                         int power = 2) {
  int ni = x.ncol(), nt = x.nrow();
  double xd,yd,zd,d = 0;
  NumericMatrix dist(nt,nt);
  for(int r = 0; r < nt; r++) {     // loop rows
    for(int c = r; c < nt; c++) {   // loop cols (only half due to symmetry)
      d = 0;                        //dummy value of final matrix entry
      if(power == 1){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          zd = std::abs(z(r,i) - z(c,i));
          d += xd + yd + zd;
        }
      } else if (power == 2){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = x(r,i) - x(c,i);
          yd = y(r,i) - y(c,i);
          zd = z(r,i) - z(c,i);
          d += sqrt(xd*xd  + yd*yd + zd*zd);    
        }
      } else {
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          zd = std::abs(z(r,i) - z(c,i));
          for(int j = 1; j < power; j++){
            xd *= xd;
            yd *= yd;
            zd *= zd;
          }
          d += exp( log(xd + yd + zd) / double(power));
        }
      }
      dist(r,c) = d;   // fill distance matrix
      dist(c,r) = d;   // fill distance matrix
    }  
  }
  return dist;
}

// [[Rcpp::export]]
NumericMatrix distMat3dV(NumericMatrix x,
                       NumericMatrix y,
                       NumericMatrix z,
                       int power = 2) {
  int ni = x.ncol(), nt = x.nrow();
  double xd,yd,zd,d = 0;
  NumericMatrix dist(nt,nt);
  for(int r = 0; r < nt; r++) {     // loop rows
    for(int c = r; c < nt; c++) {   // loop cols (only half due to symmetry)
      d = 0;                        //dummy value of final matrix entry
      if(power == 1){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          zd = std::abs(z(r,i) - z(c,i));
          d += xd + yd + zd;
        }
      } else if (power == 2){
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = x(r,i) - x(c,i);
          yd = y(r,i) - y(c,i);
          zd = z(r,i) - z(c,i);
          d += xd*xd  + yd*yd + zd*zd ;    
        }
      } else {
        for(int i = 0; i < ni; i++) { // loop 1:n
          xd = std::abs(x(r,i) - x(c,i));
          yd = std::abs(y(r,i) - y(c,i));
          zd = std::abs(z(r,i) - z(c,i));
          for(int j = 1; j < power; j++){
            xd *= xd;
            yd *= yd;
            zd *= zd;
          }
          d += xd + yd + zd;
        }
      }
      if(power == 2){
        d = sqrt(d);
      } else if(power > 2){
        d = exp( log(d) / double(power));
      }
      dist(r,c) = d;   // fill distance matrix
      dist(c,r) = d;   // fill distance matrix
    }  
  }
  return dist;
}

