#include <Rcpp.h>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// [[Rcpp::export]]
NumericMatrix cleanAngles(NumericMatrix as) {
  int n = as.nrow();
  for(int i = 0; i < n; i++){
    NumericVector traj = as(i,_);
    int np = traj.length();
    for(int j = 0; j < np; j++){
      int pt = traj[j];
      if(pt == -100){
        for(int k = 0; k < traj.length(); k++){
          double npt = traj[k];
          if(int(npt) != -100 && int(npt) != -90){
            traj[j] = npt;
            break;
            }
          }
        }
      }
      as(i,_) = traj;
    }
  return 0;
  }

