#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getlast(NumericMatrix trajectories) {
  int nr = trajectories.nrow();
  NumericVector last(nr);
  for(int i = 0; i < nr; ++i){
    NumericVector row = trajectories(i,_);
    int nc = row.length();
    for(int j = nc - 1; j > -1; --j){
      double v = row[j];
      if(v==v){
        last[i] = row[j];
        break;
        }
      }
    }
  return last;
  }

