#include <Rcpp.h>
#include <string>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// spatial rescale
//
// basic 2D function to rescale trajectories
// used in all 2D functions below

// [[Rcpp::export]]
NumericMatrix spatialize(NumericVector x, NumericVector y, int npts) {
  int ind = 0, n = x.size();
  for(int i = 0; i < n; i++){
    if(x[i] == x[i] || y[i] == y[i]) ind++;
    }
  n = ind;
  NumericVector cumdiffs(n), steps(npts);
  NumericMatrix xyn(npts,2);
  double step, w1, w2, stepi, cumdiffi;
  // Calculate cumulative distances between points
  for(int i = 0; i < n; i++){
    if(i < 1){
      cumdiffs[i] = 0.0;
      } else {
      cumdiffs[i] = cumdiffs[i-1] + sqrt(pow(x[i] - x[i-1],2) + pow(y[i] - y[i-1],2));
      }
    }
  // Calculate vector with equidistant steps
  step = double(cumdiffs[n-1]) / double(npts-1);
  for(double i = 0; i < npts; i++){
    steps[i] = step * i;
    }
  // Loop over number of points for final
  for(int i = 0; i < npts; i++){
    ind = 0;
    for(int j = 0; j < n; j++){
      stepi    = steps[i];
      cumdiffi = cumdiffs[j];
      if(stepi > cumdiffi) ind++;
      }
    if(i != (npts-1) && i != 0){
      w1 = std::abs(double(steps[i]) - double(cumdiffs[ind-1]));
      w2 = std::abs(double(steps[i]) - double(cumdiffs[ind]));
      double w  =  w2/(w1+w2);
      xyn(i,0) = double(x[ind-1]) * w + double(x[ind]) * (1-w);
      xyn(i,1) = double(y[ind-1]) * w + double(y[ind]) * (1-w);
      }
    else if(i == 0){
      xyn(i,0) = double(x[0]);
      xyn(i,1) = double(y[0]);
      }
    else {
      xyn(i,0) = double(x[n-1]);
      xyn(i,1) = double(y[n-1]);
      }
    }
  return xyn;
  }


// spatial rescale 3d
//
// basic 3D function to rescale trajectories
// used in all 3D functions below



// [[Rcpp::export]]
NumericMatrix spatialize3d(NumericVector x, NumericVector y, NumericVector z, int npts) {
  int ind = 0, n = x.size();
  for(int i = 0; i < n; i++){
    if(x[i] == x[i] || y[i] == y[i]) ind++;
    }
  n = ind;
  NumericVector cumdiffs(n), steps(npts);
  NumericMatrix xyn(npts,3);
  double step, w1, w2, stepi, cumdiffi;
  // Calculate cumulative distances between points
  for(int i = 0; i < n; i++){
    if(i < 1){
      cumdiffs[i] = 0.0;
      } else {
      cumdiffs[i] = cumdiffs[i-1] + sqrt(pow(x[i] - x[i-1],2) + pow(y[i] - y[i-1],2));
      }
    }
  // Calculate vector with equidistant steps
  step = double(cumdiffs[n-1]) / double(npts-1);
  for(double i = 0; i < npts; i++){
    steps[i] = step * i;
    }
  // Loop over number of points for final
  for(int i = 0; i < npts; i++){
    ind = 0;
    for(int j = 0; j < n; j++){
      stepi    = steps[i];
      cumdiffi = cumdiffs[j];
      if(stepi > cumdiffi) ind++;
      }
    if(i != (npts-1) && i != 0){
      w1 = std::abs(double(steps[i]) - double(cumdiffs[ind-1]));
      w2 = std::abs(double(steps[i]) - double(cumdiffs[ind]));
      double w  =  w2/(w1+w2);
      xyn(i,0) = double(x[ind-1]) * w + double(x[ind]) * (1-w);
      xyn(i,1) = double(y[ind-1]) * w + double(y[ind]) * (1-w);
      xyn(i,2) = double(z[ind-1]) * w + double(z[ind]) * (1-w);
      }
    else if(i == 0){
      xyn(i,0) = double(x[0]);
      xyn(i,1) = double(y[0]);
      xyn(i,2) = double(z[0]);
      }
    else {
      xyn(i,0) = double(x[n-1]);
      xyn(i,1) = double(y[n-1]);
      xyn(i,2) = double(z[n-1]);
      }
    }
  return xyn;
  }


// spatial rescale 4d
//
// basic 4D function to rescale trajectories
// used in all 4D functions below



// [[Rcpp::export]]
NumericMatrix spatialize4d(NumericVector x, 
                           NumericVector y, 
                           NumericVector z1, 
                           NumericVector z2, 
                           int npts) {
  int ind = 0, n = x.size();
  for(int i = 0; i < n; i++){
    if(x[i] == x[i] || y[i] == y[i]) ind++;
    }
  n = ind;
  NumericVector cumdiffs(n), steps(npts);
  NumericMatrix xyn(npts,4);
  double step, w1, w2, stepi, cumdiffi;
  // Calculate cumulative distances between points
  for(int i = 0; i < n; i++){
    if(i < 1){
      cumdiffs[i] = 0.0;
      } else {
      cumdiffs[i] = cumdiffs[i-1] + sqrt(pow(x[i] - x[i-1],2) + pow(y[i] - y[i-1],2));
      }
    }
  // Calculate vector with equidistant steps
  step = double(cumdiffs[n-1]) / double(npts-1);
  for(double i = 0; i < npts; i++){
    steps[i] = step * i;
    }
  // Loop over number of points for final
  for(int i = 0; i < npts; i++){
    ind = 0;
    for(int j = 0; j < n; j++){
      stepi    = steps[i];
      cumdiffi = cumdiffs[j];
      if(stepi > cumdiffi) ind++;
      }
    if(i != (npts-1) && i != 0){
      w1 = std::abs(double(steps[i]) - double(cumdiffs[ind-1]));
      w2 = std::abs(double(steps[i]) - double(cumdiffs[ind]));
      double w  =  w2/(w1+w2);
      xyn(i,0) = double(x[ind-1]) * w + double(x[ind]) * (1-w);
      xyn(i,1) = double(y[ind-1]) * w + double(y[ind]) * (1-w);
      xyn(i,2) = double(z1[ind-1]) * w + double(z1[ind]) * (1-w);
      xyn(i,3) = double(z2[ind-1]) * w + double(z2[ind]) * (1-w);
      }
    else if(i == 0){
      xyn(i,0) = double(x[0]);
      xyn(i,1) = double(y[0]);
      xyn(i,2) = double(z1[0]);
      xyn(i,3) = double(z2[0]);
      }
    else {
      xyn(i,0) = double(x[n-1]);
      xyn(i,1) = double(y[n-1]);
      xyn(i,2) = double(z1[n-1]);
      xyn(i,3) = double(z2[n-1]);
      }
    }
  return xyn;
  }


// spatial rescale (A)rray
//
// takes two matrixes as input and returns a list of two matrices
// containing rescaled versions of the input matrices


// [[Rcpp::export]]
GenericVector spatializeArray(NumericMatrix xs,
                              NumericMatrix ys,
                              NumericVector n_pts){
  GenericVector xy(2);
  NumericVector n_pts_v(xs.nrow());
  int max_pts = 0, n = xs.nrow();
  if(n_pts.length() != n){
    for(int i = 0; i < n; i++){
      n_pts_v[i] = n_pts[0];
      max_pts = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    for(int i = 0; i < n; i++){
      if(n_pts[i] > max_pts) max_pts = n_pts[i];
      }
    }
  NumericMatrix nxs(n, max_pts), nys(n, max_pts);
  for(int i = 0; i < n; i++){
    NumericMatrix resc_traj = spatialize(xs(i,_), ys(i,_), n_pts_v[i]);
    for(int j = 0; j < max_pts; j++){
      int tn = resc_traj.nrow();
      if(j < tn){
        nxs(i, j) = resc_traj(j,0);
        nys(i, j) = resc_traj(j,1);
        } else {
        nxs(i, j) = -10000;
        nys(i, j) = -10000;
        }
      }
    }
  xy[0] = nxs;
  xy[1] = nys;
  return xy;
}


// spatial rescale (A)rray long
//
// takes two matrixes as input and returns a Matrix
// containing the values of the input matrices in one
// columns each.


// [[Rcpp::export]]
NumericMatrix spatializeArrayToLong(NumericMatrix xs,
                                    NumericMatrix ys,
                                    NumericVector n_pts){
  int n = xs.nrow();
  NumericVector n_pts_v(n);
  long total_pts = 0;

  if(n_pts.length() != n){
    for(int i = 0; i < n; i++){
      total_pts += n_pts[0];
      n_pts_v[i] = n_pts[0];
      }
    } else {
    for(int i = 0; i < n_pts.size(); i++){
      total_pts += n_pts[i];
      }
    n_pts_v = n_pts;
    }
  if(total_pts > std::pow(2,31)-1){
    std::string a = "Error: Matrix bound reached - use smaller resolution!";
    return 0;
    };
  NumericMatrix xy(total_pts, 2);
  int ind = 0;
  for(int i = 0; i < n; i++){
    NumericMatrix resc_traj = spatialize(xs(i,_), ys(i,_), n_pts_v[i]);
    int tn = resc_traj.nrow();
    for(int j = 0; j < tn; j++){
      xy(ind, 0) = resc_traj(j,0);
      xy(ind, 1) = resc_traj(j,1);
      ind++;
      }
    }
  return xy;
  }


// spatial rescale (A)rray 3D
//
// takes two matrixes as input and returns a list of two matrices
// containing rescaled versions of the input matrices


// [[Rcpp::export]]
GenericVector spatializeArray3d(NumericMatrix xs,
                                NumericMatrix ys,
                                NumericMatrix zs,
                                NumericVector n_pts){
  GenericVector xyz(3);
  int max_pts = 0, n = xs.nrow();
  NumericVector n_pts_v(n);
  if(n_pts.length() != n){
    for(int i = 0; i < n; i++){
      n_pts_v[i] = n_pts[0];
      max_pts = n_pts[0];
      }
    } else {
    n_pts_v = n_pts;
    for(int i = 0; i < xs.nrow(); i++){
      if(n_pts[i] > max_pts) max_pts = n_pts[i];
      }
    }
  NumericMatrix nxs(n, max_pts);
  NumericMatrix nys(n, max_pts);
  NumericMatrix nzs(n, max_pts);
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialize3d(xs(i,_), ys(i,_), zs(i,_), n_pts_v[i]);
    for(int j = 0; j < max_pts; j++){
      int tn = resc_traj.nrow();
      if(j < tn){
        nxs(i, j) = resc_traj(j,0);
        nys(i, j) = resc_traj(j,1);
        nzs(i, j) = resc_traj(j,2);
        } else {
        nxs(i, j) = -10000;
        nys(i, j) = -10000;
        nzs(i, j) = -10000;
        }
      }
    }
  xyz[0] = nxs;
  xyz[1] = nys;
  xyz[2] = nzs;
  return xyz;
  }


// spatial rescale (A)rray long 3D
//
// takes two matrixes as input and returns a Matrix
// containing the values of the input matrices in one
// columns each.

// [[Rcpp::export]]
NumericMatrix spatializeArrayToLong3d(NumericMatrix xs,
                                      NumericMatrix ys,
                                      NumericMatrix zs,
                                      NumericVector n_pts){
  NumericVector n_pts_v(xs.nrow());
  long total_pts = 0;
  if(n_pts.length() != xs.nrow()){
    for(int i = 0; i < xs.nrow(); i++){
      total_pts += n_pts[0];
      n_pts_v[i] = n_pts[0];
      }
    } else {
    for(int i = 0; i < n_pts.size(); i++){
      total_pts += n_pts[i];
      }
    n_pts_v = n_pts;
    }
  if(total_pts > std::pow(2,31)-1){
    std::string a = "Error: Matrix bound reached - use smaller resolution!";
    return 0;
    };
  NumericMatrix xyz(total_pts, 3);
  int ind = 0;
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialize3d(xs(i,_), ys(i,_), zs(i,_), n_pts_v[i]);
    for(int j = 0; j < resc_traj.nrow(); j++){
      xyz(ind, 0) = resc_traj(j,0);
      xyz(ind, 1) = resc_traj(j,1);
      xyz(ind, 2) = resc_traj(j,2);
      ind++;
    }
  }
  return xyz;
}

// spatial rescale (A)rray long 4D
//
// takes two matrixes as input and returns a Matrix
// containing the values of the input matrices in one
// columns each.

// [[Rcpp::export]]
NumericMatrix spatializeArrayToLong4d(NumericMatrix xs,
                                      NumericMatrix ys,
                                      NumericMatrix z1s,
                                      NumericMatrix z2s,
                                      NumericVector n_pts){
  NumericVector n_pts_v(xs.nrow());
  long total_pts = 0;
  if(n_pts.length() != xs.nrow()){
    for(int i = 0; i < xs.nrow(); i++){
      total_pts += n_pts[0];
      n_pts_v[i] = n_pts[0];
      }
    } else {
    for(int i = 0; i < n_pts.size(); i++){
      total_pts += n_pts[i];
      }
    n_pts_v = n_pts;
    }
  if(total_pts > std::pow(2,31)-1){
    std::string a = "Error: Matrix bound reached - use smaller resolution!";
    return 0;
    };
  NumericMatrix xyz(total_pts, 4);
  int ind = 0;
  for(int i = 0; i < xs.nrow(); i++){
    NumericMatrix resc_traj = spatialize4d(xs(i,_), ys(i,_), z1s(i,_), z2s(i,_), n_pts_v[i]);
    for(int j = 0; j < resc_traj.nrow(); j++){
      xyz(ind, 0) = resc_traj(j,0);
      xyz(ind, 1) = resc_traj(j,1);
      xyz(ind, 2) = resc_traj(j,2);
      xyz(ind, 3) = resc_traj(j,3);
      ind++;
    }
  }
  return xyz;
}

