#include <Rcpp.h>
using namespace Rcpp;

// © Dirk U. Wulff, December 2016

// [[Rcpp::export]]
std::vector<double> computeNorm(double r){
  std::vector<double> wghts; 
  double rs = std::ceil(r * 2.57);
  for(int iy = -rs; iy < rs + 1; iy++){
    for(int ix = -rs; ix < rs + 1; ix++){
      double d = (ix)*(ix)+(iy)*(iy);
      double wght = std::exp(-d/(2*r*r))/(6.283185*r*r);
      wghts.push_back(wght);
    }
  }
  return(wghts);
}


// [[Rcpp::export]]
std::vector<double> gaussBlurSlow(std::vector<double> source,
                                std::vector<double> target,
                                double w, 
                                double h, 
                                double r
                                ){
  double rs = std::ceil(r * 2.57);
  for(int i = 0; i < h; i++){
    for(int j = 0; j < w; j++){
      double val = 0, wsum = 0;
      for(int iy = i - rs; iy < i + rs + 1; iy++){
        for(int ix = j - rs; ix < j + rs + 1; ix++){
          double y = std::min(int(h - 1), std::max(0, iy));
          double x = std::min(int(w - 1), std::max(0, ix));
          double d = (iy-i)*(iy-i)+(ix-j)*(ix-j);
          double wght = std::exp(-d/(2*r*r))/(6.283185*r*r);
          val += source[y*w+x] * wght;
          wsum += wght; 
          }
        }
      target[i*w+j] = val / wsum;
      }
    }
  return(target);
  }


// [[Rcpp::export]]
std::vector<double> gaussBlur(std::vector<double> source,
                                std::vector<double> target,
                                double w, 
                                double h, 
                                double r
){
  std::vector<double> norms = computeNorm(r);
  double rs = std::ceil(r * 2.57);
  for(int i = 0; i < h; i++){
    for(int j = 0; j < w; j++){
      double val = 0, wsum = 0;
      for(int iy = i - rs; iy < i + rs + 1; iy++){
        for(int ix = j - rs; ix < j + rs + 1; ix++){
          double x  = std::min(int(w - 1), std::max(0, ix));
          double y  = std::min(int(h - 1), std::max(0, iy));
          double wght = norms[((iy - i)+rs)*(2*rs+1) + ((ix - j)+rs)];
          val += source[y*w+x] * wght;
          wsum += wght; 
        }
      }
      target[i*w+j] = val / wsum;
    }
  }
  return(target);
}



// [[Rcpp::export]]
std::vector<double> boxBlur(std::vector<double> source,
                                std::vector<double> target,
                                double w, 
                                double h, 
                                double r
){
  for(int i = 0; i < h; i++){
    for(int j = 0; j < w; j++){
      double val = 0, wsum = 0;
      for(int iy = -r; iy < r + 1; iy++){
        for(int ix = -r; ix < r + 1; ix++){
          double x = std::min(int(w - 1), std::max(0, j+ix));
          double y = std::min(int(h - 1), std::max(0, i+iy));
          double wght = 1;
          val += source[y*w+x] * wght;
          wsum += wght; 
        }
      }
      target[i*w+j] = val / wsum;
    }
  }
  return(target);
}
