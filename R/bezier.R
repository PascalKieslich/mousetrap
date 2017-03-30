#' Create Bezier-curves using the Bernstein approximation.
#' 
#' \code{mt_bezier} creates 3-point Bezier-curves using the 
#'   Bernstein appproximation to simulate continuous competition 
#'   in mouse- and hand-trajectories.   
#'   
#' @param x a numeric vector giving the x-coordinates of exactly 
#'   three Bezier-points. Defaults to c(0,1,-1) matching the 'mt'
#'   format in \link{mt_align}.
#' @param y a numeric vector giving the x-coordinates of exactly
#'   three Bezier-points. Defaults to c(0,1.5,1.5) matching the 'mt'
#'   format in \link{mt_align}.
#' @param w a numeric giving one more Bezier coefficients that, which 
#'   controls the pull towards the middle point. Each entry in 
#'   \code{w} creates one Bezier-curve.
#' @param resol numeric the spatial resolution of the bezier curves. 
#'   E.g., \code{resol = 100} creates bezier curves comprised of 100 
#'   points each. 
#'
#' @examples
#' \dontrun{
#'   # Generate range of Bezier-curves
#'   b = bezier(w=seq(0,10,.1))
#'   plot.new();plot.window(c(-1,1),c(0,1.5))
#'   for(i in 1:dim(b)[1]) lines(b[i,,])
#' }
#'    
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#' 
#' @export    

bezier = function(x = c(0,1,-1), y = c(0,1.5,1.5), w = 1, resol = 100){
  
  # Setup array
  b = array(
    dim=c(length(w), resol, 2),
    dimnames=list(
      paste0('bezier_',1:length(w)),
      NULL,
      c('xpos','ypos')
      )
    )
  
  for(i in 1:length(w)){
    t = seq(0,1,length = resol)           #Resolution
    B = cbind( (1-t)^2, 2*t*(1-t),t^2 )   #Bernstein
    bx = ( B[,1] * x[1] + w[i] * B[,2] * x[2] + B[,3] * x[3] ) / 
         ( B[,1]        + w[i] * B[,2]        + B[,3]) 
    by = ( B[,1] * y[1] + w[i] * B[,2] * y[2] + B[,3] * y[3] ) / 
         ( B[,1]        + w[i] * B[,2]        + B[,3]) 
    b_mat = cbind(bx,by)
    b[i,,] = b_mat 
    }
  
  return(b)
  }

