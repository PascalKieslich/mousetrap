#' Create a Gausswindow.
#'
#' Create a Gausswindow of specified length and width as a numeric vector.
#' This function can be used for smoothing of trajectory data. The gausswindow
#' can be applied to a numeric vector using the convolve function.
#'
#' @param l length of the window.
#' @param w width of the gauss curve in the window.
#' @param ... ignored.
#' @return A numeric vector of length \code{l}.
#'
#' @examples
#' # Create some example data, just some arbitrary sum of sines
#' # NOTE: smoothed version will be a sum of sines of same
#' #       frequency as sines and cosines are eigenvectors
#' #       of convolution.
#' x <- seq(-10,10,by=0.1)
#' y <- 5*sin(2*x*2*pi+0.5)+2*sin(3*x*2*pi+0.3)+1*sin(4*x*2*pi+0.1)
#'
#' \dontrun{
#'    plot(x,y,type="l")
#' }
#'
#' # create a Gausswindow of length 10
#' g <- gausswindow(10)
#'
#' # Convolve y with g to create a smoothed version
#' # NOTE: s will be shorter than y, if type="filter"
#' #       and longer if type="open"
#' s <- convolve(y,g,type="filter")
#'
#' \dontrun{
#'    plot(x[seq_along(s)],s,type="l")
#' }
#'
#' @author
#' Tillmann Nett
#'
#' @export
gausswindow <- function(l, w=2.5,...) {
   if(l<=1) {
      stop("Length of window must be larger 1 (was", l, ")")
   }
   if(w<=0) {
      stop("Gausswindow cannot have a negative width")
   }
   ll <- l - 1
   x  <- seq(-ll/2,ll/2)
   exp(-1/2*((w*x/(ll/2))^2))
}
