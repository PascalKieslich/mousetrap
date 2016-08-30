#' Labels for mousetrap variables.
#' 
#' A named character vector specifying the names of the variables that mousetrap
#' uses for labeling the mouse-tracking variables. The default values are 
#' \code{xpos} for x-positions, \code{ypos} for y-positions, \code{timestamps} 
#' for timestamps, \code{dist} for distance traveled, \code{vel} for velocity, 
#' and \code{acc} for acceleration. Besides, \code{xpos_ideal} for x-positions 
#' and \code{ypos_ideal} for y-positions of an idealized trajectory, and 
#' \code{dev_ideal} for deviations of the actual trajectory from the idealized 
#' trajectory. These labels will be used for labeling the respective dimensions 
#' in the trajectory array (see \link{mt_example}). Please note that it is
#' currently not possible to change the default labels.
#' 
#' @examples
#' # the default values
#' mt_variable_labels <- c(
#'     timestamps="timestamps",
#'     xpos="xpos",ypos="ypos",
#'     dist="dist",vel="vel",acc="acc",
#'     xpos_ideal="xpos_ideal",
#'     ypos_ideal="ypos_ideal",
#'     dev_ideal="dev_ideal")
#' 
#' @export
mt_variable_labels=c(
  timestamps="timestamps",
  xpos="xpos",ypos="ypos",
  dist="dist",vel="vel",acc="acc",
  xpos_ideal="xpos_ideal",
  ypos_ideal="ypos_ideal",
  dev_ideal="dev_ideal")
