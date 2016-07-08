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


#' Trial identifier for mousetrap data.
#' 
#' A character string specifying the name of the column containing the ID of 
#' every trial. By default, this label is called \code{mt_id}. This ID is 
#' generated when importing data using one of the mt_import functions (e.g., 
#' \link{mt_import_mousetrap}). Every data.frame that is added to a mousetrap 
#' data object (see \link{mt_example}) contains a column of that name. In every 
#' trajectory array, the first dimension corresponding to the different trials 
#' has the respective label of the trial as name. Using the \code{mt_id} 
#' variable, data from different data.frames or arrays can be merged easily.
#' Please note that it is currently not possible to change the default value.
#' 
#' @examples
#' # the default value
#' mt_id <- "mt_id"
#' 
#' # Calculate mouse-tracking measures
#' mt_example <- mt_calculate_measures(mt_example)
#' 
#' # Merge measures with trial data (adding "_raw"
#' # to columns already existing in the trial data)
#' mt_example_results <- merge(
#'   mt_example$data, mt_example$measures,
#'   by="mt_id",suffixes=c("_raw",""))
#' 
#' @export
mt_id <- "mt_id"