#' Labels for mousetrap variables.
#' 
#' A named character vector specifying the names of the variables that mousetrap
#' uses for labeling the mouse-tracking variables. The default values are 
#' \code{xpos} for x-positions, \code{ypos} for y-positions, \code{timestamps} 
#' for timestamps, \code{dist} for distance traveled, \code{vel} for velocity, 
#' and \code{acc} for acceleration. These labels will be used for labeling the 
#' respective dimensions in the trajectory array (see \link{mt_example}).
#' 
#' @examples
#' # the default values
#' mt_variable_labels <- c(
#'     timestamps="timestamps",
#'     xpos="xpos",ypos="ypos",
#'     dist="dist",vel="vel",acc="acc")
#' 
#' @export
mt_variable_labels=c(
  timestamps="timestamps",
  xpos="xpos",ypos="ypos",
  dist="dist",vel="vel",acc="acc")


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