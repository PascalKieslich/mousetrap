#' Calculate deviations from idealized trajectory.
#' 
#' Calculate the idealized trajectory and the perpendicular deviations of the
#' actual trajectory from it for each logged position.
#' 
#' The idealized trajectory is defined as the straight line connecting the start
#' and end point of the actual trajectory (e.g., Freeman & Ambady, 2010). The 
#' deviation for each position is calclated as the perpendicular deviation of 
#' the actual trajectory from the idealized trajectory.
#' 
#' If a deviation occurs above the direct path, this is denoted by a positive
#' value. If it occurs below the direct path, this is denoted by a negative
#' value. This assumes that the complete movement in the trial was from bottom
#' to top (i.e., the end point has a higher y-position than the start point). In
#' case the movement was from top to bottom, \code{mt_calculate_deviations}
#' automatically flips the signs.
#' 
#' @inheritParams mt_time_normalize
#' @param start_ideal an optional vector specifying the start position (see
#'   Example). If specified, this position will be used as the starting point of
#'   the idealized trajectory (instead of the actual starting point).
#' @param end_ideal an optional vector specifying the end position (see
#'   Example). If specified, this position will be used as the end point of the
#'   idealized trajectory (instead of the actual end point).
#' @param prefix an optional character string that is added as a prefix to the 
#'   to be created new trajectory dimensions.
#'   
#' @return A mousetrap data object (see \link{mt_example}) with the x- and
#'   y-positions of the idealized trajectory and the perpendicular deviations of
#'   the actual trajectory from it added as additional columns to the trajectory
#'   array. If the trajectory array was provided directly as \code{data}, only
#'   the trajectory array will be returned.
#'
#' @references Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for
#'   studying real-time mental processing using a computer mouse-tracking
#'   method. \emph{Behavior Research Methods, 42}(1), 226-241.
#'   
#' @seealso \link{mt_calculate_measures} for calculating per-trial
#'   mouse-tracking measures.
#'   
#' @examples
#' # Calculate deviations from idealized trajectory
#' # (straight line connecting the start and end point of each trial)
#' mt_example <- mt_calculate_deviations(mt_example)
#'   
#' # Calculate deviations from idealized trajectory with
#' # constant start and end points across trials
#' mt_example <- mt_calculate_deviations(mt_example,
#'   start_ideal=c(xpos=0,ypos=0), end_ideal=c(xpos=-665,ypos=974))
#' 
#' @export
mt_calculate_deviations <- function(data,
  use="trajectories", save_as=use,
  start_ideal=NULL,end_ideal=NULL,
  prefix="",
  verbose=FALSE,show_progress=NULL) {
  
  if(is.null(show_progress)==FALSE){
    warning("The argument show_progress is deprecated. ",
            "Please use verbose instead.")
    verbose <- show_progress
  }
  
  # Extract trajectories and labels
  trajectories <- extract_data(data=data,use=use)
  xpos <- mt_variable_labels[["xpos"]]
  ypos <- mt_variable_labels[["ypos"]]
  xpos_ideal <- paste0(prefix,mt_variable_labels[["xpos_ideal"]])
  ypos_ideal <- paste0(prefix,mt_variable_labels[["ypos_ideal"]])
  dev_ideal <- paste0(prefix,mt_variable_labels[["dev_ideal"]])
  
  # Create new array with added columns for the new variables
  deviations <- mt_add_variables(trajectories,
    variables=c(xpos_ideal,ypos_ideal,dev_ideal))
  
  # Calculate number of logs
  nlogs <- rowSums(!is.na(deviations[,xpos,,drop=FALSE]))
  
  
  # Calculate deviations
  for (i in 1:nrow(deviations)){
    
    current_points <- deviations[i, c(xpos,ypos), 1:nlogs[i]]
    current_xpos <- deviations[i, xpos, 1:nlogs[i]]
    current_ypos <- deviations[i, ypos, 1:nlogs[i]]
    
    # Determine straight line (idealized trajectory)
    current_points_ideal <- points_on_ideal(current_points,
                                            start=start_ideal,end=end_ideal)
    current_xpos_ideal <- current_points_ideal[xpos,]
    current_ypos_ideal <- current_points_ideal[ypos,]
    
    # Calculate distance of each point on the curve from straight line
    # (cf. Pythagoras, some time ago)
    current_dev_ideal <- sqrt(colSums((current_points_ideal-current_points)^2))
    
    # Check if end point is above the start point
    end_above_start <- current_ypos_ideal[length(current_ypos_ideal)] >= current_ypos_ideal[1]
    
    # Flip deviation for points under the idealized straight line
    if (end_above_start) {
      current_dev_ideal[current_ypos_ideal > current_ypos] <- 
        -current_dev_ideal[current_ypos_ideal > current_ypos]
    } else {
      current_dev_ideal[current_ypos_ideal < current_ypos] <- 
        -current_dev_ideal[current_ypos_ideal < current_ypos]
    }
    
    # Add idealized positions and deviations to array
    deviations[i,xpos_ideal,1:nlogs[i]] <- current_xpos_ideal
    deviations[i,ypos_ideal,1:nlogs[i]] <- current_ypos_ideal
    deviations[i,dev_ideal,1:nlogs[i]] <- current_dev_ideal 
    
    if (verbose){
      if (i %% 100 == 0) message(paste(i,"trials finished"))
    }
  }
  
  if (verbose){
    message(paste("all",i,"trials finished"))
  }
  
  return(create_results(data=data, results=deviations, use=use, save_as=save_as))
  
}
