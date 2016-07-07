#' Calculate initial movement angle.
#'
#' Calculate initial movement angle (IMA) and related measures for each
#' trajectory.
#' 
#' The \code{IMA} is based on the initial movement angle used by Buetti
#' and Kerzel (2009). In their experiment, actual hand movements were recorded 
#' and the \code{IMA} was the angle between the position of the hand and the 
#' axis running through the correct response location at a specific point in 
#' time (in the original study, one fifth of the trial). Adapting this to the 
#' mouse-tracking setup, the \code{IMA} is the angle between the idealized
#' response trajectory (straight line) and the movement from the starting point
#' in the trial to the position of the mouse at the specified percentile. If
#' this position is above the idealized response trajectory, the angle has a
#' positive value. If it is below the idealized response trajectory, the angle
#' has a negative value.
#' 
#' The \code{IMA} is calculated for a specific percentile of the trial, which 
#' can be adjusted using \code{ima_percentile} (e.g., \code{ima_percentile=0.2}
#' to correspond to the study by Buetti and Kerzel, 2009). In addition, the
#' timestamp of the specified percentile is also reported (\code{IMA_time}).
#' Besides, the perpendicular deviation from the direct path at the specified
#' percentile is included (\code{IMD}).
#' 
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory 
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory data should be
#'   used.
#' @param save_as a character string specifying where the calculated measures
#'   should be stored.
#' @param ima_percentile a decimal value. The initial movement angle will be
#'   calculated at the respective percentile .
#' @param show_progress logical indicating whether function should report on the
#'   progress.
#'   
#' @return A mousetrap data object (see \link{mt_example}).
#'   
#'   If a data.frame with label specified in \code{save_as} (by default 
#'   "measures") already exists, the \code{IMA}, \code{IMA_time}, and \code{IMD}
#'   values (see Details) are added as additional columns (by merging them using
#'   the \link{mt_id} variable).
#'   
#'   If not, an additional \link{data.frame} will be added.
#'   
#'   If a trajectory array was provided directly as \code{data}, only the
#'   data.frame will be returned.
#'   
#' @references Buetti, S., & Kerzel, D. (2009). Conflicts during response 
#'   selection affect response programming: Reactions toward the source of 
#'   stimulation. \emph{Journal of Experimental Psychology: Human Perception and
#'   Performance, 35}(3), 816-834.
#' 
#' @seealso \link{mt_calculate_measures} for calculating other mouse-tracking
#' measures.
#' 
#' @examples
#' mt_example <- mt_calculate_measures(mt_example)
#' mt_example <- mt_movement_angle(mt_example,
#'   use="trajectories", save_as="measures",
#'   ima_percentile=0.20)
#' 
#' @export
mt_movement_angle <- function(data,
                              use="trajectories", save_as="measures",
                              ima_percentile=0.2,
                              show_progress=TRUE) {
  

  # Prepare data
  trajectories <- extract_data(data=data, use=use)
  timestamps <- mt_variable_labels[["timestamps"]]
  xpos <- mt_variable_labels[["xpos"]]
  ypos <- mt_variable_labels[["ypos"]]
  
  # Calculate number of logs
  nlogs <- rowSums(!is.na(trajectories[,xpos,,drop=FALSE]))
  
  measures <- matrix(
    data=NA,
    nrow=nrow(trajectories),
    ncol=3,
    dimnames=list(
      row.names(trajectories),
      c("IMA","IMA_time","IMD")
    )
  )
  
  # Calculate measures
  for (i in 1:nrow(trajectories)){
    
    current_xpos <- trajectories[i, xpos, 1:nlogs[i]]
    current_ypos <- trajectories[i, ypos, 1:nlogs[i]]
    current_timestamps <- trajectories[i,timestamps,1:nlogs[i]]
    
    # Check if end point is above the start point
    end_above_start <- current_ypos[length(current_ypos)] >= current_ypos[1]
    
    # Look at 3 points (start point, ima_point (interpolated), end point)
    
    # First, calculate the timestamp for each of these points.
    ima_timestamps <- c(
      current_timestamps[1],
      ima_percentile * current_timestamps[nlogs[i]],
      current_timestamps[nlogs[i]]
    )
    
    # Second, interpolate the positions at each of these timestamps
    # and build a matrix of these
    ima_points <- rbind(
      stats::approx(current_timestamps, current_xpos, xout=ima_timestamps)$y,
      stats::approx(current_timestamps, current_ypos, xout=ima_timestamps)$y
    )
    
    row.names(ima_points) <- c(xpos, ypos)
    
    # Calculate initial movement angle (IMA, cf. Buetti & Kerzel, 2009, Fig. 1)
    # originally: angle between position of hand after one fifth of trajectory had been
    #             traversed and axis running through correct response location.
    # here: Angle between the position of the mouse and the idealized response trajectory
    # depends on MD and the length of the idealized response trajectory,
    # where MD intersects it (called b here)
    ima_straight_line <- points_on_ideal(ima_points)
    imd <- sqrt(sum((ima_straight_line[,2]-ima_points[,2])^2))
    
    # Flip sign if point is below the ideal line
    if (end_above_start){
      if (ima_points[ypos,2]<ima_straight_line[ypos,2]){
        imd <- (-imd)
      }
    } else {
      if (ima_points[ypos,2]>ima_straight_line[ypos,2]){
        imd <- (-imd)
      }
    }
    
    # Calculate the distance between the point on the ideal line and the start point
    b <- sqrt(sum((ima_straight_line[,2]-ima_points[,1])^2))
    
    # Flip sign if point on ideal line is below the start point
    if (end_above_start){
      if (ima_straight_line[ypos,2]<ima_points[ypos,1]){
        b <- (-b)
      } 
    } else {
      if (ima_straight_line[ypos,2]>ima_points[ypos,1]){
        b <- (-b)
      }
    }
    
    
    # Use trigonometry to determine the angle between the ideal
    # line and the actual observed trajectory.
    # If distance from start position is 0, IMA is 0
    ima <- ifelse(b == 0, 0, atan2(imd,b) * 180/pi)
    
    # the value of the ima_point (second point in vector) is of interest
    measures[i,"IMA"] <- ima
    measures[i,"IMA_time"] <- ima_timestamps[2]
    measures[i,"IMD"] <- imd
    
    if (show_progress) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }
  
  if (show_progress) {
    message(paste("all", i, "trials finished"))
  }
  
  results <- data.frame(row.names(trajectories))
  colnames(results) <- mt_id
  rownames(results) <- results[,mt_id]
  results <- cbind(results,data.frame(measures))
  
  
  if (is_mousetrap_data(data)){
    if (save_as %in% names(data)) {
      data[[save_as]] <- merge(data[[save_as]], results, by=mt_id)
    } else {
      data[[save_as]] <- results
    }
    return(data)
    
  }else{
    return(results)
  }
  
}
