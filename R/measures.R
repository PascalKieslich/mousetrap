#' Calculate mouse-tracking measures.
#' 
#' Calculate a number of mouse-tracking measures for each trajectory.
#' 
#' Note that some measures are only returned if distance, velocity and 
#' acceleration are calculated using \link{mt_calculate_derivatives} before 
#' running \code{mt_calculate_measures}. Besides, the meaning of these measures
#' depends on the values of the arguments in \link{mt_calculate_derivatives}.
#'   
#' If the deviations from the idealized response trajectory have been calculated
#' using \link{mt_calculate_deviations} before running
#' \code{mt_calculate_measures}, the corresponding data in the trajectory array
#' will be used. If not, \code{mt_calculate_measures} will calculate these
#' deviations automatically.
#' 
#' The calculation of most measures can be deduced directly from their 
#' definition (see Value). For several more complex measures, a few details are 
#' provided in the following.
#' 
#' The \strong{maximum absolute deviation} (\code{MAD}) is the maximum 
#' perpendicular deviation from the straight path connecting start and end point
#' of the trajectory (e.g., Freeman & Ambady, 2010). If the \code{MAD} occurs
#' above the direct path, this is denoted by a positive value. If it occurs
#' below the direct path, this is denoted by a negative value. This assumes that
#' the complete movement in the trial was from bottom to top (i.e., the end
#' point has a higher y-position than the start point). In case the movement was
#' from top to bottom, \code{mt_calculate_measures} automatically flips the
#' signs. Both \code{MD_above} and  \code{MD_below} are also reported
#' separately. The \strong{average deviation} (\code{AD}) is the average of all
#' deviations across the trial.
#' 
#' The \code{AUC} represents the \strong{area under curve}, i.e., the geometric
#' area between the actual trajectory and the direct path. Areas above the
#' direct path are added and areas below are subtracted. The \code{AUC} is
#' calculated using the \link[pracma]{polyarea} function from the pracma
#' package.
#' 
#' 
#' @inheritParams mt_time_normalize
#' @param save_as a character string specifying where the calculated measures 
#'   should be stored.
#' @param dimensions a character vector specifying the two dimensions in the 
#'   trajectory array that contain the mouse positions. Usually (and by 
#'   default), the first value in the vector corresponds to the x-positions
#'   (\code{xpos}) and the second to the y-positions (\code{ypos}).
#' @param timestamps a character string specifying the trajectory dimension
#'   containing the timestamps.
#' @param flip_threshold a numeric value specifying the distance that needs to 
#'   be exceeded in one direction so that a change in direction counts as a
#'   flip.
#'   
#' @return A mousetrap data object (see \link{mt_example}) where an additional 
#'   \link{data.frame} has been added (by default called "measures") containing 
#'   the per-trial mouse-tracking measures. Each row in the data.frame 
#'   corresponds to one trajectory (the corresponding trajectory is identified 
#'   via the rownames and, additionally, in the column "mt_id"). Each column in 
#'   the data.frame corresponds to one of the measures. If a trajectory array 
#'   was provided directly as \code{data}, only the measures data.frame will be 
#'   returned.
#'   
#'   The following measures are computed for each trajectory (the labels
#'   relating to x- and y-positions will be adapted depending on the values
#'   specified in \code{dimensions}):
#'   \item{mt_id}{Trial ID (can be used for merging measures data.frame with
#'   other trial-level data)}
#'   \item{xpos_max}{Maximum x-position} 
#'   \item{xpos_min}{Minimum x-position}
#'   \item{ypos_max}{Maximum y-position} 
#'   \item{ypos_min}{Minimum y-position}
#'   \item{MAD}{Maximum absolute deviation from the direct path connecting start
#'   and end point of the trajectory (straight line)}
#'   \item{MAD_time}{Time at which the maximum absolute deviation was reached
#'   first}
#'   \item{MD_above}{Maximum deviation above the direct path} 
#'   \item{MD_above_time}{Time at which the maximum deviation above was reached 
#'   first}
#'   \item{MD_below}{Maximum deviation below the direct path} 
#'   \item{MD_below_time}{Time at which the maximum deviation below was reached 
#'   first}
#'   \item{AD}{Average deviation from direct path}
#'   \item{AUC}{Area under curve, the geometric area between the actual
#'   trajectory and the direct path where areas below the direct path have been
#'   subtracted}
#'   \item{xpos_flips}{Number of directional changes along x-axis} 
#'   \item{ypos_flips}{Number of directional changes along y-axis} 
#'   \item{xpos_reversals}{Number of crossings of the y-axis} 
#'   \item{ypos_reversals}{Number of crossings of the x-axis}
#'   \item{RT}{Response time, the total time passed until a response was given}
#'   \item{initiation_time}{Time passed until first mouse movement was 
#'   initiated}
#'   \item{idle_time}{Total time without mouse movement across the entirety of
#'   the trial}
#'   \item{total_dist}{Total distance covered by the trajectory}
#'   \item{vel_max}{Maximum velocity}
#'   \item{vel_max_time}{Time at which maximum velocity occurred first}
#'   \item{vel_min}{Minimum velocity} 
#'   \item{vel_min_time}{Time at which minimum velocity occurred first} 
#'   \item{acc_max}{Maximum acceleration}
#'   \item{acc_max_time}{Time at which maximum acceleration occurred first}
#'   \item{acc_min}{Minimum acceleration} 
#'   \item{acc_min_time}{Time where minimum acceleration occurred first} 
#'   
#' @references Mousetrap
#'   
#'   Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for studying 
#'   real-time mental processing using a computer mouse-tracking method. 
#'   \emph{Behavior Research Methods, 42}(1), 226-241.
#'   
#'   
#'   
#' @seealso \link{mt_sample_entropy} for calculating sample entropy.
#' 
#' \link{mt_movement_angle} for calculating the initial movement angle.
#' 
#' \link{mt_standardize} for standardizing the measures per subject.
#' 
#' \link{mt_check_bimodality} for checking bimodality of the measures using
#' different methods.
#' 
#' \link{mt_aggregate} and \link{mt_aggregate_per_subject} for aggregating the
#' measures.
#' 
#' \link{merge} for merging data.
#' 
#' @examples
#' mt_example <- mt_calculate_derivatives(mt_example)
#' mt_example <- mt_calculate_deviations(mt_example)
#' mt_example <- mt_calculate_measures(mt_example)
#' 
#' # Merge measures with trial data (adding "_raw"
#' # to columns already existing in the trial data)
#' mt_example_results <- merge(
#'   mt_example$data, mt_example$measures,
#'   by="mt_id",suffixes=c("_raw",""))
#'   
#' @export
mt_calculate_measures <- function(data,
  use="trajectories", save_as="measures",
  dimensions=c("xpos","ypos"), timestamps="timestamps",
  flip_threshold=0,
  verbose=FALSE,show_progress=NULL) {
  
  if(is.null(show_progress)==FALSE){
    warning("The argument show_progress is deprecated. ",
            "Please use verbose instead.")
    verbose <- show_progress
  }
  
  if(length(dimensions)!=2){
    stop("For dimensions, exactly two trajectory dimensions have to be specified.")
  }
  
  # Prepare data
  trajectories <- extract_data(data=data, use=use)
  dist <- "dist"
  vel  <- "vel"
  acc  <- "acc"
  dev_ideal <- "dev_ideal"
  
  dim1 <- dimensions[[1]]
  dim2 <- dimensions[[2]]
  dim1_ideal <- paste0(dim1,"_ideal")
  dim2_ideal <- paste0(dim2,"_ideal")
  
  # Calculate number of logs
  nlogs <- rowSums(!is.na(trajectories[,dim1,,drop=FALSE]))
  
  # Calculate deviations if no deviations were found in the data
  if (!all(c(dev_ideal,dim1_ideal,dim2_ideal) %in% dimnames(trajectories)[[2]])) {
    if (verbose) {
      message("Start calculating deviations of actual from idealized response trajectory.")
    }
    trajectories <- mt_calculate_deviations(
      data=trajectories, verbose=verbose
    )
    if (verbose) {
      message("Start calculating mouse-tracking measures.")
    }
  }

  # Setup variable matrix depending on whether timestamps are provided or not
  if (timestamps %in% dimnames(trajectories)[[2]]) {
    mt_measures <- c(
      paste0(dim1,c("_max","_min")),paste0(dim2,c("_max","_min")),
      "MAD", "MAD_time",
      "MD_above", "MD_above_time",
      "MD_below", "MD_below_time",
      "AD", "AUC",
      paste0(dimensions,"_flips"),
      paste0(dimensions,"_reversals"),
      "RT", "initiation_time", "idle_time"
    )
    
    # Check if there are trajectories where first timestamp is > 0:
    if (max(trajectories[,timestamps,1]) > 0) {
      message(
        "Trajectories detected where first timestamp is greater than 0. ",
        "Assuming period without movement starting at timestamp 0."
      )
      # only affects _time variables
    } else if (min(trajectories[,timestamps,1]) < 0) {
      stop(
        "Trajectories detected where first timestamp is smaller than 0. ",
        "Please check that trajectories were logged/imported correctly."
      )
    }
    
  } else {
    message(
      "No timestamps were found in trajectory array. ",
      "Not computing the corresponding measures."
    )
    mt_measures <- c(
      paste0(dim1,c("_max","_min")),paste0(dim2,c("_max","_min")),
      "MAD", "MD_above", "MD_below",
      "AD", "AUC",
      paste0(dimensions,"_flips"),
      paste0(dimensions,"_reversals")
    )
  }
  
  # Add distance, velocity and acceleration-based measures
  # if the derivatives were precomputed
  if (dist %in% dimnames(trajectories)[[2]]) {
    mt_measures <- c(mt_measures, "total_dist")
  }
  
  if (vel %in% dimnames(trajectories)[[2]] & timestamps %in% dimnames(trajectories)[[2]]) {
    mt_measures <- c(mt_measures, 
      "vel_max", "vel_max_time", "vel_min", "vel_min_time")
  }
  
  if (acc %in% dimnames(trajectories)[[2]] & timestamps %in% dimnames(trajectories)[[2]]) {
    mt_measures <- c(mt_measures, 
      "acc_max", "acc_max_time", "acc_min", "acc_min_time")
  }
  
  # Create empty matrix for measures
  # (trajectories in rows, measures in columns)
  measures <- matrix(data=NA,
    nrow=nrow(trajectories), 
    ncol=length(mt_measures),
    dimnames = list(
      row.names(trajectories),
      mt_measures
    )
  )
  
  # Iterate over trajectories and calculate measures
  for (i in 1:nrow(trajectories)) {
    
    current_nlogs <- nlogs[i]
    
    # Extract variables
    current_dim1 <- trajectories[i, dim1, 1:current_nlogs]
    current_dim2 <- trajectories[i, dim2, 1:current_nlogs]
    current_dim1_ideal <- trajectories[i, dim1_ideal, 1:current_nlogs]
    current_dim2_ideal <- trajectories[i, dim2_ideal, 1:current_nlogs]
    current_dev_ideal <- trajectories[i, dev_ideal, 1:current_nlogs]
    
    # Calculate min and max values for x and y
    measures[i,paste0(dim1,"_max")] <- max(current_dim1)
    measures[i,paste0(dim1,"_min")] <- min(current_dim1)
    measures[i,paste0(dim2,"_max")] <- max(current_dim2)
    measures[i,paste0(dim2,"_min")] <- min(current_dim2)
    
    # Maximum absolute deviation (output including sign)
    measures[i,"MAD"] <- current_dev_ideal[which.max(abs(current_dev_ideal))]
    
    # Maximum deviation above idealized trajectory 
    # (== in direction of non-chosen option)
    measures[i,"MD_above"] <- max(current_dev_ideal)
    
    # Maximum deviation below idealized trajectory 
    # (== in direction of chosen option)
    measures[i,"MD_below"] <- min(current_dev_ideal)
    
    # Calculate average deviation from direct path
    measures[i,"AD"] <- mean(current_dev_ideal)
    
    
    # Calculate area under curve
    # the geometric area between the actual trajectory and the direct path
    # where areas below the direct path have been subtracted
    measures[i,"AUC"]<- pracma::polyarea(current_dim1,current_dim2)
    
    # Flip sign of AUC depending on direction of trajectory
    
    # ... flip if trajectory ended in top-right
    if (current_dim2[current_nlogs]>current_dim2[1] & 
        current_dim1[current_nlogs]>current_dim1[1]) {
      measures[i,"AUC"] <- -(measures[i,"AUC"])
    
    # ... flip if trajectory ended in bottom-left
    } else if (current_dim2[current_nlogs]<current_dim2[1] & 
        current_dim1[current_nlogs]<current_dim1[1]) {
      measures[i,"AUC"] <- -(measures[i,"AUC"])
    }
    
    
    # Calculate number of x_flips and y_flips
    measures[i,paste0(dim1,"_flips")] <- count_changes(current_dim1, threshold=flip_threshold)
    measures[i,paste0(dim2,"_flips")] <- count_changes(current_dim2, threshold=flip_threshold)
    
    # Calculate x_reversals
    # number of crossings of the y-axis (ignoring points exactly on y axis)
    yside <- current_dim1[current_dim1!=0] > 0
    measures[i,paste0(dim1,"_reversals")] <- sum(abs(diff(yside)))
    
    # Calculate y_reversals
    # number of crossings of the x-axis (ignoring points exactly on x axis)
    xside <- current_dim2[current_dim2 != 0] > 0
    measures[i,paste0(dim2,"_reversals")] <- sum(abs(diff(xside)))
    
    # Check if timestamps are included and if so,
    # retrieve timestamps and calculate corresponding measures
    if (timestamps %in% dimnames(trajectories)[[2]]) {
      
      current_timestamps <- trajectories[i,timestamps,1:nlogs[i]]
      
      # If first timestamp > 0, add another with 0 to indicate phase without movement
      if (current_timestamps[1] > 0) {
        current_timestamps <- c(0, current_timestamps)
        current_dim1 <- c(current_dim1[1], current_dim1)
        current_dim2 <- c(current_dim2[1], current_dim2)
        current_dev_ideal <- c(current_dev_ideal[1], current_dev_ideal)
        nlogs[i] <- nlogs[i] + 1
      }

      measures[i,"RT"] <- max(current_timestamps)
      
      # Calculate variables for phases with and without movement
      time_diffs <- diff(current_timestamps)
      # Indicate for each sample whether the position changed
      pos_constant <- (diff(current_dim1) == 0) & (diff(current_dim2) == 0)

      if (all(pos_constant == FALSE)) {
        # Continuous movement
        measures[i,"initiation_time"] <- 0
        measures[i,"idle_time"] <- 0
      } else if (all(pos_constant == TRUE)) {
        # No movement at all
        measures[i,"initiation_time"] <- measures[i,"RT"]
        measures[i,"idle_time"] <- measures[i,"RT"]
      } else {
        # Intermittent movement
        measures[i,"initiation_time"] <- ifelse(
          !pos_constant[1],
          0,
          sum(time_diffs[1:(which(pos_constant == FALSE)[1]-1)])
        )
        measures[i,"idle_time"] <- sum(time_diffs[pos_constant])
      }
      
      # notes: timestamps (e.g., for MAD) always correspond
      # to the first time the max/min value was reached
      measures[i,"MAD_time"] <- current_timestamps[which.max(abs(current_dev_ideal))]
      measures[i,"MD_above_time"] <- current_timestamps[which.max(current_dev_ideal)]
      measures[i,"MD_below_time"] <- current_timestamps[which.min(current_dev_ideal)]
      
    }
    
    # Compute total distance covered
    if (dist %in% dimnames(trajectories)[[2]]) {
      measures[i,"total_dist"] <- sum(abs(trajectories[i,dist,]), na.rm=TRUE)
    }
    
    # Velocity-based measures
    if (vel %in% dimnames(trajectories)[[2]] & timestamps %in% dimnames(trajectories)[[2]]) {

      # Maximum velocity
      measures[i,"vel_max"] <- max(trajectories[i,vel,], na.rm=TRUE)
      vel_max_pos <- which.max(trajectories[i,vel,])
      
      # Interpolate timestamp of maximum velocity
      # (see mt_calculate_derivatives for logic of velocity timestamps)
      vel_max_pos <- ifelse(vel_max_pos==1, 1, c(vel_max_pos-1, vel_max_pos))
      measures[i,"vel_max_time"] <- mean(trajectories[i,timestamps,vel_max_pos])
      
      # Minimum velocity (which is treated analogously)
      measures[i,"vel_min"] <- min(trajectories[i,vel,], na.rm=TRUE)
      vel_min_pos <- which.min(trajectories[i,vel,])
      
      # average timestamps (see mt_calculate_derivatives for logic of velocity timestamps)
      vel_min_pos <- ifelse(
        vel_min_pos == 1,
        1, c(vel_min_pos-1, vel_min_pos)
      )
      measures[i,"vel_min_time"] <- mean(trajectories[i,timestamps,vel_min_pos])
    }

    if (acc %in% dimnames(trajectories)[[2]] & timestamps %in% dimnames(trajectories)[[2]]) {
      # Maximum acceleration
      measures[i,"acc_max"] <- max(trajectories[i,acc,], na.rm=TRUE)
      measures[i,"acc_max_time"] <- trajectories[i,timestamps,which.max(trajectories[i,acc,])]
      
      # Minimum acceleration
      measures[i,"acc_min"] <- min(trajectories[i,acc,], na.rm=TRUE)
      measures[i,"acc_min_time"] <- trajectories[i,timestamps,which.min(trajectories[i,acc,])]
    }

    if (verbose & i %% 100 == 0) {
      message(paste(i, "trials completed"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials completed"))
  }
  
  return(create_results(
    data=data, results=measures, use=use, save_as=save_as,
    ids=rownames(trajectories), overwrite=TRUE))
  
}
