#' Calculate mouse-tracking measures.
#' 
#' Calculate a number of mouse-tracking measures for each trajectory, such as 
#' minima, maxima, and flips for each dimension, and different measures for 
#' curvature (e.g., \code{MAD}, \code{AD}, and \code{AUC}). Note that some
#' measures are only returned if distance, velocity and acceleration are
#' calculated using \link{mt_derivatives} before running \code{mt_measures}. 
#' More information on the different measures can be found in the Details and
#' Values sections.
#' 
#' Note that some measures are only returned if distance, velocity and 
#' acceleration are calculated using \link{mt_derivatives} before 
#' running \code{mt_measures}. Besides, the meaning of these measures
#' depends on the values of the arguments in \link{mt_derivatives}.
#'   
#' If the deviations from the idealized response trajectory have been calculated
#' using \link{mt_deviations} before running
#' \code{mt_measures}, the corresponding data in the trajectory array
#' will be used. If not, \code{mt_measures} will calculate these
#' deviations automatically.
#' 
#' The calculation of most measures can be deduced directly from their 
#' definition (see Value). For several more complex measures, a few details are 
#' provided in the following.
#' 
#' The signed \strong{maximum absolute deviation} (\code{MAD}) is the maximum 
#' perpendicular deviation from the straight path connecting start and end point
#' of the trajectory (e.g., Freeman & Ambady, 2010). If the \code{MAD} occurs 
#' above the direct path, this is denoted by a positive value. If it occurs 
#' below the direct path, this is denoted by a negative value. This assumes that
#' the complete movement in the trial was from bottom to top (i.e., the end 
#' point has a higher y-position than the start point). In case the movement was
#' from top to bottom, \code{mt_measures} automatically flips the signs. Both 
#' \code{MD_above} and  \code{MD_below} are also reported separately.
#' 
#' The \strong{average deviation} (\code{AD}) is the average of all deviations 
#' across the trial. Note that \code{AD} ignores the timestamps when calculating
#' this average. This implicitly assumes that the time passed between each 
#' recording of the mouse is the same within each individual trajectory. If the 
#' \code{AD} is calculated using raw data that were obtained with an 
#' approximately constant logging resolution (sampling rate), this assumption is
#' usually justified (\link{mt_check_resolution} can be used to check this). 
#' Alternatively, the \code{AD} can be calculated based on time-normalized 
#' trajectories; these can be computed using \link{mt_time_normalize} which 
#' creates equidistant time steps within each trajectory.
#' 
#' The \code{AUC} represents the \strong{area under curve}, i.e., the geometric
#' area between the actual trajectory and the direct path. Areas above the
#' direct path are added and areas below are subtracted. The \code{AUC} is
#' calculated using the \link[pracma]{polyarea} function from the pracma
#' package.
#' 
#' Note that all \strong{time} related measures (except \code{idle_time} and
#' \code{hover_time}) are reported using the timestamp metric as present in the
#' data. To interpret the timestamp values as time since tracking start, the
#' assumption has to be made that for each trajectory the tracking started at
#' timestamp 0 and that all timestamps indicate the time passed since tracking
#' start. Therefore, all timestamps should be reset during data import by
#' subtracting the value of the first timestamp from all timestamps within a
#' trial (assuming that the first timestamp corresponds to the time when
#' tracking started). Timestamps are reset by default when importing the data
#' using one of the mt_import functions (e.g., \link{mt_import_mousetrap}). Note
#' that \code{initiation_time} is defined as the last timestamp before the
#' \code{initiation_threshold} was crossed.
#' 
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
#'   flip. If several thresholds are specified, flips will be returned in
#'   separate variables for each threshold value (the variable name will be
#'   suffixed with the threshold value).
#' @param hover_threshold an optional numeric value. If specified, \code{hovers}
#'   (and \code{hover_time})  will be calculated as the number (and total time)
#'   of periods without movement in a trial (whose duration exceeds the value
#'   specified in \code{hover_threshold}). If several thresholds are specified,
#'   hovers and hover_time will be returned in separate variables for each
#'   threshold value (the variable name will be suffixed with the threshold
#'   value).
#' @param hover_incl_initial logical indicating if the calculation of hovers
#'   should include a potential initial phase in the trial without mouse
#'   movements (this initial phase is included by default).
#' @param initiation_threshold a numeric value specifying the distance from the
#'   start point of the trajectory that needs to be exceeded for calculating the
#'   initiation time. By default, it is 0, meaning that any movement counts as
#'   movement initiation.
#'   
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
#'   specified in \code{dimensions}). Please note that additional information is
#'   provided in the Details section.
#'
#'   \item{mt_id}{Trial ID (can be used for merging measures data.frame with
#'   other trial-level data)}
#'   \item{xpos_max}{Maximum x-position} 
#'   \item{xpos_min}{Minimum x-position}
#'   \item{ypos_max}{Maximum y-position} 
#'   \item{ypos_min}{Minimum y-position}
#'   \item{MAD}{Signed Maximum absolute deviation from the direct path
#'   connecting start and end point of the trajectory (straight line).
#'   If the \code{MAD} occurs above the direct path, this is denoted by
#'   a positive value; if it occurs below, by a negative value.}
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
#'   \item{xpos_flips}{Number of directional changes along x-axis (exceeding the
#'   distance specified in \code{flip_threshold})}
#'   \item{ypos_flips}{Number of directional changes along y-axis (exceeding the
#'   distance specified in \code{flip_threshold})}
#'   \item{xpos_reversals}{Number of crossings of the y-axis} 
#'   \item{ypos_reversals}{Number of crossings of the x-axis}
#'   \item{RT}{Response time, time at which tracking stopped}
#'   \item{initiation_time}{Time at which first mouse movement was initiated}
#'   \item{idle_time}{Total time without mouse movement across the entirety of
#'   the trial}
#'   \item{hover_time}{Total time of all periods without movement in a trial 
#'   (whose duration exceeds the value specified in \code{hover_threshold})} 
#'   \item{hovers}{Number of periods without movement in a trial (whose duration
#'   exceeds the value specified in \code{hover_threshold})}
#'   \item{total_dist}{Total distance covered by the trajectory}
#'   \item{vel_max}{Maximum velocity}
#'   \item{vel_max_time}{Time at which maximum velocity occurred first}
#'   \item{vel_min}{Minimum velocity} 
#'   \item{vel_min_time}{Time at which minimum velocity occurred first} 
#'   \item{acc_max}{Maximum acceleration}
#'   \item{acc_max_time}{Time at which maximum acceleration occurred first}
#'   \item{acc_min}{Minimum acceleration} 
#'   \item{acc_min_time}{Time at which minimum acceleration occurred first} 
#'   
#' @references Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to
#'   implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J.
#'   G. Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp.
#'   111-130). New York, NY: Routledge.
#'
#'   Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for studying 
#'   real-time mental processing using a computer mouse-tracking method. 
#'   \emph{Behavior Research Methods, 42}(1), 226-241.
#'   
#'   
#'   
#' @seealso \link{mt_sample_entropy} for calculating sample entropy.
#'
#'   \link{mt_standardize} for standardizing the measures per subject.
#'
#'   \link{mt_check_bimodality} for checking bimodality of the measures using
#'   different methods.
#'
#'   \link{mt_aggregate} and \link{mt_aggregate_per_subject} for aggregating the
#'   measures.
#'
#'   \link[dplyr:mutate-joins]{inner_join} for merging data using the \code{dplyr} package.
#' 
#' 
#' @examples
#' mt_example <- mt_derivatives(mt_example)
#' mt_example <- mt_deviations(mt_example)
#' mt_example <- mt_measures(mt_example)
#' 
#' # Merge measures with trial data
#' mt_example_results <- dplyr::inner_join(
#'   mt_example$data, mt_example$measures,
#'   by="mt_id")
#'   
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#' 
#' @export
mt_measures <- function(
  data,
  use="trajectories", save_as="measures",
  dimensions=c("xpos","ypos"), timestamps="timestamps",
  flip_threshold=0, hover_threshold=NULL, hover_incl_initial=TRUE,
  initiation_threshold=0,
  verbose=FALSE) {
  
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
  nlogs <- mt_count(trajectories,dimensions=dim1)
  
  
  # Calculate deviations if no deviations were found in the data
  if (!all(c(dev_ideal,dim1_ideal,dim2_ideal) %in% dimnames(trajectories)[[3]])) {
    if (verbose) {
      message("Start calculating deviations of actual from idealized response trajectory.")
    }
    trajectories <- mt_deviations(
      data=trajectories, verbose=verbose
    )
    if (verbose) {
      message("Start calculating mouse-tracking measures.")
    }
  }

  # Setup variable matrix depending on whether timestamps are provided or not
  if (timestamps %in% dimnames(trajectories)[[3]]) {
    mt_measures <- c(
      paste0(dim1,c("_max","_min")),paste0(dim2,c("_max","_min")),
      "MAD", "MAD_time",
      "MD_above", "MD_above_time",
      "MD_below", "MD_below_time",
      "AD", "AUC"
    )
    
    if (length(flip_threshold)==1){
      mt_measures <- c(
        mt_measures,
        paste0(dimensions,"_flips")
      )
    } else {
      mt_measures <- c(mt_measures, paste(paste0(dimensions,"_flips"),rep(flip_threshold,each=2),sep="_"))
    }
    
    mt_measures <- c(
      mt_measures,
      paste0(dimensions,"_reversals"),
      "RT", "initiation_time", "idle_time"
    )
    
    if(!is.null(hover_threshold)){
      if (length(hover_threshold)==1){
        mt_measures <- c(mt_measures, "hover_time", "hovers")
      } else {
        mt_measures <- c(mt_measures, paste(c("hover_time", "hovers"),rep(hover_threshold,each=2),sep="_"))
      }
    }
    
    # Check if there are trajectories where first timestamp is > 0:
    if (max(trajectories[,1,timestamps]) > 0) {
      message(
        "Trajectories detected where first timestamp is greater than 0. ",
        "Please see Details section of mt_measures documentation ",
        "for interpretation of time related measures."
      )
    } 
    
    # Check if there are trajectories where first timestamp is < 0:
    if (min(trajectories[,1,timestamps]) < 0) {
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
      "AD", "AUC"
    )
    
    if (length(flip_threshold)==1){
      mt_measures <- c(
        mt_measures,
        paste0(dimensions,"_flips")
      )
    } else {
      mt_measures <- c(mt_measures, paste(paste0(dimensions,"_flips"),rep(flip_threshold,each=2),sep="_"))
    }
    
    mt_measures <- c(
      mt_measures,
      paste0(dimensions,"_reversals")
    )
    
  }
  
  # Add distance, velocity and acceleration-based measures
  # if the derivatives were precomputed
  if (dist %in% dimnames(trajectories)[[3]]) {
    mt_measures <- c(mt_measures, "total_dist")
  }
  
  if (vel %in% dimnames(trajectories)[[3]] & timestamps %in% dimnames(trajectories)[[3]]) {
    mt_measures <- c(mt_measures, 
      "vel_max", "vel_max_time", "vel_min", "vel_min_time")
  }
  
  if (acc %in% dimnames(trajectories)[[3]] & timestamps %in% dimnames(trajectories)[[3]]) {
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
    current_dim1 <- trajectories[i, 1:current_nlogs, dim1]
    current_dim2 <- trajectories[i, 1:current_nlogs, dim2]
    current_dim1_ideal <- trajectories[i, 1:current_nlogs, dim1_ideal]
    current_dim2_ideal <- trajectories[i, 1:current_nlogs, dim2_ideal]
    current_dev_ideal <- trajectories[i, 1:current_nlogs, dev_ideal]
    
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
    measures[i,grep(paste0(dim1,"_flips"),mt_measures)] <-
      sapply(flip_threshold,count_changes,pos=current_dim1)
    measures[i,grep(paste0(dim2,"_flips"),mt_measures)] <-
      sapply(flip_threshold,count_changes,pos=current_dim2)

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
    if (timestamps %in% dimnames(trajectories)[[3]]) {
      
      current_timestamps <- trajectories[i,1:current_nlogs,timestamps]
      
      measures[i,"RT"] <- current_timestamps[current_nlogs]
      
      dist_from_start <- sqrt((current_dim1-current_dim1[1])^2+(current_dim2-current_dim2[1])^2)
      measures[i,"initiation_time"] <- current_timestamps[which(dist_from_start>initiation_threshold)[1]-1]
      if (is.na(measures[i,"initiation_time"])){
        measures[i,"initiation_time"] <- measures[i,"RT"]
      } 
      
      # Calculate variables for phases with and without movement
      time_diffs <- diff(current_timestamps)
      # Indicate for each sample whether the position changed
      pos_constant <- (diff(current_dim1) == 0) & (diff(current_dim2) == 0)

      if (all(pos_constant == FALSE)) {
        # Continuous movement
        measures[i,"idle_time"] <- current_timestamps[1]
        if (!is.null(hover_threshold)){
          measures[i,grep("hover_time",mt_measures)] <- 
            ifelse(current_timestamps[1]>hover_threshold,current_timestamps[1],0)
          
          measures[i,grep("hovers",mt_measures)] <- 0
        }
        
      } else if (all(pos_constant == TRUE)) {
        # No movement at all
        measures[i,"idle_time"] <- measures[i,"RT"]
        if (!is.null(hover_threshold)){
          
          measures[i,grep("hover_time",mt_measures)] <- 
            ifelse(measures[i,"RT"]>hover_threshold,measures[i,"RT"],0)
          
          measures[i,grep("hovers",mt_measures)] <- 
            ifelse(measures[i,"RT"]>hover_threshold,1,0)
        }
        
      } else {
        # Intermittent movement
        
        measures[i,"idle_time"] <- sum(time_diffs[pos_constant])
        
        
        if (!is.null(hover_threshold)){
          # Calculate hovers
          
          # Set time diffs for periods with movement to 0
          time_diffs[!pos_constant] <- 0
          
          # Retrieve for each period without movement the last cumulated timestamp
          time_diffs <- cumsum(time_diffs)[c(diff(pos_constant)==(-1),pos_constant[length(pos_constant)])]
          
          # Calculate the duration for each period as the difference of timestamps
          if(length(time_diffs)>1) {
            time_diffs <- c(time_diffs[1],diff(time_diffs))
          }
          
          # Calculate number of periods without movement that exceed the threshold and their total time
          measures[i,grep("hover_time",mt_measures)] <-
            sapply(hover_threshold,function(threshold) sum(time_diffs[time_diffs>threshold]))
          
          measures[i,grep("hovers",mt_measures)] <-
            sapply(hover_threshold, function(threshold) sum(time_diffs>threshold))
          
        }
      
      }
      
      if (!is.null(hover_threshold) & (hover_incl_initial==FALSE)){
        
        measures[i,grep("hover_time",mt_measures)] <- measures[i,grep("hover_time",mt_measures)]-
          ifelse(measures[i,"initiation_time"]>hover_threshold,measures[i,"initiation_time"],0)
        
        measures[i,grep("hovers",mt_measures)] <- measures[i,grep("hovers",mt_measures)]-
          ifelse(measures[i,"initiation_time"]>hover_threshold,1,0)
        
      }
      
      
      # notes: timestamps (e.g., for MAD) always correspond
      # to the first time the max/min value was reached
      measures[i,"MAD_time"] <- current_timestamps[which.max(abs(current_dev_ideal))]
      measures[i,"MD_above_time"] <- current_timestamps[which.max(current_dev_ideal)]
      measures[i,"MD_below_time"] <- current_timestamps[which.min(current_dev_ideal)]
      
    }
    
    # Compute total distance covered
    if (dist %in% dimnames(trajectories)[[3]]) {
      measures[i,"total_dist"] <- sum(abs(trajectories[i,,dist]), na.rm=TRUE)
    }
    
    # Velocity-based measures
    if (vel %in% dimnames(trajectories)[[3]] & timestamps %in% dimnames(trajectories)[[3]]) {

      # Maximum velocity
      measures[i,"vel_max"] <- max(trajectories[i,,vel], na.rm=TRUE)
      vel_max_pos <- which.max(trajectories[i,,vel])
      
      # Interpolate timestamp of maximum velocity
      # (see mt_derivatives for logic of velocity timestamps)
      vel_max_pos <- ifelse(vel_max_pos==1, 1, c(vel_max_pos-1, vel_max_pos))
      measures[i,"vel_max_time"] <- mean(trajectories[i,vel_max_pos,timestamps])
      
      # Minimum velocity (which is treated analogously)
      measures[i,"vel_min"] <- min(trajectories[i,,vel], na.rm=TRUE)
      vel_min_pos <- which.min(trajectories[i,,vel])
      
      # average timestamps (see mt_derivatives for logic of velocity timestamps)
      vel_min_pos <- ifelse(
        vel_min_pos == 1,
        1, c(vel_min_pos-1, vel_min_pos)
      )
      measures[i,"vel_min_time"] <- mean(trajectories[i,vel_min_pos,timestamps])
    }

    if (acc %in% dimnames(trajectories)[[3]] & timestamps %in% dimnames(trajectories)[[3]]) {
      # Maximum acceleration
      measures[i,"acc_max"] <- max(trajectories[i,,acc], na.rm=TRUE)
      measures[i,"acc_max_time"] <- trajectories[i,which.max(trajectories[i,,acc]),timestamps]
      
      # Minimum acceleration
      measures[i,"acc_min"] <- min(trajectories[i,,acc], na.rm=TRUE)
      measures[i,"acc_min_time"] <- trajectories[i,which.min(trajectories[i,,acc]),timestamps]
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
