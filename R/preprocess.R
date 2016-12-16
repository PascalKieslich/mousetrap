#' Time normalize trajectories.
#'
#' Compute time-normalized trajectories using a constant number of equally sized
#' time steps. Time normalization is performed separately for all specified
#' trajectory dimensions (by default, the x- and y-positions) using linear
#' interpolation based on the timestamps. By default, 101 time steps are used
#' (following Spivey et al., 2005).
#'
#' Time-normalization is often performed if the number of recorded x- and
#' y-positions varies across trajectories, which typically occurs when
#' trajectories vary in their response time. After time-normalization, all
#' trajectories have the same number of recorded positions (which is specified
#' using \code{nsteps}) and the positions at different relative time points can
#' be compared across trajectories.
#'
#' For example, time normalized trajectories can be compared across conditions
#' that differed in their overall response time, as the timestamps are now
#' relative to the overall trial duration. This is also helpful for creating
#' average trajectories, which are often used in plots.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory
#'   array can be provided directly (in this case \code{use} will be ignored).
#' @param use a character string specifying which trajectory data should be
#'   used.
#' @param save_as a character string specifying where the resulting trajectory
#'   data should be stored.
#' @param dimensions a character vector specifying the dimensions in the
#'   trajectory array that should be time-normalized. If \code{"all"}, all
#'   trajectory dimensions except the timestamps will be time-normalized.
#' @param timestamps a character string specifying the trajectory dimension
#'   containing the timestamps.
#' @param nsteps an integer specifying the number of equally sized time steps.
#' @param verbose logical indicating whether function should report its
#'   progress.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   array (by default called \code{tn_trajectories}) containing the
#'   time-normalized trajectories. In this array, another dimension (called
#'   \code{steps}) has been added with increasing integer values indexing the
#'   time-normalized position. If a trajectory array was provided directly as
#'   \code{data}, only the time-normalized trajectories will be returned.
#'
#' @references Spivey, M. J., Grosjean, M., & Knoblich, G. (2005). Continuous
#'   attraction toward phonological competitors. \emph{Proceedings of the
#'   National Academy of Sciences of the United States of America, 102}(29),
#'   10393-10398.
#'
#' @seealso \link[stats]{approx} for information about the function used for
#'   linear interpolation.
#'
#' \link{mt_resample} for resampling trajectories using a constant time
#' interval.
#'
#' @examples
#' mt_example <- mt_time_normalize(mt_example,
#'   save_as="tn_trajectories", nsteps=101)
#'
#' @export
mt_time_normalize <- function(data,
                              use="trajectories", save_as="tn_trajectories",
                              dimensions=c("xpos","ypos"), timestamps="timestamps",
                              nsteps=101,
                              verbose=FALSE) {
  
  if (length(dimensions) == 1 & dimensions[[1]] == "all") {
    dimensions <- dimnames(trajectories)[[3]]
    dimensions <- dimensions[dimensions!=timestamps]
  }

  # Preparation
  trajectories <- extract_data(data=data,use=use)

  # Create empty array for output
  tn_trajectories <- array(
    dim=c(nrow(trajectories), nsteps, 2+length(dimensions)),
    dimnames=list(
      dimnames(trajectories)[[1]],
      NULL,
      c(timestamps, dimensions, "steps")
    )
  )

  # Perform time normalization
  for (i in 1:nrow(trajectories)) {
    # The approx() function performs linear interpolation
    # for coordinates.

    # Timestamps
    tn_trajectories[i,,timestamps] <- stats::approx(
      trajectories[i,,timestamps], trajectories[i,,timestamps], n=nsteps)$y

    # Specified trajectory dimensions
    for (dimension in dimensions) {
      tn_trajectories[i,,dimension] <- stats::approx(
        trajectories[i,,timestamps], trajectories[i,,dimension], n=nsteps)$y
    }

    # Label steps as such
    tn_trajectories[i,,"steps"] <- 1:nsteps

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  return(create_results(data=data, results=tn_trajectories, use=use, save_as=save_as))
}


#' Remap mouse trajectories.
#'
#' Remap all trajectories to one side (or one quadrant) of the coordinate
#' system. In doing so, \code{mt_remap_symmetric} assumes a centered coordinate
#' system and a symmetric design of the response buttons (see Details).
#'
#' When mouse trajectories are compared across different conditions, it is
#' typically desirable that the endpoints of the trajectories share the same
#' direction (e.g., diagonally up and left). This way, the trajectories can be
#' compared regardless of the button they were directed at.
#'
#' \code{mt_remap_symmetric} can be used to achieve this provided that two
#' assumptions hold:
#'
#' First, this function assumes a centered coordinate system, i.e. the
#' coordinate system is centered on the screen center. This is the case when the
#' data is produced by the mousetrap plug-ins in OpenSesame.
#'
#' Second, it assumes that the response buttons in the mouse-tracking experiment
#' are symmetric, in that they all are equally distant from the screen center.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the two dimensions in the
#'   trajectory array that contain the mouse positions, the first value
#'   corresponding to the x-positions, the second to the y-positions.
#' @param remap_xpos character string indicating the direction in which to remap
#'   values on the x axis. If set to "left" (as per default), trajectories with
#'   an endpoint on the right (i.e. with a positive x-value) will be remapped to
#'   the left. The alternatives are "right" which has the reverse effect, and
#'   "no", which disables remapping on the horizontal dimension.
#' @param remap_ypos character string defining whether tracks directed downwards
#'   on the y axis should be remapped so that they end with a positive y value.
#'   This will be performed if this parameter is set to "up" (which is the
#'   default), and the reverse occurs if the parameter is set to "down". If it
#'   is set to "no", y-values remain untouched.
#'
#' @return A mousetrap data object (see \link{mt_example}) with remapped
#'   trajectories. If the trajectory array was provided directly as \code{data},
#'   only the trajectory array will be returned.
#'
#' @examples
#' # Remap trajectories so that all trajectories
#' # end in the top-left corner
#' mt_example <- mt_import_mousetrap(mt_example_raw)
#' mt_example <- mt_remap_symmetric(mt_example)
#'
#' # Only flip trajectories vertically so that all
#' # trajectories end in the upper half of the screen
#' mt_example <- mt_import_mousetrap(mt_example_raw)
#' mt_example <- mt_remap_symmetric(mt_example,
#'   remap_xpos="no", remap_ypos="up")
#'
#' @export
mt_remap_symmetric <- function(
  data,
  use="trajectories", save_as=use,
  dimensions=c("xpos","ypos"),
  remap_xpos='left', remap_ypos='up') {

  # Data setup
  trajectories <- extract_data(data=data,use=use)
  xpos <- dimensions[[1]]
  ypos <- dimensions[[2]]

  # Argument checking
  if (!(remap_xpos %in% c('left', 'right', 'no'))) {
    stop('Invalid value in remap_xpos argument')
  }
  if (!(remap_ypos %in% c('up', 'down', 'no'))) {
    stop('Invalid value in remap_ypos argument')
  }

  # Remap values
  for (i in 1:nrow(trajectories)) {
    # Determine the length (in samples) of all trajectories
    nlogs <- sum(!is.na(trajectories[i, , xpos]))

    # Remap x values (if desired)
    if (remap_xpos != 'no') {
      if (
        # Remap tracks that are headed in the undesired
        # direction (as measured by their endpoint) ...
        (remap_xpos == 'left'  & trajectories[i, nlogs, xpos] > 0) |
        (remap_xpos == 'right' & trajectories[i, nlogs, xpos] < 0)
      ) {
        # ... by reversing the x coordinate
        trajectories[i, , xpos] <- (-trajectories[i, , xpos])
      }
    }

    # Do likewise for y values
    if (remap_ypos != 'no') {
      if (
        (remap_ypos == 'up'   & trajectories[i, nlogs, ypos] < 0) |
        (remap_ypos == 'down' & trajectories[i, nlogs, ypos] > 0)
      ) {
        trajectories[i, , ypos] <- (-trajectories[i, , ypos])
      }
    }

  }

  return(create_results(data=data, results=trajectories, use=use, save_as=save_as))
}


#' Exclude initial phase without mouse movement.
#'
#' Exclude the initial phase in a trial where the mouse was not moved. The
#' corresponding samples (x- and y-positions and timestamps) in the trajectory
#' data will be removed.
#'
#' \code{mt_exclude_initiation} removes all samples (x- and y-positions as well
#' as timestamps) at the beginning of the trial during which the mouse was not
#' moved from its initial position. The last unchanged sample is retained in the
#' data.
#'
#' If \code{reset_timestamps == TRUE} (the default), it subtracts the last
#' timestamp before a movement occurs from all timestamps , so that the series
#' of timestamps once more begin with zero. If the argument is set to
#' \code{FALSE}, the values of the timestamps are unchanged.
#'
#' Please note that resetting the timestamps will result in changes in several
#' mouse-tracking measures, notably those which report timestamps (e.g.,
#' \code{MAD_time}). Typically, however, these changes are desired when using
#' this function.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the dimensions in the
#'   trajectory array that contain the mouse positions.
#' @param reset_timestamps logical indicating whether the timestamps should be
#'   reset after removing the initial phase without movement (see Details).
#'
#' @return A mousetrap data object (see \link{mt_example}) from which the
#'   initial phase without mouse movement was removed. If the trajectory array
#'   was provided directly as \code{data}, only the trajectory array will be
#'   returned.
#'
#' @seealso \link{mt_measures} for calculating the initiation time.
#'
#' @examples
#' mt_example <- mt_exclude_initiation(mt_example,
#'   save_as="mod_trajectories")
#'
#' @export
mt_exclude_initiation <- function(data,
  use="trajectories", save_as=use,
  dimensions=c("xpos","ypos"), timestamps="timestamps",
  reset_timestamps=TRUE,
  verbose=FALSE) {
  
  # Gather necessary data
  trajectories <- extract_data(data=data, use=use)

  # Only keep relevant dimensions
  trajectories <- trajectories[,,c(timestamps, dimensions),drop=FALSE]

  # Calculate number of logs
  nlogs <- rowSums(!is.na(trajectories[, ,timestamps , drop=FALSE]))

  # Exclude phase where mouse stayed on start coordinates
  for (i in 1:nrow(trajectories)) {

    # Extract trajectory data
    current_trajectories <- trajectories[i, 1:nlogs[i],]

    # Iterate over trajectories
    current_timestamps <- current_trajectories[,timestamps]
    current_points <- current_trajectories[,dimensions,drop=FALSE]

    # Vector indicating if mouse has not left the starting point
    on_start <- cumsum(rowSums(abs(current_points - current_points[1,]))) == 0

    # Change last element where mouse is still on starting point so that this
    # point is included in the calculations
    on_start[sum(on_start, na.rm=TRUE)] <- FALSE

    # Exclude data without movements
    current_timestamps <- current_timestamps[!on_start]
    current_points <- current_points[!on_start,]

    # Clear data in array
    trajectories[i,,] <- NA

    # Add data to array
    trajectories[i, 1:length(current_timestamps), timestamps] <- current_timestamps
    trajectories[i, 1:length(current_timestamps), dimensions] <- current_points

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  # Reset timestamps (optional)
  if (reset_timestamps) {
    trajectories[,,timestamps] <- trajectories[, , timestamps] - trajectories[, 1, timestamps]
  }

  return(create_results(data=data, results=trajectories, use=use, save_as=save_as))
}


#' Space normalize trajectories.
#'
#' Adjust trajectories so that all trajectories have an identical start and end
#' point. If no end points are provided, trajectories are only adjusted so that
#' they have the same start position.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the dimensions in the
#'   trajectory array that should be space-normalized.
#' @param start a numeric vector specifying the start values for each dimension,
#'   i.e., the values the first recorded position should have in every trial.
#' @param end a numeric vector specifying the end values for each dimension,
#'   i.e., the values the last recorded position should have in every trial. If
#'   \code{NULL}, trajectories are only adjusted so that they have the same
#'   start position.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   array (by default called \code{sn_trajectories}) containing the
#'   space-normalized trajectories. All other trajectory dimensions not
#'   specified in \code{dimensions} (e.g., timestamps) will be kept as is in the
#'   resulting trajectory array. If a trajectory array was provided directly as
#'   \code{data}, only the space-normalized trajectories will be returned.
#'
#' @references Dale, R., Kehoe, C., & Spivey, M. J. (2007). Graded motor
#'   responses in the time course of categorizing atypical exemplars.
#'   \emph{Memory & Cognition, 35}(1), 15-28.
#'
#' @seealso \link{mt_align_start} for aligning the start position of
#'   trajectories.
#'
#'   \link{mt_remap_symmetric} for remapping trajectories.
#'
#' @examples
#' mt_example <- mt_space_normalize(mt_example,
#'   save_as ="sn_trajectories",
#'   start=c(0,0), end=c(-1,1))
#'
#' @export
mt_space_normalize <- function(
  data,
  use="trajectories", save_as="sn_trajectories",
  dimensions=c("xpos", "ypos"),
  start=c(0, 0), end=NULL,
  verbose=FALSE) {
  
  # Preparation
  trajectories <- extract_data(data=data,use=use)

  # Perform space normalization
  for (i in 1:nrow(trajectories)) {
    for (j in 1:length(dimensions)) {

      current_positions <- trajectories[i, , dimensions[[j]]]
      nlogs <- sum(!is.na(current_positions))

      current_positions <- current_positions - current_positions[1]
      if (!is.null(end)) {
        current_positions <- current_positions / (current_positions[nlogs] - current_positions[1])
        current_positions <- current_positions * (end[[j]]-start[[j]])
      }
      trajectories[i, , dimensions[[j]]] <-  current_positions + start[[j]]

    }

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  return(create_results(data=data, results=trajectories, use=use, save_as=save_as))
}


#' Align start position of trajectories.
#'
#' Adjust trajectories so that all trajectories have the same start position.
#'
#' @inheritParams mt_space_normalize
#'
#' @return A mousetrap data object (see \link{mt_example}) with aligned
#'   trajectories. If the trajectory array was provided directly as \code{data},
#'   only the trajectory array will be returned.
#'
#' @seealso \link{mt_space_normalize} for space normalizing trajectories.
#'
#' \link{mt_remap_symmetric} for remapping trajectories.
#'
#' @examples
#' mt_example <- mt_align_start(mt_example,
#'   start=c(0,0))
#'
#' @export
mt_align_start <- function(
  data,
  use="trajectories", save_as="trajectories",
  dimensions=c("xpos","ypos"), start=c(0,0),
  verbose=FALSE) {

  return(
    mt_space_normalize(
      data=data, use=use, save_as=save_as,
      start=start, end=NULL,
      verbose=verbose
    )
  )
}


#' Resample trajectories using a constant time interval.
#'
#' Resample trajectory positions using a constant time interval. If no timestamp
#' that represents an exact multiple of this time interval is found, linear
#' interpolation is performed using the two adjacent timestamps.
#'
#' \code{mt_resample} can be used if the number of logged positions in a trial
#' should be reduced. \code{mt_resample} achieves this by artificially
#' decreasing the resolution with which the positions were recorded. For
#' example, if mouse positions were recorded every 10 ms in an experiment, but
#' one was only interested in the exact mouse position every 50 ms,
#' \code{mt_resample} with \code{step_size=50} could be used. In this case, only
#' every fifth sample would be kept.
#'
#' In addition, \code{mt_resample} can be used to only retain values for
#' specific timestamps across trials (e.g., if for each trial the position of
#' the mouse exactly 250 ms and 500 ms after onset of the trial are of
#' interest). In case that a trial does not contain samples at the specified
#' timestamps, linear interpolation is performed using the two adjacent
#' timestamps.
#'
#' Note that \code{mt_resample} does not average across time intervals. For
#' this, \link{mt_average} can be used.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the dimensions in the
#'   trajectory array that should be resampled. If \code{"all"}, all trajectory
#'   dimensions except the timestamps will be resampled.
#' @param step_size an integer specifying the size of the constant time
#'   interval. The unit corresponds to the unit of the timestamps.
#' @param exact_last_timestamp logical indicating if the last timestamp should
#'   always be appended (which is the case by default). If \code{FALSE}, the
#'   last timestamp is only appended if it is a multiple of the step_size.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   array (by default called \code{rs_trajectories}) containing the resampled
#'   trajectories. If a trajectory array was provided directly as \code{data},
#'   only the resampled trajectories will be returned.
#'
#' @seealso \link[stats]{approx} for information about the function used for
#' linear interpolation.
#'
#' \link{mt_average} for averaging trajectories across constant time intervals.
#'
#' \link{mt_time_normalize} for time-normalizing trajectories.
#'
#' @examples
#' mt_example <- mt_resample(mt_example,
#'   save_as="rs_trajectories",
#'   step_size=50)
#'
#' @export
mt_resample <- function(data,
  use="trajectories", save_as="rs_trajectories",
  dimensions=c("xpos", "ypos"), timestamps="timestamps",
  step_size=10, exact_last_timestamp=TRUE,
  verbose=FALSE) {
  
  if (length(dimensions) == 1 & dimensions[[1]] == "all") {
    dimensions <- dimnames(trajectories)[[3]]
    dimensions <- dimensions[dimensions != timestamps]
  }

  # Preparation
  trajectories <- extract_data(data=data, use=use)

  # Calculate the number of steps after resampling
  max_steps <- ceiling(
    max(trajectories[,,timestamps], na.rm=TRUE) / step_size
  ) + 1

  # Create an empty output array
  rs_trajectories <- array(
    dim=c(nrow(trajectories), max_steps, 1+length(dimensions)),
    dimnames=list(
      dimnames(trajectories)[[1]],
      NULL,
      c(timestamps, dimensions)
    )
  )

  # Check if there are trajectories where first timestamp is > 0:
  if (max(trajectories[,1,timestamps]) > 0) {
    message(
      "Trajectories detected where first timestamp is greater than 0. ",
      "Assuming period without movement starting at timestamp 0."
    )
  }

  # Perform downsampling
  for (i in 1:nrow(trajectories)) {
    current_trajectories <- trajectories[i,,]
    current_timestamps <- current_trajectories[,timestamps]
    nlogs <- sum(!is.na(current_timestamps))

    # If first timestamp is > 0, add another with
    # a timestamp of zero and the first recorded position
    if (current_timestamps[1] > 0) {
      current_timestamps <- c(0, current_timestamps)
      current_trajectories <- rbind(current_trajectories[1,], current_trajectories)
      nlogs <- nlogs + 1
    }

    current_timestamps <- current_timestamps[1:nlogs]
    current_trajectories <- current_trajectories[1:nlogs,]
    max_time <- current_timestamps[nlogs]

    # Generate new timestamps
    custom_timesteps <- seq(current_timestamps[1], current_timestamps[nlogs], by=step_size)

    # If last timestamp should always be appended, insert correct value
    if (max_time %% step_size != 0 & exact_last_timestamp) {
      custom_timesteps <- c(custom_timesteps, current_timestamps[nlogs])
    }

    # Perform linear interpolation using custom steps
    int_timestamps <- stats::approx(current_timestamps, current_timestamps, xout=custom_timesteps)$y
    rs_trajectories[i,1:length(int_timestamps),timestamps] <- int_timestamps

    # Perform linear interpolation for specified trajectory dimensions
    for (dimension in dimensions) {
      rs_trajectories[i,1:length(int_timestamps),dimension] <- stats::approx(
        current_timestamps, current_trajectories[,dimension], xout=custom_timesteps)$y
    }

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  return(create_results(data=data, results=rs_trajectories, use=use, save_as=save_as))
}

#' Average trajectories across intervals.
#'
#' Average trajectory data across specified intervals (e.g., constant time
#' intervals). For every specified dimension in the trajectory array (by
#' default, every dimension, i.e., x- and y-position, possibly also velocity and
#' acceleration etc.), the mean value for the respective interval is calculated
#' (see Details for information regarding the exact averaging procedure).
#'
#' For each interval, it is first determined which of the values lie within the
#' respective interval of the dimension used for averaging (e.g., timestamps).
#' Intervals are left-open, right-closed (e.g., if values are averaged across
#' constant timestamps of 100 ms, a timestamp of 1200 would be included in the
#' interval 1100-1200 while a timestamp of 1300 would be included in the
#' interval 1200-1300). Then, all values for which the corresponding average
#' dimension values lie within the interval are averaged.
#'
#' In case the last interval is not fully covered (e.g., if the last timestamp
#' has the value 1250), values for the corresponding interval (1200-1300) will
#' be computed based on the average of the values up to the last existing value.
#'
#' Note that \code{mt_average} assumes that the trajectory variables are
#' recorded with a constant sampling rate (i.e., with a constant difference in
#' the timestamps). If the sampling rate varies considerably, \link{mt_resample}
#' should be called before averaging to arrive at equally spaced timestamps. The
#' sampling rate can be investigated using \link{mt_check_resolution}.
#'
#' If average velocity and acceleration are of interest,
#' \link{mt_derivatives} should be called before averaging.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the dimensions in the
#'   trajectory array that should be averaged. By default (\code{"all"}), all
#'   trajectory dimensions will be averaged.
#' @param av_dimension a character string specifying which values should be used
#'   for determining the intervals for averaging (\code{"timestamps"} by
#'   default).
#' @param intervals an optional numeric vector. If specified, these values are
#'   taken as the borders of the intervals (\code{interval_size} and
#'   \code{max_interval} are ignored).
#' @param interval_size an integer specifying the size of the constant dimension
#'   interval.
#' @param max_interval an integer specifying the upper limit of the last
#'   dimension value that should be included (therefore, it should be a multiple
#'   of the \code{interval_size}). If specified, only values will be used for
#'   averaging where the dimension values are smaller than \code{max_interval}.
#'   If unspecified (the default), all values will be included.
#' @param dimension Deprecated. Please use \code{av_dimension} instead.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   array (by default called \code{av_trajectories}) that contains the average
#'   trajectory data per dimension interval. If a trajectory array was provided
#'   directly as \code{data}, only the average trajectories will be returned.
#'
#'   For the dimension values used for averaging (specified in
#'   \code{av_dimension}), the mid point of the respective interval is reported,
#'   which is helpful for plotting the trajectory data later on. However, this
#'   value does not necessarily correspond to the empirical mean of the
#'   dimension values in the interval.
#'
#' @seealso \link{mt_derivatives} for calculating velocity and
#'   acceleration.
#'
#' \link{mt_resample} for resampling trajectories using a constant time
#' interval.
#'
#' @examples
#' mt_example <- mt_derivatives(mt_example)
#'
#' # average trajectories across 100 ms intervals
#' mt_example <- mt_average(mt_example, save_as="av_trajectories",
#'   interval_size=100)
#'
#' # average time-normalized trajectories across specific intervals
#' # of the time steps
#' mt_example <- mt_time_normalize(mt_example)
#' mt_example <- mt_average(mt_example,
#'   use="tn_trajectories", save_as="av_tn_trajectories",
#'   av_dimension = "steps", intervals = c(0.5,33.5,67.5,101.5))
#'
#' @export
mt_average <- function(data,
  use="trajectories", save_as="av_trajectories",
  dimensions="all", av_dimension="timestamps",
  intervals=NULL, interval_size=100, max_interval=NULL,
  verbose=FALSE,
  dimension=NULL) {
  
  if (is.null(dimension) == FALSE) {
    warning(
      "The argument dimension is deprecated. ",
      "Please use av_dimension instead.",
      call.=FALSE
    )
    av_dimension <- dimension
  }

  trajectories <- extract_data(data=data,use=use)

  if (!av_dimension %in% dimnames(trajectories)[[3]]) {
    stop("Dimension '",av_dimension,"' not found in trajectory array.")
  }

  if (length(dimensions) == 1 & dimensions[[1]] == "all") {
    dimensions <- dimnames(trajectories)[[3]]
    dimensions <- dimensions[dimensions!=av_dimension]
  }

  if (is.null(intervals)) {
    # Compute the maximum number of possible intervals
    if (is.null(max_interval)) {
      # Determine this number automatically based on
      # the given interval size
      max_n_intervals <- ceiling(
        max(trajectories[,,av_dimension], na.rm=TRUE) / interval_size
      )

    } else {
      # If trajectories are truncated at max_interval,
      # calculate the number of steps up to this point
      if(max_interval %% interval_size != 0) {
        warning("max_interval is not a multiple of interval_size.")
      }
      max_n_intervals <- ceiling(max_interval / interval_size)

    }

    interval_sizes <- rep(interval_size, max_n_intervals)

  } else {
    max_n_intervals <- length(intervals)-1
    max_interval <- intervals[length(intervals)]
    interval_sizes <- diff(intervals)
  }

  # Create an empty output array
  av_trajectories <- array(
    dim=c(nrow(trajectories), max_n_intervals, 1+length(dimensions)),
    dimnames=list(
      dimnames(trajectories)[[1]],
      NULL,
      c(av_dimension,dimensions)
    )
  )

  for (i in 1:nrow(trajectories)) {

    current_av_values <- trajectories[i,,av_dimension]
    nlogs <- sum(!is.na(current_av_values))
    current_av_values <- current_av_values[1:nlogs]

    if (!is.null(max_interval)) {
      # In case an upper interval limit is set
      # only keep values up to the maximum interval
      if (current_av_values[nlogs]>max_interval) {
        nlogs <- sum(current_av_values <= max_interval)
        current_av_values <- current_av_values[1:nlogs]
      }
    }

    # Set lower borders
    if (is.null(intervals)) {
      # Subtract small number from last value as intervals are right-closed
      lower_borders <- seq(0, current_av_values[nlogs]-1e-6, interval_size)
    } else {
      lower_borders <- intervals[-length(intervals)]
    }

    nintervals <- length(lower_borders)

    if (is.null(intervals)) {
      av_trajectories[i,1:nintervals,av_dimension] <- lower_borders + interval_size / 2
    } else {
      av_trajectories[i,1:nintervals,av_dimension] <- intervals[1:nintervals] + diff(intervals[1:(nintervals+1)]) / 2
    }

    for (var in dimensions) {
      # Manipulate specified variables
      current_measures <- trajectories[i, 1:nlogs, var]

      # Perform averaging
      av_measures <- sapply(1:nintervals, function(j) {
        in_interval <- (current_av_values > lower_borders[j] &
          current_av_values <= (lower_borders[j] + interval_sizes[j]))
        return(mean(current_measures[in_interval], na.rm=TRUE))
      })

      av_trajectories[i,1:nintervals,var] <- av_measures
    }

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  return(create_results(data=data, results=av_trajectories, use=use, save_as=save_as))
}

#' Filter mousetrap data.
#'
#' Return a subset of the mousetrap data including only the trial data and
#' corresponding trajectories that meet the conditions specified in the
#' arguments.
#'
#' \code{mt_subset} is helpful when trials should be removed from all analyses.
#' By default, \code{check} is set to "data" meaning that the subset condition
#' is evaluated based on the trial data (stored in \code{data[["data"]]}).
#' However, it might also be of interest to only include trials based on
#' specific mouse-tracking measures (e.g., all trials with an \code{MAD} smaller
#' than 200). In this case, \code{check} needs to be set to the respective name
#' of the data.frame (e.g., "measures").
#'
#' Note that if specific trials should be removed from all analyses based on a
#' condition known a priori (e.g., practice trials), it is more efficient to use
#' the \link{subset} function on the raw data before importing the trajectories
#' using one of the mt_import functions (such as \link{mt_import_mousetrap}).
#'
#' Besides, if trials should only be removed from some analyses or for specific
#' plots, note that other mousetrap functions (e.g., \link{mt_reshape},
#' \link{mt_aggregate}, and \link{mt_plot}) also allow for subsetting.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details).
#' @param subset a logical expression (passed on to \link{subset}) indicating
#'   the rows to keep. Missing values are taken as \code{FALSE}.
#' @param check a character string specifying which data should be used for
#'   checking the subset condition.
#'
#' @return A mousetrap data object (see \link{mt_example}) with filtered
#'   data and trajectories.
#'
#' @seealso \link{subset} for the R base subset function for vectors, matrices,
#' or data.frames.
#'
#' \link{mt_reshape} for information about the subset argument in various other
#' mousetrap functions.
#'
#' @examples
#' # Subset based on trial data
#' mt_example_atypical <- mt_subset(mt_example, Condition=="Atypical")
#'
#' # Subset based on mouse-tracking measure (MAD)
#' mt_example <- mt_measures(mt_example)
#' mt_example_mad_sub <- mt_subset(mt_example, MAD<400, check="measures")
#'
#' @export
mt_subset <- function(data, subset, check="data") {

  # Use substitute to allow that arguments in subset
  # can be specified like the arguments in the subset function
  subset <- substitute(subset)

  # Filter data
  data[[check]] <- extract_data(data=data,use=check)
  data[[check]] <- base::subset(data[[check]], subset=eval(subset))

  # Remove trials and trajectories
  for (use in names(data)) {

    if (length(dim(data[[use]])) == 2) {
      
      # for special case of square matrices (e.g., distmat) remove both columns and rows
      if(class(data[[use]])[1]=="matrix" & dim(data[[use]])[1]==dim(data[[use]])[2]){
        data[[use]] <- data[[use]][
          rownames(data[[use]]) %in% rownames(data[[check]]),
          rownames(data[[use]]) %in% rownames(data[[check]]),
          drop=FALSE
          ]
        
      # otherwise, only remove rows
      } else {
        data[[use]] <- data[[use]][
          rownames(data[[use]]) %in% rownames(data[[check]]),,drop=FALSE
          ]
      }
      
      
    } else {
      data[[use]] <- data[[use]][
        rownames(data[[use]]) %in% rownames(data[[check]]),,,drop=FALSE
        ]
    }
  }

  return(data)
}


#' Add new variables to trajectory array.
#'
#' Add new variables to the trajectory array (and remove potentially existing
#' variables of the same name). This is mostly a helper function used by other
#' functions in this package (e.g., \link{mt_deviations}). However, it
#' can also be helpful if the user has calculated new variables for each logged
#' coordinate and wants to add them to an existing trajectory array.
#'
#' @inheritParams mt_time_normalize
#' @param variables either a character vector specifying the name of the new
#'   variables that should be added to the trajectory array. In this case, the
#'   new variables are added as additional columns to the trajectory array
#'   filled with \code{NA}s. Or a list of matrices that each contain the data of
#'   one of the to be added variables. In this case, the new variables with
#'   their values are added as additional columns to the trajectory array.
#' @return A mousetrap data object (see \link{mt_example}) where the new
#'   variables have been added as additional columns to the trajectory array.
#'   Depending on the input to \code{variables}, the values for the added
#'   variables are either \code{NA}s or their actual values. If columns of the
#'   same name already existed, they have been removed. If the trajectory array
#'   was provided directly as \code{data}, only the trajectory array will be
#'   returned.
#'
#' @examples
#' # Calculate new (arbitrary) variables for this example
#' # ... the sum of the x- and y-positions
#' xy_sum <- mt_example$trajectories[,,"xpos"] + mt_example$trajectories[,,"ypos"]
#' # ... the product of the x- and y-positions
#' xy_prod <- mt_example$trajectories[,,"xpos"] * mt_example$trajectories[,,"ypos"]
#'
#' # Add the new variables to the trajectory array
#' mt_example <- mt_add_variables(mt_example,
#'   variables=list(xy_sum=xy_sum, xy_prod=xy_prod))
#'
#' @export
mt_add_variables <- function(data,
                             use="trajectories", save_as=use,
                             variables) {

  # Extract trajectories
  trajectories <- extract_data(data=data,use=use)

  # If variables are provided as list with actual data,
  # extract variable names
  if (is.list(variables)) {
    data_list <- variables
    variables <- names(variables)
  } else if (is.vector(variables)) {
    data_list <- NULL
  } else {
    stop("Variables can either be a vector or a list.")
  }

  # Remove potentially existing variables in original array
  trajectories <- trajectories[
    ,
    ,
    !dimnames(trajectories)[[3]] %in% variables,
    drop=FALSE]

  # Setup new array
  trajectories_ext <- array(
    dim=dim(trajectories) + c(0, 0, length(variables)),
    dimnames=list(
      dimnames(trajectories)[[1]],
      dimnames(trajectories)[[2]],
      c(
        dimnames(trajectories)[[3]],
        variables
      )

    )
  )

  # Fill it with existing data
  trajectories_ext[,,dimnames(trajectories)[[3]]] <-
    trajectories[,,dimnames(trajectories)[[3]]]

  # Add new data if new data was provided
  if (is.null(data_list) == FALSE) {
    for (var in variables) {
      trajectories_ext[,,var] <- data_list[[var]]
    }
  }

  return(create_results(data=data, results=trajectories_ext, use=use, save_as=save_as))
}
