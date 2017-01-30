#' Calculate distance, velocity, and acceleration.
#'
#' Calculate distance traveled, velocity, and acceleration for each logged
#' position. Distance is calculated as the Euclidean distance between successive
#' coordinates, and velocity as distance covered per time interval. The
#' acceleration denotes the difference in velocity, again normalized per time.
#'
#' Distances, velocities and acceleration are computed as follows:
#'
#' The first entry in each respective vector is always zero. Each subsequent
#' entry thus represents the Euclidean distance traveled since the previous
#' recorded set of coordinates and the velocity with which the movement between
#' both samples took place. Thus, both distance and velocity represent the
#' intervening period between the previous sample and the one with which the
#' numeric value is saved.
#'
#' The acceleration, by contrast, denotes the change in velocity between two
#' adjacent periods. Because of this, it is shifted forward to best match the
#' actual time point at which the acceleration was measured. Because there will
#' always be one less value computed for acceleration than for velocity, the
#' final value in the acceleration vector has been padded with an NA. To
#' reconstruct the velocity from the acceleration, multiply the acceleration
#' vector with the sampling interval, compute the cumulative sum of the result,
#' and add a zero at the beginning.
#'
#' If the distance is calculated across both horizontal and vertical (x and y)
#' dimensions, distance and velocity is always positive (or 0). If only one
#' dimension is used, increases in x (or y) values result in positive distances
#' and velocity values, decreases in negative distances and velocity values.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying across which dimension(s)
#'   distances, velocity, and acceleration are calculated. By default
#'   (\code{c("xpos","ypos")}), they are calculated across both x and y
#'   dimensions. Alternatively, only one dimesion can be specified, e.g.,
#'   \code{"xpos"} or \code{"ypos"}.
#' @param timestamps a character string specifying the trajectory dimension
#'   containing the timestamps.
#' @param prefix an optional character string that is added as a prefix to the
#'   to be created new trajectory dimensions.
#' @param acc_on_abs_vel logical indicating if acceleration should be calculated
#'   based on absolute velocity values (ignoring direction). Only relevant if
#'   velocity can be negative (see Details).
#' @param return_delta_time logical indicating if the timestamp differences 
#'   should be returned as well (as "delta_time").
#'
#' @return A mousetrap data object (see \link{mt_example}) with Euclidian 
#'   distance, velocity, and acceleration added as additional variables to the 
#'   trajectory array (called \code{dist}, \code{vel}, and  \code{acc}, if no 
#'   prefix was specified). If the trajectory array was provided directly as 
#'   \code{data}, only the trajectory array will be returned.
#'
#' @seealso \link{mt_average} for averaging trajectories across constant time
#' intervals.
#'
#' \link{mt_measures} for calculating per-trial mouse-tracking
#' measures.
#'
#' @examples
#' # Calculate derivatives looking at movement
#' # across both dimensions
#' mt_example <- mt_derivatives(mt_example)
#'
#' # Calculate derivatives ony looking at movement
#' # in x dimension
#' mt_example <- mt_derivatives(mt_example,
#'   dimensions="xpos")
#'
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_derivatives <- function(
  data,
  use="trajectories", save_as=use,
  dimensions=c("xpos","ypos"), timestamps="timestamps",
  prefix="",
  acc_on_abs_vel=FALSE, return_delta_time=FALSE,
  verbose=FALSE) {
  
  
  # Extract trajectories and create labels
  trajectories <- extract_data(data=data, use=use)
  dist <- paste0(prefix, "dist")
  vel  <- paste0(prefix, "vel")
  acc  <- paste0(prefix, "acc")
  delta_time <-  paste0(prefix, "delta_time")

  # Create new array with added columns for the new variables
  derivatives <- mt_add_variables(trajectories,
    variables=c(dist,vel,acc))
  
  if (return_delta_time){
    derivatives <- mt_add_variables(derivatives,
                                    variables=c(delta_time))
  }

  # Calculate derivatives
  for (i in 1:nrow(trajectories)) {

    # Compute deltas for timestamps
    delta_timestamps <- diff(derivatives[i,,timestamps])

    # Compute distance depending on the number of dimensions

    # For one dimension, simply compute the distance
    if (length(dimensions) == 1) {
      distances <- diff(derivatives[i,,dimensions])

    # For more than one dimension, compute Eucledian distance between measurements
    } else {
      distances <- sqrt(rowSums(diff(derivatives[i,,dimensions])^2))
    }

    # Compute velocity based on distance and time deltas
    velocities <- distances / delta_timestamps

    # Compute acceleration based on the velocity differences
    if (acc_on_abs_vel) {
      accelerations <- diff(abs(velocities)) / delta_timestamps[-length(delta_timestamps)]
    } else {
      accelerations <- diff(velocities) / delta_timestamps[-length(delta_timestamps)]
    }

    # Pad the accelerations so that they can be concatenated to the remaining data
    accelerations <- c(accelerations, NA)

    # Add derivatives to array (adding a ceiling so they have the same length)
    derivatives[i,,dist] <- c(0, distances)
    derivatives[i,,vel] <- c(0, velocities)
    derivatives[i,,acc] <- c(0, accelerations)
    
    if (return_delta_time){
      derivatives[i,,delta_time] <- c(0, delta_timestamps)
    }

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  return(create_results(
    data=data, results=derivatives,
    use=use, save_as=save_as
  ))
}
