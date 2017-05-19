#' Spatialize trajectories.
#' 
#' Re-represent each trajectory spatially using a constant number of points so 
#' that adjacent points on the trajectory become equidistant to each other.
#' 
#' \code{mt_spatialize} is used to emphasize the trajectories' shape. Usually, 
#' the vast majority of points of a raw or a time-normalized trajectory lie
#' close to the start and end point. \code{mt_spatialize} re-distributes these
#' points so that the spatial distribution is uniform across the entire
#' trajectory. \code{mt_spatialize} is mainly used to improve the results of
#' clustering (in particular \link{mt_cluster}) and visualization.
#'
#' @inheritParams mt_time_normalize
#' @param dimensions a character string specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3 for two-dimensional or 
#'   three-dimensional data.
#' @param n_points an integer or vector of integers specifying the number of 
#'   points used to represent the spatially rescaled trajectories. If a single 
#'   integer is provided, the number of points will be constant across 
#'   trajectories. Alternatively, a vector of integers can provided that specify
#'   the number of points for each trajectory individually.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional 
#'   array (by default called \code{sp_trajectories}) containing the spatialized
#'   trajectories. If a trajectory array was provided directly as \code{data}, 
#'   only the spatialized trajectories will be returned.
#'
#' @examples
#' KH2017 <- mt_spatialize(data=KH2017,
#'   dimensions = c('xpos','ypos'),
#'   n_points = 20)
#'
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#'
#' Jonas M. B. Haslbeck (\email{jonas.haslbeck@@gmail.com})
#'
#' @export

mt_spatialize = function(data,
                         use = 'trajectories',
                         dimensions = c('xpos', 'ypos'),
                         save_as = 'sp_trajectories',
                         n_points = 20
                         ){

  # Extract trajectories
  trajectories <- extract_data(data,use)

  # Tests
  if (!length(dimensions) %in% c(2,3)) {
    stop('Dimensions must of length 2 or 3.')
  }
  if (!all(dimensions %in% dimnames(trajectories)[[3]])) {
    stop('Not all dimensions exist.')
  }

  # Spatialize trajectories
  if (length(dimensions) == 2) {
    if (nrow(trajectories) == 1) {
      # Cover special case of single trajectory
      dim_1 <- matrix(trajectories[,,dimensions[1]], nrow=1)
      dim_2 <- matrix(trajectories[,,dimensions[2]], nrow=1)
    } else {
      dim_1 <- trajectories[,,dimensions[1]]
      dim_2 <- trajectories[,,dimensions[2]]
    }
    spatialized_trajectories <- spatializeArray(dim_1, dim_2, n_points)
  } else if (length(dimensions) == 3) {
    if (nrow(trajectories) == 1) {
      # Cover special case of single trajectory
      dim_1 <- matrix(trajectories[,,dimensions[1]],nrow=1)
      dim_2 <- matrix(trajectories[,,dimensions[2]],nrow=1)
      dim_3 <- matrix(trajectories[,,dimensions[3]],nrow=1)
      } else {
      dim_1 <- trajectories[,,dimensions[1]]
      dim_2 <- trajectories[,,dimensions[2]]
      dim_3 <- trajectories[,,dimensions[3]]
    }
    spatialized_trajectories <- spatializeArray3d(dim_1, dim_2, dim_3, n_points)
  }

  # create new trajectory array
  result <- array(
    dim=c(
      dim(trajectories)[1],
      max(n_points),
      length(dimensions)
    ),
    dimnames=list(dimnames(trajectories)[[1]], NULL, dimensions)
  )

  # add rescaled to new trajectories and set NAs
  for (i in 1:length(spatialized_trajectories)) {
    tmp_traj <- spatialized_trajectories[[i]]
    tmp_traj[tmp_traj == -10000] <- NA
    result[,,i] <- tmp_traj
    }

  # return data
  return(create_results(data=data, results=result, use=use, save_as=save_as))
}

# Spatialize trajectories to long (internal function)
mt_spatialize_tolong <- function(data,
                                 use='trajectories',
                                 dimensions=c('xpos','ypos'),
                                 n_points=20
){

  # Extract trajectories
  trajectories <- extract_data(data, use)

  # Tests
  if (!length(dimensions) %in% c(2,3,4)) {
    stop('Dimensions must of length 2, 3, or 4.')
  }
  if (!all(dimensions %in% dimnames(trajectories)[[3]])) {
    stop('Not all dimensions exist.')
  }

  # Spatialize trajectories

  if (length(dimensions) == 2) {
    if (nrow(trajectories) == 1) {
      # Cover special case of single trajectory
      dim_1 <- matrix(trajectories[,,dimensions[1]], nrow=1)
      dim_2 <- matrix(trajectories[,,dimensions[2]], nrow=1)
    } else {
      dim_1 <- trajectories[,,dimensions[1]]
      dim_2 <- trajectories[,,dimensions[2]]
    }
    spatialized_trajectories <- spatializeArrayToLong(dim_1, dim_2, n_points)
  } else if (length(dimensions) == 3) {
    if (nrow(trajectories) == 1) {
      # Cover special case of single trajectory
      dim_1 <- matrix(trajectories[,,dimensions[1]],nrow=1)
      dim_2 <- matrix(trajectories[,,dimensions[2]],nrow=1)
      dim_3 <- matrix(trajectories[,,dimensions[3]],nrow=1)
    } else {
      dim_1 <- trajectories[,,dimensions[1]]
      dim_2 <- trajectories[,,dimensions[2]]
      dim_3 <- trajectories[,,dimensions[3]]
    }
    spatialized_trajectories <- spatializeArrayToLong3d(
      dim_1, dim_2, dim_3, n_points
    )
  } else if (length(dimensions) == 4) {
    if (nrow(trajectories) == 1) {
      # Cover special case of single trajectory
      dim_1 <- matrix(trajectories[,,dimensions[1]],nrow=1)
      dim_2 <- matrix(trajectories[,,dimensions[2]],nrow=1)
      dim_3 <- matrix(trajectories[,,dimensions[3]],nrow=1)
      dim_4 <- matrix(trajectories[,,dimensions[4]],nrow=1)
    } else {
      dim_1 <- trajectories[,,dimensions[1]]
      dim_2 <- trajectories[,,dimensions[2]]
      dim_3 <- trajectories[,,dimensions[3]]
      dim_4 <- trajectories[,,dimensions[4]]
    }
    spatialized_trajectories <- spatializeArrayToLong4d(
      dim_1, dim_2, dim_3, dim_4, n_points
    )
  }
  
  # Return data
  return(spatialized_trajectories)
}
