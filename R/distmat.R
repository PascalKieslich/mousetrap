#' Compute distance matrix.
#' 
#' Computes the point- or vector-wise dissimilarity between each pair of 
#' trajectories.
#' 
#' \code{mt_distmat} computes point- or vector-wise dissimilarities between 
#' pairs of trajectories. Point-wise dissimilarity refers to computing the 
#' distance metric defined by \code{minkowski_p} for every point of the 
#' trajectory and then summing the results. That is, if \code{minkowski_p = 2} 
#' the point-wise dissimilarity between two trajectories, each defined by a set 
#' of x and y coordinates, is calculated as \code{sum(sqrt((x_i-x_j)^2 + (y_i-y_j)^2))}. 
#' Vector-wise dissimilarity, on the other hand refers to computing the distance
#' metric once for the entire trajectory. That is, vector-wise dissimilarity is 
#' computed as \code{sqrt(sum((x_i-x_j)^2 + (y_i-y_j)^2))}.
#' 
#' @inheritParams mt_time_normalize
#' @param save_as a character string specifying where the resulting data should
#'   be stored.
#' @param dimensions a character vector specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3 for two-dimensional or 
#'   three-dimensional trajectories respectively.
#' @param weights numeric vector specifying the relative importance of the 
#'   variables specified in \code{dimensions}. Defaults to a vector of 1s 
#'   implying equal importance. Technically, each variable is rescaled so that
#'   the standard deviation matches the corresponding value in \code{weights}.
#'   To use the original variables, set \code{weights = NULL}.
#' @param pointwise boolean specifying the way dissimilarity between the 
#'   trajectories is measured (see Details). If \code{TRUE} (the default), 
#'   \code{mt_distmat} measures the average dissimilarity and then sums the 
#'   results. If \code{FALSE}, \code{mt_distmat}  measures dissimilarity once 
#'   (by treating the various points as independent dimensions).
#' @param minkowski_p an integer specifying the distance metric. 
#'   \code{minkowski_p = 1} computes the city-block distance, \code{minkowski_p 
#'   = 2} (the default) computes the Euclidian distance, \code{minkowski_p = 3} 
#'   the cubic distance, etc.
#' @param na_rm logical specifying whether trajectory points containing NAs 
#'   should be removed. Removal is done column-wise. That is, if any trajectory 
#'   has a missing value at, e.g., the 10th recorded position, the 10th position
#'   is removed for all trajectories. This is necessary to compute distance
#'   between trajectories.
#'   
#' @return A mousetrap data object (see \link{mt_example}) with an additional 
#'   object added (by default called \code{distmat}) containing the distance 
#'   matrix. If a trajectory array was provided directly as \code{data}, only 
#'   the distance matrix will be returned.
#'
#' @examples
#' # Length normalize trajectories
#' mt_example <- mt_length_normalize(mt_example)
#'  
#' # Compute distance matrix
#' mt_example <- mt_distmat(mt_example, use="ln_trajectories")
#'
#' @author
#' Dirk U. Wulff
#' 
#' Jonas M. B. Haslbeck
#'
#' @export
mt_distmat <- function(data,
                       use = 'ln_trajectories',
                       save_as = 'distmat',
                       dimensions = c('xpos','ypos'),
                       weights = rep(1,length(dimensions)),
                       pointwise = TRUE,
                       minkowski_p = 2,
                       na_rm = FALSE
                       ){
  
  # Extract data
  trajectories <- extract_data(data,use)
  
  # Tests
  if(!length(dimensions) %in% c(2,3)) stop('Dimensions must be of length 2 or 3.')
  if(!all(dimensions %in% dimnames(trajectories)[[3]])) stop('Not all dimensions exist.')
  
  
  # prepare trajectories
  trajectories = prepare_trajectories(trajectories = trajectories, 
                                      dimensions = dimensions, 
                                      weights = weights,
                                      na_rm = na_rm)  
  
  
  # Get distances
  if(length(dimensions) == 2){
    if(pointwise == TRUE){
        dmat <- distMat(trajectories[,,dimensions[1]],
                        trajectories[,,dimensions[2]],
                        power = minkowski_p)
      } else {
        dmat <- distMatV(trajectories[,,dimensions[1]],
                         trajectories[,,dimensions[2]],
                         power = minkowski_p)
      }
    } else {
      if(pointwise == TRUE){
          dmat <- distMat3d(trajectories[,,dimensions[1]],
                            trajectories[,,dimensions[2]],
                            trajectories[,,dimensions[3]],
                            power = minkowski_p)
        } else {
          dmat <- distMat3dV(data[[use]][,,dimensions[1]],
                             data[[use]][,,dimensions[2]],
                             data[[use]][,,dimensions[3]],
                             power = minkowski_p)
      }
    }
  
  # Set row and colnames
  rownames(dmat) <- dimnames(trajectories)[[1]]
  colnames(dmat) <- dimnames(trajectories)[[1]]

  # Save and return data
  if(is_mousetrap_data(data)){
    data[[save_as]] <- dmat
    } else {
    data <- dmat
    }
  return(data)
}


