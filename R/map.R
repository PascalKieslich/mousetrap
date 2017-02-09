#' Map trajectories.
#'
#' \code{mt_map} maps trajectories onto a predefined set of prototype 
#' trajectories. It first computes distances between the trajectories and each
#' of the supplied trajectory types and then assigns each trajectory to the 
#' prototype that produced the smallest distance.
#' 
#' Mouse trajectories often occur in distinct, qualitative types (see Wulff, 
#' Haslbeck, Schulte-Mecklenbeck, 2017; Haslbeck, Wulff, Kieslich, Henninger, & 
#' Schulte-Mecklenbeck, 2017). Common trajectory types are linear trajectories,
#' mildly and strongly curved trajctories, and single and multiple
#' change-of-mind trials. \code{mt_map} allows to map
#' trajectories to a predefined set of trajectory types.
#' 
#' \code{mt_map} first adjusts prototypes to match the coordinate system of the 
#' trajectories specified by \code{use}. Then \code{mt_map} computes the
#' distances between each trajectory and each of the supplied prototypes (see 
#' \link{mt_distmat}) and then assigns each trajectory to the prototype that 
#' produced the smallest distance.
#' 
#' Mapping trajectories to prototypes requires that trajectories (and added
#' prototypes) are aligned to end in the top-left corner of the coordinate
#' system (see \link{mt_remap_symmetric} and \code{mt_align}). Furthermore it is
#' recommended to use spatialized trajectories (see \link{mt_spatialize};
#' Haslbeck, Wulff, Kieslich, Henninger, & Schulte-Mecklenbeck, 2017).
#' 
#' @inheritParams mt_distmat
#' @param prototypes a trajectory array containing the prototypes the 
#'   trajectories are mapped to. As a starting point, the trajectories stored in
#'   \link{mt_prototypes} can be used. See Details and Examples for selecting 
#'   prototypes and creating new ones.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional 
#'   \link{data.frame} (by default called \code{prototyping}) that contains the 
#'   best fitting prototype for each trajectory (in the "prototype" column the 
#'   number of the prototype is specified, in the "prototype_label" the ID of 
#'   the respective prototype) and the distance of the trajectory to the closest
#'   prototype. If a trajectory array was provided directly as \code{data}, only
#'   the data.frame containing the results will be returned.
#'
#' @references Haslbeck, J. M. B., Wulff,D. U., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2017). Advanced mouse- and hand-tracking
#'   analysis: Detecting and visualizing clusters in movement trajectories.
#'   Manuscript in preparation.
#'         
#' @examples
#' # Spatialize trajectories
#' mt_example <- mt_spatialize(mt_example)
#' 
#' # Map trajectories onto standard cluster set
#' mt_example <- mt_map(mt_example,
#'   use="sp_trajectories", prototypes=mt_prototypes)
#'   
#' 
#' # Add additional prototypes
#' mt_prototypes_ext <- mt_add_trajectory(mt_prototypes,
#'    xpos = c(0,1,-1,1,-1), ypos = c(0,1.5,1.5,1.5,1.5), id = "dCoM3"
#' )
#' mt_prototypes_ext <- mt_add_trajectory(mt_prototypes_ext,
#'    xpos = c(0,0,-1), ypos = c(0,1.5,1.5), id = "neutral"
#' )
#' 
#' # Map trajectories onto extended cluster set
#' mt_example <- mt_map(mt_example,
#'   use="sp_trajectories", prototypes=mt_prototypes_ext)
#' 
#'
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#' 
#' Jonas M. B. Haslbeck (\email{jonas.haslbeck@@gmail.com})
#'
#' @export
mt_map = function(
  data,
  use = 'trajectories',
  save_as = 'prototyping',
  dimensions = c('xpos','ypos'),
  
  # prototype arguments
  prototypes,

  # distance arguments
  pointwise = TRUE,
  minkowski_p = 2
  ){

  # Extract trajectories
  trajectories <- extract_data(data,use)
  
  # Tests
  if(!length(dimensions) %in% c(2,3)) stop('Dimensions must be of length 2 or 3.')
  if(!all(dimensions %in% dimnames(trajectories)[[3]])) stop('Not all dimensions exist.')
  # Ensure that there are no NAs
  if(any(is.na(trajectories[,,dimensions]))) {
    stop("Missing values in trajectories not allowed for mt_map ",
         "as all trajectories must have the same number of observations.")
  }
  
  # Align and rescale prototypes and combine them with trajectories
  n_points <- dim(trajectories)[2]
  n_proto  <- dim(prototypes)[1]
  prototypes <- mt_align(prototypes,coordinates = c(
    colMeans(trajectories[,1,dimensions]),colMeans(trajectories[,n_points,dimensions])
  ))
  prototypes <- mt_spatialize(prototypes,n_points = n_points, dimensions = dimensions)
  joint_array <- mt_bind(prototypes,trajectories,verbose=FALSE)
  

  # ---- compute distance & closest prototype
  distm <-  mt_distmat(
    joint_array,
    dimensions = dimensions,
    pointwise = pointwise,
    minkowski_p = minkowski_p)
  
  dists <- distm[1:n_proto,-c(1:n_proto)]
  min_dist <- apply(dists,2,min)
  prototype <- apply(dists,2,function(x) which(x == min(x)))
  prototype_label <- factor(rownames(prototypes)[prototype],levels=rownames(prototypes))

  # Save data
  return(create_results(
    data=data, results=data.frame(prototype,prototype_label,min_dist), 
    use=use, save_as=save_as,
    ids=rownames(trajectories), overwrite=TRUE))
  
}
