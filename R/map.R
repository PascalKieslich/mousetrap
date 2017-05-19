#' Map trajectories to prototypes.
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
#' change-of-mind trials. \code{mt_map} allows to map trajectories to a
#' predefined set of trajectory types.
#' 
#' First, \code{mt_map} adjusts prototypes to match the coordinate system of the
#' trajectories specified by \code{use}. Next, \code{mt_map} computes the 
#' distances between each trajectory and each of the supplied prototypes (see 
#' \link{mt_distmat}) and then assigns each trajectory to the closest prototype
#' (i.e., the prototype that produced the smallest distance).
#' 
#' Mapping trajectories to prototypes requires that the endpoints of all
#' trajectories (and added prototypes) share the same direction, i.e., that all
#' trajectories end in the top-left corner of the coordinate system
#' (\link{mt_remap_symmetric} or \link{mt_align} can be used to achieve this).
#' Furthermore, it is recommended to use spatialized trajectories (see
#' \link{mt_spatialize}; Haslbeck, Wulff, Kieslich, Henninger, &
#' Schulte-Mecklenbeck, 2017).
#' 
#' @inheritParams mt_distmat
#' @param prototypes a trajectory array containing the prototypes the 
#'   trajectories are mapped to. As a starting point, the trajectories stored in
#'   \link{mt_prototypes} can be used. See Details and Examples for selecting 
#'   prototypes and creating new ones.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional 
#'   \link{data.frame} (by default called \code{prototyping}) that contains the 
#'   best fitting prototype for each trajectory (the number of the prototype is 
#'   specified under \code{prototype}, the label of the prototype under 
#'   \code{prototype_label}) and the distance of the trajectory to the best 
#'   fitting prototype (\code{min_dist}). If a trajectory array was provided
#'   directly as \code{data}, only the data.frame containing the results will be
#'   returned.
#'
#' @references Wulff, D. U., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M. 
#'   (2017). Measuring the (dis-)continuous mind. Manuscript in preparation.
#'   
#'   Haslbeck, J. M. B., Wulff, D. U., Kieslich, P. J., Henninger, F., & 
#'   Schulte-Mecklenbeck, M. (2017). Advanced mouse- and hand-tracking analysis:
#'   Detecting and visualizing clusters in movement trajectories. Manuscript in 
#'   preparation.
#'         
#' @examples
#' # Spatialize trajectories
#' KH2017 <- mt_spatialize(KH2017)
#' 
#' # Map trajectories onto standard prototype set
#' KH2017 <- mt_map(KH2017,
#'   use="sp_trajectories")
#' 
#' 
#' # Plot prototypes
#' mt_plot(mt_prototypes,facet_col="mt_id") + 
#'   ggplot2::facet_grid(.~factor(mt_id,levels=unique(mt_id)))
#' 
#' # Plot trajectories per assigned prototype
#' mt_plot(KH2017,use="sp_trajectories",
#'   use2="prototyping",facet_col="prototype_label")
#' 
#' 
#' # Map trajectories onto reduced prototype set
#' KH2017 <- mt_map(KH2017,
#'   use="sp_trajectories",
#'   prototypes=mt_prototypes[c("straight","curved","cCoM"),,],
#'   save_as="prototyping_red")
#' 
#' 
#' # Map trajectories onto extended prototype set
#' 
#' # Add additional prototypes
#' mt_prototypes_ext <- mt_add_trajectory(mt_prototypes,
#'    xpos = c(0,1,-1,1,-1), ypos = c(0,1.5,1.5,1.5,1.5), id = "dCoM3"
#' )
#' mt_prototypes_ext <- mt_add_trajectory(mt_prototypes_ext,
#'    xpos = c(0,0,-1), ypos = c(0,1.5,1.5), id = "neutral"
#' )
#' 
#' # Map trajectories
#' KH2017 <- mt_map(KH2017,
#'   use="sp_trajectories", prototypes=mt_prototypes_ext,
#'   save_as="prototyping_ext")
#' 
#'
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#' 
#' Jonas M. B. Haslbeck (\email{jonas.haslbeck@@gmail.com})
#'
#' @export
mt_map <- function(
  data,
  use = 'sp_trajectories',
  save_as = 'prototyping',
  dimensions = c('xpos','ypos'),
  
  # prototype arguments
  prototypes = mousetrap::mt_prototypes,

  # distance arguments
  weights = rep(1, length(dimensions)),
  pointwise = TRUE,
  na_rm = FALSE,  
  minkowski_p = 2
  ){

  # Extract trajectories
  trajectories <- extract_data(data,use)
  
  # Tests
  if(!length(dimensions) %in% c(2,3)) stop('Dimensions must be of length 2 or 3.')
  if(!all(dimensions %in% dimnames(trajectories)[[3]])) stop(paste0('Not all dimensions found in "',use,'".'))
  
  # Ensure that there are no NAs
  if(any(is.na(trajectories[,,dimensions]))) {
    stop("Missing values in trajectories not allowed for mt_map ",
         "as all trajectories must have the same number of observations.")
  }
  
  # check prototype dimensionality
  if(!all(dimensions %in% dimnames(prototypes)[[3]])) stop(paste0('Not all dimensions found in prototypes.'))
  
  # Align and rescale prototypes and combine them with trajectories
  n_points <- dim(trajectories)[2]
  n_proto  <- dim(prototypes)[1]
  prototypes <- mt_align(prototypes,coordinates = c(
    colMeans(trajectories[,1,dimensions]),colMeans(trajectories[,n_points,dimensions])
    ))
  prototypes <- mt_spatialize(prototypes,n_points = n_points, dimensions = dimensions)
  joint_array <- mt_bind(prototypes,trajectories,verbose=FALSE)
  
  # limit trajectories to dimensions
  joint_array <- joint_array[,,dimensions]
  
  # prepare trajectories
  joint_array = prepare_trajectories(trajectories = joint_array, 
                                      dimensions = dimensions, 
                                      weights = weights,
                                      na_rm = na_rm)  

  # ---- compute distance & closest prototype
  distm <-  mt_distmat(
    joint_array,
    dimensions = dimensions,
    weights = NULL,
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
    ids=rownames(trajectories), overwrite=FALSE))
  
}
