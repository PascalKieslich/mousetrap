#' Align trajectories.
#' 
#' \code{mt_align} aligns trajectories to a common start point, end point, and /
#' or coordinate system. 
#' 
#' If \code{align_start} / \code{align_end} is \code{FALSE}, \code{coordinates} 
#' define the position of the average start / end point across all trajectories. 
#' Note that if both \code{align_start} and \code{align_end} are \code{FALSE} 
#' (the default), this completely preserves the relative location of all 
#' trajectory points.
#' 
#' If \code{align_start} / \code{align_end} is \code{TRUE}, the start / end
#' point of each trajectory is set to the exact position specified in
#' \code{coordinates}. \code{align_start} and \code{align_end} can be set
#' completely independently of one another, i.e., one can align only end points,
#' only start points, none, or both. 
#' 
#' Note that if the end points of trajectories are not aligned coordinates refer
#' to the hypothetical case where all trajectories are mapped to one side.
#'    
#' If \code{align_start} is set to \code{"left"} or \code{"right"} trajectories 
#' will be flipped to the lower or upper spectrum of the first dimensions, 
#' respectively. If the first dimension is the x-coordinate this is equivalent
#' to flipping the trajectories to the left and right side, respectively.
#'  
#' @inheritParams mt_time_normalize
#' @param dimensions a character string specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3 for two-dimensional or 
#'   three-dimensional alignment respectively.
#' @param coordinates either a numeric vector of length 4 specifying the xstart, 
#'   ystart, xend, yend coordinates of the trajectory start and end points. Can 
#'   also be \code{isotropic} (the default) to preserve the coordinates of dim1 
#'   and dim2, \code{isotropic-norm} to set the coordinates to 
#'   \code{c(0,0,-1,x)} where x is chosen to preserve the aspect ratio of dim1 
#'   and dim2, \code{mt} to set coordinates to \code{c(0,0,-1,1.5)}, \code{norm}
#'   to set coordinates  to \code{c(0,0,-1,1)}, and \code{wide} to set 
#'   coordinates  to \code{c(0,0,-1,1.2)}. In the three-dimensional case, 
#'   coordinates is a vector of length 6.
#' @param align_start boolean specifying whether the start points of all
#'   trajectories should be aligned to the position specified in
#'   \code{coordinates}. See Details.
#' @param align_end boolean specifying whether the end points of all trajectories
#'   should be aligned to the position specified in \code{coordinates}. See 
#'   Details.
#' @param align_side character string specifying whether all trajectories should
#'   be flipped to the left side (\code{left}), the right side (\code{right}),
#'   or not at all (\code{no}). Assumes that first entry in \code{dimensions}
#'   are the x positions.
#' @return A mousetrap data object (see \link{mt_example}) with aligned 
#'   trajectories. Per default, the dimemnsions in the original trajectory array
#'   will be replaced. If a different trajectory array is specified using
#'   \code{save_as}, a new trajectory array will be created (including only the
#'   aligned dimensions). If a trajectory array was provided directly as
#'   \code{data}, only the aligned trajectories will be returned.
#' 
#' @examples
#' mt_example <- mt_align(mt_example,
#'   align_start = TRUE, align_end = TRUE,
#'   coordinates = 'mt')
#' 
#' @seealso 
#' \link{}...
#' 
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#' 
#' @export
mt_align = function(data,
                    use = 'trajectories',
                    save_as = use,
                    dimensions = c('xpos','ypos'),
                    coordinates = 'isotropic',
                    align_start = FALSE, 
                    align_end = FALSE,
                    align_side = 'no',
                    verbose=FALSE
){
  
  # Extract data
  trajectories <- extract_data(data,use)
  
  # Tests
  if(!length(dimensions) %in% c(2,3)) stop('Dimensions must of length 2 or 3.') 
  if(!all(dimensions %in% dimnames(trajectories)[[3]])) stop('Not all dimensions exist.')
  if(is.character(coordinates) & 
     !any(coordinates %in% c('isotropic','isotropic-norm','mt','norm','wide')) & 
     !is.numeric(coordinates)){
    stop("Dimensions must be numeric or one of the following values: ",
         "'isotropic', isotropic-norm', 'mt', 'norm', 'wide'.")
  }
  
  # set flip to true
  tmp_flip = TRUE
  
  # Set coordinates if they were not provided explicitly
  if(length(coordinates)==1){
    
    if(coordinates %in% c('isotropic', 'isotropic-norm')){
      
      # flip to compute coordinates
      flips = matrix(NA,ncol = length(dimensions), nrow = dim(trajectories)[1])
      for(i in 1:length(dimensions)){
        start  <- mean(trajectories[,1,dimensions[i]])
        ends   <- getlast(trajectories[,,dimensions[i]])
        if(i == 1){
          flip   <- ends > start
        } else {
          flip   <- ends < start
        }
        trajectories[flip,,dimensions[i]] <-  ((trajectories[flip,,dimensions[i]] - start) * -1) + start
        flips[,i] = flip
      }
 
      # prevent flipping
      tmp_flip = FALSE
        
      # start and end points
      m1  <- colMeans(trajectories[,1,dimensions])
      mn  <- colMeans(apply(trajectories[,,dimensions],3,getlast))
         
      if(coordinates == 'isotropic'){
        
        # set coordinates
        coordinates <- c(m1,mn)
        
      } else if (coordinates=='isotropic-norm') {
        
        # compute coordinates
        if(length(dimensions) == 2){
          d1 = abs(mn[1] - m1[1])
          d2 = abs(mn[2] - m1[2])
          coordinates = c(0,0,-1,d2/d1)
        } else {
          d1 = mn[1] - m1[1]
          d2 = mn[2] - m1[2]
          d3 = mn[3] - m1[3]  
          coordinates = c(0,0,0,-1,d2/d1,d3/d1)
        }
        
      }
        
    } else {
      
      if(coordinates == 'mt'){
        if(length(dimensions) == 2){
          coordinates <- c(0,0,-1,1.5)
        } else {
          coordinates <- c(0,0,0,-1,1.5,-1)
        }
        
      } else if(coordinates == 'norm'){
        if(length(dimensions) == 2){
          coordinates <- c(0,0,-1,1)
        }else{
          coordinates <- c(0,0,0,-1,1,1)
        }
        
      } else if(coordinates == 'wide'){
        if(length(dimensions) == 2){
          coordinates <- c(0,0,-1,1.2)
        }else{
          coordinates <- c(0,0,0,-1,1.2,-1)
        }
      }
    }
    if(verbose == TRUE) message('Aligning to: ',paste(coordinates,collapse=' '))
    
  }
  
  
  # Flip trajectories
  if(tmp_flip == TRUE){
    flips = matrix(NA,ncol = length(dimensions), nrow = dim(trajectories)[1])
    for(i in 1:length(dimensions)){
      start  <- mean(trajectories[,1,dimensions[i]])
      ends   <- getlast(trajectories[,,dimensions[i]])
      if(coordinates[i] > coordinates[i + length(dimensions)]){
        flip <- ends > start
      } else {
        flip <- ends < start  
      }
      trajectories[flip,,dimensions[i]] <-  ((trajectories[flip,,dimensions[i]] - start) * -1) + start
      flips[,i] = flip
    }
  }
  
  # Replace NAs
  reset_NAs <- FALSE
  if(any(is.na(trajectories[,,dimensions]))){ 
    reset_NAs = TRUE
    trajectories[,,dimensions][is.na(trajectories[,,dimensions])] =  -3.141592653589793
  }
  
  # Align data   
  if(length(dimensions) == 2){
    if(length(coordinates) != 4) stop('Coordinates must be a numeric vector of length 4.')
      al_trajectories = trajAlign(trajectories[,,dimensions[1]],
                                  trajectories[,,dimensions[2]],
                                  start = align_start,end = align_end,coordinates = coordinates)
  }
  if(length(dimensions) == 3){
    if(length(coordinates) != 6) stop('Coordinates must be a numeric vector of length 6.')
    al_trajectories = trajAlign3d(trajectories[,,dimensions[1]],
                                  trajectories[,,dimensions[2]],
                                  trajectories[,,dimensions[3]],
                                  start = align_start,end = align_end,coordinates = coordinates)
  }
  
  # Reset NAs
  if(reset_NAs){
    for(i in 1:length(dimensions)){
      al_trajectories[[i]][al_trajectories[[i]] == -3.141592653589793] =  NA
    }
  }
  
  # Add aligned data
  for(i in 1:length(dimensions)){
    trajectories[,,dimensions[i]] = al_trajectories[[i]]
  }
  
  # Reflip trajectories
  if(any(flips) & align_end == FALSE){
    for(i in 1:length(dimensions)){
      start <- mean(trajectories[,1,dimensions[i]])
      trajectories[flips[,i],,dimensions[i]] <-  ((trajectories[flips[,i],,dimensions[i]] - start) * -1) + start
    }
  }
  
  # align to right if required
  if(align_side != 'no'){
    start  <- mean(trajectories[,1,dimensions[1]])
    ends   <- getlast(trajectories[,,dimensions[1]])
    if(align_side == 'right'){
      flip   <- ends < start
      } else if (align_side == 'left'){
      flip   <- ends > start        
      }
    trajectories[flip,,dimensions[1]] <-  ((trajectories[flip,,dimensions[1]] - start) * -1) + start
  }
  
  # If aligned data should be stored in new object, remove other dimensions
  if (save_as != use){
    trajectories <- trajectories[,,dimensions]
  }
  
  return(create_results(data=data, results=trajectories, use=use, save_as=save_as))
  
}





