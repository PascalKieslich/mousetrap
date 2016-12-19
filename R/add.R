#' Add new trajectory to trajectory array.
#'
#' Add new trajectory to trajectory array.
#'
#' @inheritParams mt_time_normalize
#' @param xpos a vector of x positions. Ignored, if \code{xypos} is provided.
#' @param ypos a vector of y positions. Ignored, if \code{xypos} is provided.
#' @param xypos a matrix, the first column corresponding to the x positions, the
#'   second to the y positions.
#' @param id a character string specifying the identifier of the to be added
#'   trajectory.
#' @return A mousetrap data object (see \link{mt_example}) where the new 
#'   trajectory has been added.
#'   If the trajectory array was provided directly as \code{data}, only the
#'   trajectory array will be returned.
#'
#' @examples
#' new_trajectory <- cbind(
#'   c(0,0.5,1),
#'   c(0,1, 2)
#'  )
#'  
#'  mt_example <- mt_add_trajectory(
#'    mt_example, xypos=new_trajectory, id = "test_traj"
#'  )
#'
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_add_trajectory <- function(
  data,
  use="trajectories", save_as=use,
  xpos=NULL, ypos=NULL, xypos=NULL, id = "new") {
  
  # Extract trajectories
  trajectories <- extract_data(data=data,use=use)
  
  # New trajectory
  if(is.null(xypos)){
    xypos <- cbind(xpos,ypos)
  }
  colnames(xypos) <- c("xpos","ypos")
  rownames(xypos) <- NULL
  
  
  # Check for potentially existing trajectories of same name
  if (id %in%dimnames(trajectories)[[1]]){
    stop("Trajectory of same name already exist in data. ",
         "Please specify a different name using the id argument.")
  }
  
  # Setup new array
  trajectories_ext <- array(
    dim=dim(trajectories) + 
      c(1, max(c(0,nrow(xypos)-dim(trajectories)[2])),0),
    dimnames=list(
      c(dimnames(trajectories)[[1]],id),
      dimnames(trajectories)[[2]],
      dimnames(trajectories)[[3]]
    )
  )
  
  # Fill it with existing data
  trajectories_ext[dimnames(trajectories)[[1]],1:dim(trajectories)[2],] <-
    trajectories
  
  # Add new trajectory
  trajectories_ext[id,1:nrow(xypos),colnames(xypos)] <- xypos
  
  # If mousetrap data object is provided, add new line to data
  if (is_mousetrap_data(data)) {
    ids <- c(rownames(data$data), id)
    data$data <- rbind(data$data, NA)
    rownames(data$data) <- ids
    data$data[,"mt_id"] <- ids
  }
  
  return(create_results(data=data, results=trajectories_ext, use=use, save_as=save_as))
}

#' Join two trajectory arrays
#' 
#' Join two trajectory arrays.
#' 
#' @inheritParams mt_time_normalize
#' @param trajectories1 a trajectory array (see \link{mt_example}).
#' @param trajectories2 a trajectory array (see \link{mt_example}).
#' 
#' @return A trajectory array.
#' 
#' @examples
#' 
#' \dontrun{
#' trajectories_combined <- mt_bind(
#'   mt_example$trajectories,
#'   mt_prototypes
#' )
#' }
#' 
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_bind <- function(
  trajectories1,
  trajectories2,
  verbose=FALSE) {
  
  # Check dimensions of trajectory arrays
  dim1 <- dim(trajectories1)
  dim2 <- dim(trajectories2)
  dimnames1 <- dimnames(trajectories1)
  dimnames2 <- dimnames(trajectories2)
  
  # Check id variables
  if (any(dimnames1[[1]] %in% dimnames2[[1]])){
      stop("trajectories1 and trajectories2 cannot be joined as they share identical trial identifiers.")
  }

  # Check mouse-tracking variables
  if (verbose) {
    message("Checking trajectory variables.")
  }
  variables <- dimnames1[[3]]
  if(any(!dimnames1[[3]]%in%dimnames2[[3]])){
    if (verbose) {
      message("trajectories1 contain mouse-tracking variables not present in trajectories2.")
    }
  }
  if(any(!dimnames2[[3]]%in%dimnames1[[3]])){
    if (verbose) {
      message("trajectories2 contain mouse-tracking variables not present in trajectories1.")
    }
    variables <- c(variables,
      dimnames2[[3]][!dimnames2[[3]]%in%dimnames1[[3]]])
  }
  
  # Setup new array
  if (verbose) {
    message("Joining trajectory arrays.")
  }
  trajectories <- array(
    dim = c(
      dim1[1]+dim2[1],
      max(c(dim1[2],dim2[2])),
      length(variables)
    ),
    
    dimnames = list(
      c(dimnames1[[1]],dimnames2[[1]]),
      NULL,
      variables
    )
  )
  
  # Add data
  trajectories[dimnames1[[1]],1:dim1[2],dimnames1[[3]]] <- 
    trajectories1
  trajectories[dimnames2[[1]],1:dim2[2],dimnames2[[3]]] <- 
    trajectories2
  
  return(trajectories)
}
