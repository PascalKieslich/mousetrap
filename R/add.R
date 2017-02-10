#' Add new trajectory to trajectory array.
#'
#' Add a single new trajectory to trajectory array.
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
#' # Add additional prototype to mt_prototypes
#' mt_prototypes_ext <- mt_add_trajectory(mt_prototypes,
#'    xpos = c(0,1,-1,1,-1), ypos = c(0,1.5,1.5,1.5,1.5), id = "dCoM3"
#' )
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
#' Join two trajectory arrays. This function is mainly used internally, but can 
#' be helpful in those (relatively rare) occasions where addional processed 
#' trajectory data should be added to another trajectory array.
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
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
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
