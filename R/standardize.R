#' Scale and center variables within the levels of another variable.
#'
#' \code{scale_within} centers and/or scales variables in a data.frame (using 
#' \link{scale}) depending on the levels of one or more other variables. By 
#' default, variables are standardized (i.e., centered and scaled). A typical 
#' application is the within-subject standardization of variables in a repeated 
#' measures design.
#'
#' @param data a \link{data.frame}.
#' @param variables a character string (or vector) specifying one or more
#'   variables that \link{scale} is applied to. If unspecified,
#'   \code{scale_within} will be applied to all variables in data.
#' @param within an optional character string specifying the name of one or more
#'   variables in \code{data}. If specified, \link{scale} is applied separately 
#'   for each of the levels of the variable (or for each combination of levels,
#'   if more than one variable is specified). Alternatively, a vector directly
#'   containing the level values.
#' @param prefix a character string that is inserted before each scaled
#'   variable. By default (empty string) the original variables are replaced.
#' @param center argument passed on to \link{scale}.
#' @param scale argument passed on to \link{scale}.
#'
#' @return The original data.frame including the centered and / or scaled
#'   variables.
#'
#' @seealso \link{scale} for the R base scale function.
#' 
#'    \link{mt_standardize} for standardizing measures in a mousetrap data object.
#'
#' @examples
#' ChickWeight_scaled <-  scale_within(
#'  ChickWeight, variables="weight",
#'  within="Chick", prefix="z_")
#'  
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#' 
#' @export
scale_within <-function(data,
  variables=NULL, within=NULL,
  prefix="", center=TRUE, scale=TRUE) {

  # If no variables are specified, operate on all columns
  if (is.null(variables)) {
    variables <- colnames(data)
  }

  # If the within parameter contains less values
  # than the number of rows of the data.frame,
  # interpret this as the column names of the grouping
  # variables. Otherwise, interpret a vector of values
  # as a grouping factor.
  if (!is.null(within)) {
    if (length(within) < nrow(data)) {

      # Extract within variable values
      within_values <- data[, within, drop=FALSE]

      # Combine within variable values to one
      # vector coding each combination of values
      # with a unique integer
      within_values <- as.numeric(factor(
        apply(within_values, 1, paste, collapse=";")
      ))

      variables <- variables[!variables %in% within]
    } else {
      within_values <- within
    }
  }

  # Standardize each of the variables individually
  for (var in variables) {
    # Compute the label for the new,
    # standardized variable
    label <- paste(prefix, var, sep="")

    if (is.null(within)) {
      # If no grouping factor is specified,
      # scale the data across the entire variable
      data[,label] <- as.vector(
        base::scale(data[,var])
      )
    } else {
      # If there is a grouping factor specified,
      # apply scale individually for each of its levels
      for (i in unique(within_values)) {
        data[within_values==i,label] <-
          as.vector(
            base::scale(
              data[within_values==i, var],
              center=center,
              scale=scale
            )
          )
      }
    }
  }

  return(data)
}


#' Standardize mouse-tracking measures per level of other variables.
#'
#' Standardize selected mouse-tracking measures across all trials or per level
#' of one or more other variable, and store them in new variables. This function
#' is a thin wrapper around \link{scale_within}, focussed on mouse-tracking data
#' stored in a mousetrap data object.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which data should be used. By
#'   default points to the \code{measures} data.frame created using
#'   \link{mt_measures}.
#' @param use_variables a vector specifying which variables should be
#'   standardized. If unspecified, all variables will be standardized.
#' @param within an optional character string specifying one or more variables
#'   in \code{data[["data"]]}. If specified, all measures will be standardized
#'   separately for each level of the variable (or for each combination of
#'   levels, if more than one variable is specified).
#' @param prefix a character string that is inserted before each standardized
#'   variable. If an empty string is specified, the original variables are
#'   replaced.
#' @param center argument passed on to \link{scale}.
#' @param scale argument passed on to \link{scale}.
#'
#' @return A mousetrap data object (see \link{mt_example})
#' including the standardized measures.
#'
#' @seealso \link{mt_scale_trajectories} for standardizing variables in mouse
#'   trajectory arrays.
#'
#'   \link{scale_within} which is called by \code{mt_standardize}.
#'
#'   \link{scale} for the R base scale function.
#'
#' @examples
#' mt_example <- mt_measures(mt_example)
#'
#' # Standardize MAD and AD per subject
#' mt_example <- mt_standardize(mt_example,
#'   use_variables=c("MAD", "AD"),
#'   within="subject_nr", prefix="z_")
#'
#' # Standardize MAD and AD per subject and Condition
#' mt_example <- mt_standardize(mt_example,
#'   use_variables=c("MAD", "AD"),
#'   within=c("subject_nr", "Condition"),
#'   prefix="z_")
#'   
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#'   
#' @export
mt_standardize <-function(data, use="measures",
  use_variables=NULL, within=NULL,
  prefix="z_", center=TRUE, scale=TRUE) {

  data[[use]] <- extract_data(data=data,use=use)
  
  if(is.null(within)==FALSE){
    
    # Extract within variable values
    within <- data$data[rownames(data[[use]]), within, drop=FALSE]
    
    # Combine within variable values to one
    # vector coding each combination of values
    # with a unique integer
    within <- as.numeric(factor(
      apply(within, 1, paste, collapse=";")
    ))
    
  }
  
  # Apply the scale_within function
  data[[use]] <- scale_within(
    data[[use]],
    variables=use_variables,
    within=within,
    prefix=prefix,
    center=center,
    scale=scale
  )

  return(data)
}



#' Standardize variables in mouse trajectory array.
#' 
#' \code{mt_scale_trajectories} centers and / or standardizes selected
#' trajectory variables within or across trajectories.
#' 
#' @inheritParams mt_time_normalize
#' @param var_names character vector giving the labels of the to be 
#'   standardized variables.
#' @param center logical specifying whether variables should be centered (i.e.,
#'   \code{mean = 0}). Can be a logical vector, in which case the values of
#'   \code{scale} are mapped to the variables specified in \code{var_names}.
#' @param scale logical or numeric specifying the scaling of the variables. When
#'   logical, \code{scale = TRUE} normalizes the trajectory variable to sd = 1,
#'   whereas \code{scale = FALSE} leaves the variable on its original scale. When
#'   numeric, the trajectory variables are scaled by (i.e., divided by) the
#'   specific value in scale. Can also be a numeric vector, in which case the
#'   values of \code{scale} are mapped to the variables specified in 
#'   \code{var_names}.
#' @param within_trajectory logical specifying whether trajectory variables
#'   should be scaled within or across trajectories. If \code{within_trajectory
#'   == TRUE}, scaling trajectories to mean = 0 and sd = 1 means that every to
#'   be standardized trajectory variable will have mean = 0 and sd = 1. If
#'   \code{within_trajectory == FALSE} (the default), mean = 0 and sd = 1 are
#'   only true in the aggregate (i.e., across all trajectories). Can be a
#'   logical vector, in which case the values of \code{scale} are mapped to the
#'   variables specified in \code{var_names}.
#' @param prefix character string added to the names of the new standardized
#'   variables. If \code{prefix = ""}, the original variables will be
#'   overwritten.
#' @param transform function that takes a numeric matrix as argument and returns
#'   a numeric matrix of same size with transformed values. If \code{NULL} the
#'   original values are passed on to standardization.
#' 
#' @seealso
#' \link{mt_standardize} for standardizing mouse-tracking measures per level of
#' other variables.
#' 
#' @examples
#' # Calculate derivatives
#' mt_example <- mt_derivatives(mt_example)
#' 
#' # Standardize velocity across trajectories
#' mt_example <- mt_scale_trajectories(mt_example,var_names = "vel")
#' 
#' @author  Dirk U. Wulff
#'  
#' @return A mousetrap data object (see \link{mt_example}) with an additional 
#'   variable containing the standardized trajectory variable added to the 
#'   trajectory array). If the trajectory array was provided directly as 
#'   \code{data}, only the trajectory array will be returned.
#'   
#' @export 
mt_scale_trajectories = function(data,
  use = 'trajectories',
  save_as = use,
  var_names,
  center = TRUE, 
  scale = TRUE,
  within_trajectory = FALSE,
  prefix="z_",
  transform = NULL
  ){
  

  # extract trajectories
  traj = extract_data(data = data, use = use)

  # test if variables are in traj
  if(!all(var_names %in% dimnames(traj)[[3]])) stop('Variables not found')

  if(center == FALSE & (scale == TRUE | is.numeric(scale))){
    message('Note scaling does affect the arithmetic means of the variables even if center = FALSE.')
  }
  
  # store old names
  nam = dimnames(traj)

  # expand center
  if(length(center) != length(var_names)){
    if(length(center) == 1){
      center = rep(center,length(var_names))
    } else {
      stop('center must be length 1 or match length of var_names')
    }
  }
  
  # expand scale
  if(length(scale) != length(var_names)){
    if(length(scale) == 1){
      scale = rep(scale,length(var_names))
    } else {
      stop('scale must be length 1 or match length of var_names')
    }
  }
  
  # expand within
  if(length(within_trajectory) != length(var_names)){
    if(length(within_trajectory) == 1){
      within_trajectory = rep(within_trajectory,length(var_names))
    } else {
      stop('within_trajectory must be length 1 or match length of var_names')
    }
  }
  
  
  # standardize all variables and add to traj
  vars = list()
  for(i in 1:length(var_names)){
    
    # extract trajectory variable
    var = traj[,,var_names[i]]
    
    # transform trajectory variable
    if(!is.null(transform)) var = do.call(transform,list(var))
    
    ### scale
    # z standardizing within and across
    if(is.logical(scale[i])){
      
      # within
      if(within_trajectory[i] == TRUE){
        n_var = scale_rows(var, center = center[i], scale = scale[i])   
      
      # across  
      } else {
        n_var = scale_mat(var, center = center[i], scale = scale[i])
      }
    
    # arbitrary standardizing
    } else if(is.numeric(scale)){
      
      # within
      if(within_trajectory[i] == TRUE){
        n_var = trans_rows(var,center = center[i], scale = scale[i])
      
      # across  
      } else {
        n_var = trans_mat(var,center = center[i], scale = scale[i])       
      }
    
    # report inappropriare scale  
    } else {
      stop('scale must be logical or numeric')
    } 

    
    # restore NAs
    n_var[is.na(var)] = NA
    vars[[i]] = n_var
  }

  # add new variables
  names(vars) = paste0(prefix,var_names)
  traj = mt_add_variables(traj,variables = vars)
  
  # Return results
  return(create_results(
    data=data, results=traj,
    use=use, save_as=save_as
  ))
}
