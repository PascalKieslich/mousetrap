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
#' @param within a character string specifying the name of one or more variables
#'   in \code{data}. For each of the levels of this variable (or for each
#'   combination of levels, if more than one variable is specified),
#'   \link{scale} is applied separately. Alternatively, a vector directly
#'   containing the level values.
#' @param prefix a character string that is inserted before each scaled
#'   variable. By default (empty string) the original variables are replaced.
#' @param center argument passed on to \link{scale}.
#' @param scale argument passed on to \link{scale}.
#'
#' @return The original data.frame including the centered and / or scaled
#'   variables.
#'
#' @seealso
#' \link{scale} for the R base scale function.
#'
#' \link{mt_standardize} for standardizing measures in a mousetrap data object.
#'
#' @examples
#' ChickWeight_scaled <-  scale_within(
#'  ChickWeight, variables="weight",
#'  within="Chick", prefix="z_")
#'  
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
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
#' Standardize selected mouse-tracking measures per level of one or more other
#' variable, and store them in new variables. This function is a thin wrapper
#' around \link{scale_within}, focussed on mouse-tracking data stored in a
#' mousetrap data object.
#'
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which data should be used. By
#'   default points to the \code{measures} data.frame created using
#'   \link{mt_measures}.
#' @param use_variables a vector specifying which variables should be
#'   standardized. If unspecified, all variables will be standardized.
#' @param within a character string specifying one or more variables in
#'   \code{data[["data"]]}. All measures will be standardized separately for
#'   each level of the variable (or for each combination of levels, if more than
#'   one variable is specified). By default, points to the subject identifier
#'   used in OpenSesame ("subject_nr").
#' @param prefix a character string that is inserted before each standardized
#'   variable. If an empty string is specified, the original variables are
#'   replaced.
#' @param center argument passed on to \link{scale}.
#' @param scale argument passed on to \link{scale}.
#'
#' @return A mousetrap data object (see \link{mt_example})
#' including the standardized measures.
#'
#' @seealso
#' \link{scale_within} which is called by \code{mt_standardize}.
#'
#' \link{scale} for the R base scale function.
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
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#'   
#' @export
mt_standardize <-function(data, use="measures",
  use_variables=NULL, within="subject_nr",
  prefix="z_", center=TRUE, scale=TRUE) {

  data[[use]] <- extract_data(data=data,use=use)
  
  # Extract within variable values
  within <- data$data[rownames(data[[use]]), within, drop=FALSE]

  # Combine within variable values to one
  # vector coding each combination of values
  # with a unique integer
  within <- as.numeric(factor(
    apply(within, 1, paste, collapse=";")
  ))

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



#' Standardize trajectories
#' 
#' \code
mt_scale_trajectories = function(data,
                                 use = 'trajectories',
                                 variables,
                                 prefix="z_", 
                                 center=TRUE, 
                                 scale=TRUE){
  
  # extract trajectories
  traj = extract_data(data=data,use=use)
  
  # test if variables are in traj
  if(!all(variables %in% dimenames(traj)[[3]])) stop('Variables not found')

  # store old names
  nam = dimnames(traj)
  
  # standardize all variables and add to traj
  for(i in 1:length(variables)){
    var = zstandardize(traj[,,variables[i]],center,scale)
    traj = mt_add_variables(traj,var)
    }
  
  # add names
  dimnames(traj) = c(nam[[1]],nam[[2]],c(nam[[3]],paste0('z_',variables)))

  # store
  if(class(data) == 'mousetrap'){ 
    data[[use]] = traj
    return(data)
    } else {
    return(traj)
    }
}
