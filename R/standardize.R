#' Scale and center variables within the levels of another variable.
#' 
#' \code{scale_within} centers and/or scales variables in a data.frame (using
#' \link{scale}) depending on the levels of another variable. By default, 
#' variables are standardized (i.e., centered and scaled). A typical application
#' is the within-subject standardization of variables in a repeated measures 
#' design.
#' 
#' @param data a \link{data.frame}.
#' @param variables a character string (or vector) specifying one or more
#'   variables scale is applied to. If unspecified, \code{scale_within} will be
#'   applied to all variables in data.
#' @param within a character string specifying the name of a variable in
#'   \code{data}. For each of the levels of this variable, \link{scale} is
#'   applied separately. Alternatively, a vector directly containing the level
#'   values.
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
#' @export
scale_within <-function(data,
  variables=NULL, within=NULL,
  prefix="", center=TRUE, scale=TRUE) {
  
  # If no variables are specified, operate on all columns
  if (is.null(variables)){
    variables <- colnames(data)
  }
  
  # If the within parameter contains only one value,
  # interpret this as the column name of the grouping
  # variable. Otherwise, interpret a vector of values
  # as a grouping factor.
  if(!is.null(within)){
    if (length(within)==1){
      within_values <- data[,within]
      variables <- variables[!variables==within]  
    } else {
      within_values <- within
    }
  }   
  
  for (var in variables){
    label <- paste(prefix,var,sep="")
    
    if (is.null(within)) {
      data[,label] <- scale(data[,var])
      
    } else {
      data[,label] <- NA
      for (i in unique(within_values)){
        data[within_values==i,label] <- 
          base::scale(data[within_values==i,var],
                      center=center,scale=scale)
      } 
    }
  }
  
  
  return(data)
}


#' Standardize mouse-tracking measures per level of another variable.
#' 
#' Standardize selected mouse-tracking measures per level of another variable
#' (e.g., per subject) and store them in new variables. This function is a
#' thin wrapper around \link{scale_within}, but focussed on mouse-tracking
#' data.
#' 
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which data should be used. By
#'   default points to the \code{measures} data.frame created using
#'   \link{mt_calculate_measures}.
#' @param use_variables a vector specifying which variables should be
#'   standardized. If unspecified, all variables will be standardized.
#' @param within a character string specifying a variable in
#'   \code{data[["data"]]}. All measures will be standardized separately for
#'   each level of the variable. By default, points to the subject identifier
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
#' mt_example <- mt_calculate_measures(mt_example)
#' mt_example <- mt_standardize(mt_example,
#'   use_variables=c("MAD", "AD"),
#'   within="subject_nr", prefix="z_")
#' 
#' @export
mt_standardize <-function(data, use="measures",
  use_variables=NULL, within="subject_nr",
  prefix="z_", center=TRUE, scale=TRUE) {
  
  data[[use]] <- extract_data(data=data,use=use)
  
  data[[use]] <- scale_within(
    data[[use]], variables=use_variables,
    within=data$data[,within],
    prefix=prefix, center=center,
    scale=scale
  )
  
  return(data)
}
