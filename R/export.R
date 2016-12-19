#' Export mouse-tracking data.
#' 
#' \code{mt_export_long} and \code{mt_export_wide} can be used for exporting 
#' mouse-tracking data from a mousetrap data object in long or wide format. If
#' desired, additional data (stored in \code{data[[use2]]}) can be merged with
#' the trajectory data before export. \code{mt_export_long} and 
#' \code{mt_export_wide} are wrapper functions for \link{mt_reshape}.
#' 
#' @inheritParams mt_reshape
#' @param use a character string specifying which data should be exported. The 
#'   corresponding data are selected from data using \code{data[[use]]}. 
#'   Usually, this value corresponds to either "trajectories" or
#'   "tn_trajectories", depending on whether the raw or the time-normalized
#'   trajectories should be exported.
#' @param use_variables a character vector specifying which mouse-tracking 
#'   variables should be exported. This corresponds to the labels of the
#'   trajectory array dimensions. If unspecified, all variables will be
#'   exported.
#' @param use2_variables an optional character string (or vector) specifying the
#'   variables (in \code{data[[use2]]}) that should be merged with the data.
#' @param ... additional arguments passed on to \link{mt_reshape} (such as 
#'   \code{subset}).
#' 
#' @return A \link{data.frame} containing the exported data.
#' 
#' @seealso
#' 
#' \link{mt_import_long} for importing mouse-tracking data saved in a long
#' format.
#' 
#' \link{mt_import_wide} for importing mouse-tracking data saved in a wide
#' format.
#' 
#' @examples
#' # Export data in long format
#' # (and include information about condition and subject_nr)
#' mt_data_long <- mt_export_long(mt_example,
#'   use2_variables=c("subject_nr","Condition"))
#' 
#' # Export data in wide format
#' # (and include information about condition and subject_nr)
#' mt_data_wide <- mt_export_wide(mt_example,
#'   use2_variables=c("subject_nr","Condition"))
#' 
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @describeIn mt_export_long Export mouse-tracking data in long format
#' @export
mt_export_long <- function(data,
  use="trajectories", use_variables=NULL,
  use2="data", use2_variables=NULL,
  ...) {
  
  return(mt_reshape(
    data=data,
    use=use, use_variables=use_variables,
    use2=use2, use2_variables=use2_variables,
    trajectories_long=TRUE,
    aggregate=FALSE,
    aggregate_subjects_only=FALSE,
    ...
  ))
}


#' @describeIn mt_export_long Export mouse-tracking data in wide format
#' @export
mt_export_wide <- function(data,
  use="trajectories", use_variables=NULL,
  use2="data", use2_variables=NULL,
  ...) {
  
  return(mt_reshape(
    data=data,
    use=use, use_variables=use_variables,
    use2=use2, use2_variables=use2_variables,
    trajectories_long=FALSE,
    aggregate=FALSE,
    aggregate_subjects_only=FALSE,
    ...
  ))
}
