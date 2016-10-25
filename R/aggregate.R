#' Aggregate mouse-tracking data per condition.
#'
#' \code{mt_aggregate} is used for aggregating mouse-tracking measures (or
#' trajectories) per condition. One or several condition variables can be
#' specified using \code{use2_variables}. Aggregation will be performed
#' separately for each level of the condition variables. \code{mt_aggregate} is
#' a wrapper function for \code{\link{mt_reshape}}.
#'
#' @inheritParams mt_reshape
#' @param use a character string specifying which dataset should be aggregated.
#'   The corresponding data are selected from \code{data} using
#'   \code{data[[use]]}. Usually, this value corresponds to either
#'   "tn_trajectories" or "measures", depending on whether the time-normalized
#'   trajectories or derived measures should be aggregated.
#' @param use_variables a character vector specifying the mouse-tracking 
#'   variables to aggregate. If a data.frame with mouse-tracking measures is
#'   provided as \code{data}, this corresponds to the column names. If a
#'   trajectory array is provided, this argument should specify the labels of
#'   respective array dimensions. If unspecified, all variables will be
#'   aggregated.
#' @param use2 a character string specifying where the data containing the
#'   condition information can be found. Defaults to "data" as
#'   \code{data[["data"]]} usually contains all non mouse-tracking trial data.
#'   Alternatively, a data.frame can be provided directly.
#' @param use2_variables a character string (or vector) specifying the variables
#'   (in \code{data[[use2]]}) across which the trajectories / measures will be
#'   aggregated. For each combination of levels of the grouping variable(s),
#'   aggregation will be performed separately using \link[dplyr]{summarize_at}.
#' @param subject_id an optional character string specifying the column that
#'   contains the subject identifier. If specified, aggregation will be
#'   performed within subjects first (i.e., within subjects for all available
#'   values of the grouping variables specified in \code{use2_variables}).
#' @param ... additional arguments passed on to \link{mt_reshape} (such as
#'   \code{subset}).
#'
#' @return A \link{data.frame} containing the aggregated data.
#'
#' @seealso
#'
#' \link{mt_aggregate_per_subject} for aggregating mouse-tracking measures and
#' trajectories per subject.
#'
#' \link[dplyr]{summarize_at} for aggregating data using the \code{dplyr}
#' package.
#'
#' @examples
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'
#' # Aggregate time-normalized trajectories per condition
#' average_trajectories <-  mt_aggregate(mt_example,
#'   use="tn_trajectories",
#'   use2_variables="Condition"
#' )
#'
#'
#' # Calculate mouse-tracking measures
#' mt_example <- mt_measures(mt_example)
#'
#' # Aggregate measures per condition
#' average_measures <- mt_aggregate(mt_example,
#'   use="measures", use_variables=c("MAD", "AD"),
#'   use2_variables="Condition"
#' )
#'
#' # Aggregate measures per condition
#' # first within subjects and then across subjects
#' average_measures <- mt_aggregate(mt_example,
#'   use="measures", use_variables=c("MAD", "AD"),
#'   use2_variables="Condition",
#'   subject_id="subject_nr"
#' )
#'
#' @export
mt_aggregate <- function(data,
                         use="measures", use_variables=NULL,
                         use2="data", use2_variables=NULL,
                         subject_id=NULL, trajectories_long=TRUE, ...) {

  return(mt_reshape(
    data=data,
    use=use, use_variables=use_variables,
    use2=use2, use2_variables=use2_variables,
    subject_id=subject_id,
    trajectories_long=trajectories_long,
    aggregate=TRUE,
    aggregate_subjects_only=FALSE,
    ...
  ))
}

#' Aggregate mouse-tracking data per condition separately for each subject.
#'
#' \code{mt_aggregate_per_subject} can be used for aggregating mouse-tracking
#' measures (or trajectories) per condition separately for each subject. One or
#' more condition variables can be specified using \code{use2_variables}.
#' Aggregation will be performed separately for each level of the condition
#' variables. \code{mt_aggregate_per_subject} is a wrapper function for
#' \link{mt_reshape}.
#'
#' @inheritParams mt_aggregate
#' @param subject_id a character string specifying which column contains the
#'   subject identifier.
#'
#' @return A \link{data.frame} containing the aggregated data.
#'
#' @seealso
#'
#' \link{mt_aggregate} for aggregating mouse-tracking measures and trajectories
#' per condition.
#'
#' \link[dplyr]{summarize_at} for aggregating data using the \code{dplyr}
#' package.
#'
#' @examples
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'
#' # Aggregate time-normalized trajectories per condition
#' # separately per subject
#' average_trajectories <- mt_aggregate_per_subject(
#'   mt_example,
#'   use="tn_trajectories",
#'   use2_variables="Condition",
#'   subject_id="subject_nr"
#' )
#'
#'
#' # Calculate mouse-tracking measures
#' mt_example <- mt_measures(mt_example)
#'
#' # Aggregate measures per condition
#' # separately per subject
#' average_measures <- mt_aggregate_per_subject(
#'   mt_example,
#'   use="measures",
#'   use_variables=c("MAD", "AD"),
#'   use2_variables="Condition",
#'   subject_id="subject_nr"
#' )
#'
#' @export
mt_aggregate_per_subject <- function(data,
                                     use="measures", use_variables=NULL,
                                     use2="data", use2_variables=NULL,
                                     subject_id, trajectories_long=TRUE, ...) {

  return(mt_reshape(
    data=data,
    use=use, use_variables=use_variables,
    use2=use2, use2_variables=use2_variables,
    subject_id=subject_id,
    trajectories_long=trajectories_long,
    aggregate=TRUE,
    aggregate_subjects_only=TRUE,
    ...
  ))
}
