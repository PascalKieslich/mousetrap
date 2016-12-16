#' Check logging resolution by looking at timestamp differences.
#' 
#' \code{mt_check_resolution} computes the timestamp differences as a measure of
#' the logging resolution. It provides various descriptive statistics to check
#' the logging resolution.
#' 
#' If mouse-tracking experiments are conducted using the mousetrap plug-ins for 
#' OpenSesame, the logging resolution can be specified explicitly in the 
#' experiment under "Logging resolution", which corresponds to the delay (in 
#' milliseconds) between recordings of the mouse position. By default, mouse 
#' positions are recorded every 10 ms (corresponding to a 100 Hz sampling rate).
#' As the actual resolution achieved depends on the performance of the hardware,
#' it makes sense to check the logging resolution using 
#' \code{mt_check_resolution}. Note that delays smaller than the specified delay
#' typically result from mouse clicks in the experiment.
#' 
#' 
#' @inheritParams mt_time_normalize
#' @param timestamps a character string specifying the trajectory dimension
#'   containing the timestamps.
#' @param desired an optional integer. If specified, additional statistics are
#'   computed concerning the (relative) frequencies with which exactly the
#'   desired timestamp difference (with tolerance 1e-12) occurred.
#'   
#' @return A list with various descriptive statistics. For convenience, the
#'   relative frequencies are rounded to 4 decimal places.
#'
#' @examples
#' mt_check_resolution(mt_example)
#' 
#' @export
mt_check_resolution <- function(data, use="trajectories",
  timestamps="timestamps", desired=NULL) {
  
  trajectories <- extract_data(data=data,use=use)

  # Compute steps in the timestamps
  if(dim( trajectories )[1] == 1) {
    log_diffs <- diff(trajectories[, , timestamps])
  } else {
    log_diffs <- diff(t(trajectories[, , timestamps]))
  }
  
  # Clean data type and remove empty values
  log_diffs <- as.numeric(log_diffs)
  log_diffs <- log_diffs[!is.na(log_diffs)]
  
  results <- list(
    summary=summary(log_diffs),
    sd=stats::sd(log_diffs),
    frequencies=table(log_diffs),
    relative_frequencies=round(
      table(log_diffs) / length(log_diffs), 4)
  )
  
  if (!is.null(desired)){
    log_diffs_class <-
      cut(log_diffs,
        c(0,desired-10^(-12),desired+10^(-12),Inf),
        c("smaller","desired","greater")
      )
    
    results <-
      c(results,
        list(
          frequencies_desired=table(log_diffs_class),
          relative_frequencies_desired=round(
            table(log_diffs_class) / length(log_diffs_class), 4)
        ))
    
  }

  return(results)  
}
