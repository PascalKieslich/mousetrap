#' Read data exported from MouseTracker.
#' 
#' \code{read_mousetracker} imports data that was collected using MouseTacker 
#' (Freeman & Ambady, 2010) and exported to a \code{.csv} file through 
#' MouseTracker Analyzer. It focuses on the raw data, and provides several 
#' options to remove pre-processed aggregate data (see Details).
#' 
#' MouseTracker by default includes the (per subject) aggregated trajectories in
#' the exported data set. As these are typically not desired in the trial-based 
#' dataset, \code{read_mousetracker} provides several ways of removing them.
#' 
#' Data that stem from a "time normalized analysis" in MouseTracker Analyzer, 
#' can be read into R using \code{read_mousetracker} with default arguments (the
#' aggregated trajectory data are automatically detected and removed, as they 
#' are marked by a row labeled "MEAN SUBJECT-BY-SUBJECT DATA" in the \code{.csv}
#' file).
#' 
#' Data that stem from a "raw time analysis" in MouseTracker Analyzer, also 
#' usually contain the aggregated trajectory data. However, as they are not 
#' marked by a specific row label in the \code{.csv} file, the number of last 
#' lines that should be omitted has to be specified explicitly using the 
#' argument \code{last_lines_to_rm}. Typically, this number is two times the 
#' number of data files.
#' 
#' After reading the data into R, \link{mt_import_wide} can be used to prepare 
#' the trajectory data for analyses using the mousetrap library.
#' 
#' The current version of \code{read_mousetracker} has been tested with data 
#' exported from MouseTracker Version 2.82.
#' 
#' @param file a character string. The name of the file which the data are to be
#'   read from. If it does not contain an absolute path, the file name is 
#'   relative to the current working directory.
#' @param remove_mean_data logical indicating if mean subject data (i.e., the 
#'   aggregated trajectories) should be automatically detected and removed. This
#'   only works for data from "time normalized analysis" in MouseTracker (see 
#'   Details).
#' @param last_lines_to_rm integer indicating the number of last lines to omit 
#'   from the imported data set. Typically used for data from "raw time 
#'   analysis" in MouseTracker (see Details). This option is independent of 
#'   \code{remove_mean_data}. The removal of the last lines is performed after 
#'   the optional removal of the auto-detected mean data.
#' @param remove_empty_columns logical indicating whether columns containing NAs
#'   only should be removed.
#' @return A \link{data.frame}.
#'
#' @references Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for
#' studying real-time mental processing using a computer mouse-tracking method. 
#' \emph{Behavior Research Methods, 42}(1), 226-241.
#'
#' @seealso
#' \link{mt_import_wide} to prepare the trajectory data for analyses in mousetrap.
#'
#' @examples
#' \dontrun{
#' # Data from "time normalized analysis" in MouseTracker
#' raw_data <- read_mousetracker("tn_data_exported.csv")
#'
#' # Data from "raw time analysis" in MouseTracker
#' raw_data <- read_mousetracker("raw_data_exported.csv",last_lines_to_rm=2*8)
#' }
#' @export
read_mousetracker <- function(file,
  remove_mean_data=TRUE, last_lines_to_rm=0,
  remove_empty_columns=TRUE) {
  
  # Read raw data
  raw_data <- utils::read.csv(file, 
    sep=",", dec=".", skip=1,
    stringsAsFactor=FALSE, na.strings=""
  )
  
  # Attempt to remove aggregate data
  if(remove_mean_data){
    mean_data_label_row <- grep("DATA", raw_data[,1])
    if (length(mean_data_label_row) == 1){
      raw_data <- raw_data[1:(mean_data_label_row - 1),]
    }
  }
  # Remove lines at the end of the file
  raw_data <- raw_data[1:(nrow(raw_data)-last_lines_to_rm),]
  # Remove empty columns
  if(remove_empty_columns){
    raw_data <- raw_data[,colSums(is.na(raw_data))!=nrow(raw_data)]
  }
  # Assign proper data types to all columns
  # (using character where appropriate, rather than
  # factors)
  for (i in 1:ncol(raw_data)){
    raw_data[,i] <- type.convert(as.character(raw_data[,i]), as.is=TRUE)
  }

  # Return dataset
  return(raw_data)
}
