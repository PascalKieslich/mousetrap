#' Import mouse-tracking data recorded using the mousetrap plug-ins in OpenSesame.
#'
#' \code{mt_import_mousetrap} accepts a data.frame of (merged) raw data from a 
#' mouse-tracking experiment implemented in 
#' \href{https://osdoc.cogsci.nl/}{OpenSesame} using the 
#' \href{https://github.com/pascalkieslich/mousetrap-os}{mousetrap plugin} 
#' (Kieslich & Henninger, 2017). From this data.frame, 
#' \code{mt_import_mousetrap} creates a mousetrap data object containing the 
#' trajectories and additional data for further processing within the mousetrap 
#' package. Specifically, it returns a list that includes the trajectory data as
#' an array (called \code{trajectories}), and all other data as a data.frame 
#' (called \code{data}). This data structure can then be passed on to other 
#' functions within this package (see \link{mousetrap} for an overview).
#'
#' When working with mouse-tracking data that were recorded using the mousetrap
#' plug-ins for OpenSesame, usually only the \code{raw_data} need to be
#' provided. All other arguments have sensible defaults.
#'
#' If the relevant timestamps, x-positions, and y-positions are each stored in
#' one variable, a character string specifying (parts of) the respective column
#' name needs to be provided. In this case, the column names are extracted using
#' \link{grep} to find the column that starts with the respective character
#' string (in OpenSesame these will typically contain the name of the item that
#' was used to record them, such as \code{xpos_get_response}). This means that
#' the exact column names do not have to be provided - as long as only one
#' column starts with the respective character string (otherwise, the exact
#' column names have to be provided).
#'
#' If several variables contain the timestamps, x-positions, and y-positions
#' within a trial (e.g., \code{xpos_part1} and \code{xpos_part2}), a vector of
#' the exact column names has to be provided (e.g.,
#' \code{xpos_label=c("xpos_part1","xpos_part2"))}. \code{mt_import_mousetrap}
#' will then merge all raw data in the order with which the variable labels have
#' been specified. If one variable contains NAs or an empty string in a trial,
#' these cases will be ignored (this covers the special case that, e.g.,
#' \code{xpos_part2} is only relevant for some trials and contains NAs in the
#' other trials).
#'
#' \code{duplicates} allows for different options to handle duplicate timestamps
#' within a trial: \itemize{
#' \item{\code{remove_first}: First timestamp and corresponding x-/y-positions
#' are removed (the default).}
#' \item{\code{remove_last}: Last timestamp and corresponding x-/y-positions are
#' removed.}
#' \item{\code{ignore}: Duplicates are kept.} }
#'
#' \code{unordered} allows for different options to handle unordered, that is,
#' non-monotonically increasing timestamps, within a trial: \itemize{
#' \item{\code{warn}: A warning is issued if unordered timestamps are
#' encountered in a trial (the default).}
#' \item{\code{remove}: Unordered timestamps within a trial are removed. This
#' means that any timestamp that is smaller than its predecessor will be removed
#' along with the corresponding x-/y-position.}
#' \item{\code{ignore}: Unordered timestamps are kept and no warning is issued.} }
#'
#' @param raw_data a data.frame containing the raw data.
#' @param xpos_label a character string specifying the name of the column(s) in
#'   which the x-positions are stored (see Details).
#' @param ypos_label a character string specifying the name of the column(s) in
#'   which the y-positions are stored (see Details).
#' @param timestamps_label a character string specifying the name of the column(s)
#'   in which the timestamps are stored (see Details).
#' @param mt_id_label an optional character string (or vector) specifying the
#'   name of the column that provides a unique ID for every trial (the trial
#'   identifier). If unspecified (the default), an ID variable will be
#'   generated. If more than one variable name is provided, a new ID variable
#'   will be created by combining the values of each variable. The trial
#'   identifier will be set as the \link{rownames} of the resulting trajectories
#'   and trial data, and additionally be stored in the column "mt_id" in the
#'   trial data.
#' @param split a character string indicating how the different timestamps and
#'   coordinates within a trial are separated.
#' @param duplicates a character string indicating how duplicate timestamps
#'   within a trial are handled (see Details).
#' @param unordered a character string indicating how unordered (i.e.,
#'   non-monotonically increasing) timestamps within a trial are handled (see
#'   Details).
#' @param reset_timestamps logical indicating if the first timestamp should be
#'   subtracted from all timestamps within a trial. Default is \code{TRUE} as it
#'   is recommended for all following analyses in mousetrap.
#' @param digits an optional integer. If specified, timestamps will be rounded.
#'   Potentially useful if timestamps are recorded with submillisecond
#'   precision.
#' @param verbose logical indicating whether function should report its
#'   progress.
#'
#' @return A mousetrap data object (see \link{mt_example}).
#'
#'   If mouse-tracking data were recorded using the mousetrap plug-ins for
#'   OpenSesame, the unit of the timestamps is milliseconds.
#'
#' @references Kieslich, P. J., & Henninger, F. (2017). Mousetrap: An
#'   integrated, open-source mouse-tracking package. \emph{Behavior Research
#'   Methods, 49}(5), 1652-1667. \url{https://doi.org/10.3758/s13428-017-0900-z}
#'
#' @seealso \link[readbulk]{read_opensesame} from the \code{readbulk} library
#'   for reading and combining raw data files that were collected with
#'   OpenSesame.
#'
#' \link{mt_import_wide} and \link{mt_import_long} for importing mouse-tracking
#' data from other sources.
#'
#' @examples
#' mt_data <- mt_import_mousetrap(mt_example_raw)
#'
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_import_mousetrap <- function(raw_data,
  xpos_label="xpos", ypos_label="ypos",
  timestamps_label="timestamps",
  mt_id_label=NULL,
  split=",",
  duplicates="remove_first",
  unordered="warn",
  reset_timestamps=TRUE,
  digits=NULL,
  verbose=FALSE) {
  
  # Set labels
  timestamps <- "timestamps"
  xpos <- "xpos"
  ypos <- "ypos"

  # Ensure that raw_data is a data.frame
  raw_data <- as.data.frame(raw_data)

  # Add mt_id variable
  if (is.null(mt_id_label)) {
    if (verbose) {
      message("No mt_id_label provided. ",
              "A new trial identifying variable called mt_id was created.")
    }

    # if no column name for ID variable is provided create one
    ids <- 1:nrow(raw_data)
    # use formatC to add leading 0s
    raw_data[,"mt_id"] <- paste(
      "id",
      formatC(ids, width=trunc(log10(nrow(raw_data))) + 1, flag="0"),
      sep=""
    )
  } else {

    # Extract values of mt_id variable (and convert them to character)
    ids <- as.character(raw_data[,mt_id_label[[1]]])

    # If more than one trial identifying variable is specified,
    # combine them into one unique identifier.
    if(length(mt_id_label) > 1) {
      for(var in mt_id_label[-1]) {
        ids <- paste0(ids,'_',raw_data[,var])
      }
    }

    # Add mt_id column to raw_data
    raw_data[,"mt_id"] <- ids

    if(anyDuplicated(ids) > 0) {
      stop("Values in specified mt_id_label variable are not unique.")
    }

  }

  # Set rownames of raw_data to trial identifier
  rownames(raw_data) <- raw_data[,"mt_id"]

  # Get length of label variables
  n_labels <- c(length(xpos_label), length(ypos_label), length(timestamps_label))

  # If more than one label per variable is provided,
  # join data stored in the different variables
  if (any(n_labels > 1)) {

    if (any(n_labels != n_labels[1])) {
      stop("xpos_label, ypos_label, and timestamps_label differ in their length.")
    }

    join_data <- function(m) {
      m[is.na(m)] <- ""
      return(apply(m, MARGIN=1, paste, collapse=split))
    }

    raw_data[,timestamps_label[1]] <- join_data(raw_data[,timestamps_label,drop=FALSE])
    raw_data[,xpos_label[1]] <- join_data(raw_data[,xpos_label,drop=FALSE])
    raw_data[,ypos_label[1]] <- join_data(raw_data[,ypos_label,drop=FALSE])

    mt_labels <- c(timestamps_label[1], xpos_label[1], ypos_label[1])
    names(mt_labels) <- c(timestamps, xpos, ypos)
    columns <- mt_labels
    names(columns) <- mt_labels

  # If only one label per variable is provided, allow for partial specification
  # of the variable name and search for it in the data.frame
  } else {

    check_columns <- function(var) {

      # Extract all columns starting with the respective string
      colname <- grep(paste0("^", var), colnames(raw_data), value=TRUE)
      if (length(colname) == 1) {
        return(colname)
      } else if (length(colname) > 0) {
        stop(paste(
          "More than one variable in data.frame starts with the label",
          var, "- please specify a unique label."
        ))
      } else {
        stop(
          "No variable in data.frame starts with the label ",
          var,
          " - please specify a correct variable."
        )
      }
    }

    mt_labels <- c(timestamps_label, xpos_label, ypos_label)
    names(mt_labels) <- c(timestamps, xpos, ypos)
    columns <- sapply(mt_labels, check_columns)
    names(columns) <- mt_labels
  }

  # Split data
  split_raw_data <- function(x) {

    # Remove all irrelevant characters
    x <- gsub(pattern=paste0("[^-0123456789.",split,"]"),replacement = "", x)

    # Remove leading / end / double split characters
    x <- gsub(pattern=paste0("^",split),replacement = "", x)
    x <- gsub(pattern=paste0(split,"$"),replacement = "", x)
    x <- gsub(pattern=paste0(split,split),replacement = "", x)

    # Split according to specified character
    x <- strsplit(x, split=split)

    return(as.numeric(unlist(x)))
  }

  data_list <- apply(raw_data[,columns], c(1, 2), split_raw_data)

  # Determine maximum number of logs
  if (inherits(data_list,"matrix")) {
    max_logs <- max(sapply(data_list,length))
  } else {
    max_logs <- dim(data_list)[1]
  }

  # Create array with raw MT data
  trajectories <- array(
    dim=c(nrow(raw_data), max_logs, 3),
    dimnames=list(raw_data[,"mt_id"], NULL, c(timestamps,xpos,ypos))
  )

  for (i in 1:dim(trajectories)[1]) {
    # Extract data from list (regular case)
    if (inherits(data_list,"matrix")) {
      for (j in names(mt_labels)) {
        mt_l <- columns[mt_labels[j]]

        if(length(data_list[i,][[mt_l]]) > 0){ ## only extract if there is data
          trajectories[i, 1:length(data_list[i,][[mt_l]]), j] <- data_list[i,][[mt_l]]
        }

      }
    # Special case (only one trajectory or equal number of logs)
    } else {
      trajectories[i,,timestamps] <- data_list[,i,columns[timestamps_label]]
      trajectories[i,,xpos] <- data_list[,i,columns[xpos_label]]
      trajectories[i,,ypos] <- data_list[,i,columns[ypos_label]]
    }

    # Check timestamps

    # Extract timestamps
    current_timestamps <- trajectories[i,,timestamps]
    current_timestamps <- current_timestamps[1:sum(!is.na(current_timestamps))]

    # Check that timestamps are monotonically increasing
    if (unordered != "ignore") {
      
      if (unordered %in% c("warn", "remove")) {
        
        if (any(diff(current_timestamps) < 0)) {
          if (unordered=="warn"){
            warning(
              "Trajectory encountered where timestamps are not monotonically increasing."
            )
            
          } else{
            current_xpos <- trajectories[i, 1:length(current_timestamps), xpos]
            current_ypos <- trajectories[i, 1:length(current_timestamps), ypos]
            trajectories[i,,] <- NA
            keep <- c(TRUE,diff(current_timestamps) >= 0)
            current_timestamps <- current_timestamps[keep]
            trajectories[i,1:length(current_timestamps),timestamps] <- current_timestamps
            trajectories[i,1:length(current_timestamps),xpos] <- current_xpos[keep]
            trajectories[i,1:length(current_timestamps),ypos] <- current_ypos[keep]
            warning(
              "Trajectory encountered where timestamps are not monotonically increasing. ",
              "The corresponding timestamps were removed."
            )
          }
        }
        
      } else {
        stop(
          "Please specify correct value for unordered: ",
          "warn, remove, or ignore"
        )
      }
    }

    # Check for duplicates
    if (duplicates != "ignore") {
      if (duplicates %in% c("remove_first", "remove_last")) {

        if (anyDuplicated(current_timestamps) > 0) {
          current_xpos <- trajectories[i, 1:length(current_timestamps), xpos]
          current_ypos <- trajectories[i, 1:length(current_timestamps), ypos]
          trajectories[i,,] <- NA

          keep <- !duplicated(current_timestamps, fromLast = duplicates=="remove_first")
          current_timestamps <- current_timestamps[keep]
          trajectories[i,1:length(current_timestamps),timestamps] <- current_timestamps
          trajectories[i,1:length(current_timestamps),xpos] <- current_xpos[keep]
          trajectories[i,1:length(current_timestamps),ypos] <- current_ypos[keep]
        }

      } else {
        stop(
          "Please specify correct value for duplicates: ",
          "ignore, remove_first, or remove_last"
        )
      }

    }

    if (verbose && i %% 100 == 0) {
      message(paste(i, "trials finished"))
    }
  }

  if (verbose) {
    message(paste("all", i, "trials finished"))
  }

  # Check if there are trials with no logs
  if (any(is.na(trajectories[,1,]))) {
    na_trials <- rowSums(is.na(trajectories[,1,,drop=FALSE]))
    warning(paste(
      "The following trials do not contain any logging data for at least one variable:",
      paste(names(na_trials[na_trials>0]), collapse=", ")
    ))
  }

  # Check for each trial, if the number of logs is the same for every variale
  nlogs <- apply(trajectories, c(1,3), function(x) {sum(!is.na(x))})
  nlogs_sd <- apply(nlogs, 1, stats::sd)
  if (any(nlogs_sd > 0)) {
    warning(paste(
      "In the following trials, the number of logs is not the same for all variables:",
      paste(names(nlogs_sd[nlogs_sd>0]), collapse=", ")
      ))
  }


  # Subtract first timestamp for each trial
  if (reset_timestamps) {
    trajectories[,,timestamps] <- trajectories[,,timestamps] - trajectories[,1,timestamps]
  }
  
  # Round timestamps (optional)
  if (!is.null(digits)){
    trajectories[, , timestamps] <- round(trajectories[, , timestamps],digits=digits)
  }

  # Drop raw data columns
  if (any(n_labels > 1)) {
    raw_data <- raw_data[, !colnames(raw_data) %in%
      c(timestamps_label,xpos_label,ypos_label)]
  } else {
    raw_data <- raw_data[, !colnames(raw_data) %in% columns]
  }
  
  result <- c(list("data"=raw_data, "trajectories"=trajectories))
  class(result) <- "mousetrap"

  return(result)
}


#' Import mouse-tracking data saved in wide format.
#'
#' \code{mt_import_wide} receives a data.frame where mouse-tracking data are 
#' stored in wide format, i.e., where one row contains the data of one trial and
#' every recorded mouse position and variable is saved in a separate variable 
#' (e.g., X_1, X_2, ..., Y_1, Y_2, ...). This is, e.g., the case when collecting
#' data using \href{http://www.mousetracker.org/}{MouseTracker} (Freeman &
#' Ambady, 2010). From this data.frame, \code{mt_import_wide} creates a
#' mousetrap data object containing the trajectories and additional data for
#' further processing within the mousetrap package. Specifically, it returns a
#' list that includes the trajectory data as an array (called
#' \code{trajectories}), and all other data as a data.frame (called
#' \code{data}). This data structure can then be passed on to other functions
#' within this package (see \link{mousetrap} for an overview).
#'
#' \code{mt_import_wide} is designed to import mouse-tracking data saved in a 
#' wide format. The defaults are set so that usually only the \code{raw_data} 
#' need to be provided when data have been collecting using MouseTracker
#' (Freeman & Ambady, 2010) and have been read into R using \link{read_mt}.
#'
#' If no \code{pos_ids} are provided, column labels for the respective variable 
#' (e.g., x-positions) are extracted using \code{\link{grep}} returning all 
#' variables that start with the respective character string (e.g., "X_" if 
#' \code{xpos_label="X"} and \code{pos_sep="_"}).
#'
#' If no timestamps are found in the data, \code{mt_import_wide} automatically
#' creates a timestamps variable with increasing integers (starting with 0)
#' assuming equally spaced sampling intervals.
#'
#' @inheritParams mt_import_mousetrap
#' @param raw_data a data.frame containing the raw data.
#' @param xpos_label a character string specifying the core of the column labels
#'   containing the x-positions (e.g., "X" for "X_1", "X_2", ...).
#' @param ypos_label a character string specifying the core of the column labels
#'   containing the y-positions (e.g., "Y" for "Y_1", "Y_2", ...).
#' @param zpos_label a character string specifying the core of the column labels
#'   containing the z-positions.
#' @param timestamps_label an optional character string specifying the core of
#'   the column labels containing the timestamps. If no timestamps are found in
#'   the data, a timestamps variable with increasing integers will be created
#'   (assuming equidistant time steps).
#' @param add_labels a character vector specifying the core of columns
#'   containing additional mouse-tracking variables.
#' @param pos_sep a character string indicating the character that connects the
#'   core label and the position, (e.g., "_" for "X_1", "Y_1", ...).
#' @param pos_ids the vector of IDs used for indexing the x-coordinates,
#'   y-coordinates etc. (e.g., 1:101 for time-normalized trajectories from
#'   MouseTracker). If unspecified (the default), column labels for the
#'   respective variable will be extracted using grep (see Details).
#'
#' @return A mousetrap data object (see \link{mt_example}).
#'
#' @references Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for
#' studying real-time mental processing using a computer mouse-tracking method.
#' \emph{Behavior Research Methods, 42}(1), 226-241.
#'
#' @seealso \link{read_mt} for reading raw data that was collected using
#'   MouseTracker (Freeman & Ambady, 2010) and stored as a file in the ".mt"
#'   format.
#' 
#' \link{mt_import_mousetrap} and \link{mt_import_long} for importing
#' mouse-tracking data in other formats.
#'
#' @examples
#' # Create data in wide format for test purposes
#' mt_data_wide <- mt_export_wide(mt_example,
#'   use2_variables=c("subject_nr", "Condition"))
#'
#' # Import the data using mt_import_wide
#' mt_data <- mt_import_wide(mt_data_wide,
#'   xpos_label="xpos", ypos_label="ypos",
#'   timestamps_label="timestamps")
#' 
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_import_wide <- function(raw_data,
  xpos_label="X", ypos_label="Y", zpos_label=NULL,
  timestamps_label="T",
  add_labels=NULL,
  mt_id_label=NULL,
  pos_sep="_", pos_ids=NULL,
  reset_timestamps=TRUE,
  verbose=TRUE) {

  # Ensure that raw_data is a data.frame
  raw_data <- as.data.frame(raw_data)

  # Add mt_id variable
  if (is.null(mt_id_label)) {
    if (verbose) {
      message("No mt_id_label provided. ",
              "A new trial identifying variable called mt_id was created.")
    }

    # if no column name for ID variable is provided, create one
    ids <- 1:nrow(raw_data)
    # use formatC to add leading 0s
    raw_data[,"mt_id"] <- paste(
      "id",
      formatC(ids, width=trunc(log10(nrow(raw_data)))+1, flag="0"),
      sep=""
      )
  } else {

    # Extract values of mt_id variable (and convert them to character)
    ids <- as.character(raw_data[,mt_id_label[[1]]])

    # If more than one trial identifying variable is specified,
    # combine them into one unique identifier.
    if(length(mt_id_label)>1){
      for(var in mt_id_label[-1]){
        ids <- paste0(ids,'_',raw_data[,var])
      }
    }

    # Add mt_id column to raw_data
    raw_data[,"mt_id"] <- ids


    if(anyDuplicated(ids) > 0) {
      stop("Values in specified mt_id_label variable are not unique.")
    }

  }

  # Set rownames of raw_data to trial identifier
  rownames(raw_data) <- raw_data[,"mt_id"]


  # Collect and rename variables
  timestamps <- "timestamps"
  mt_labels = c(timestamps=timestamps_label,
                xpos=xpos_label, ypos=ypos_label, zpos=zpos_label)
  if(is.null(add_labels)==FALSE){
    names(add_labels) <- add_labels
    mt_labels <- c(mt_labels, add_labels)
  }

  # Create an empty list for storing the column names of each variable
  mt_columns <- vector("list", length=length(mt_labels))
  names(mt_columns) <- names(mt_labels)

  if (is.null(pos_ids)){
    if(verbose){
      message(
        "No pos_ids provided. ",
        "The following variables were found using grep:"
      )
    }

  }

  for (mt_var in names(mt_labels)) {

    # Create specific column names if pos_ids are provided
    if (!is.null(pos_ids)) {
      mt_columns[[mt_var]] <- paste(
        mt_labels[[mt_var]],
        pos_ids, sep=pos_sep
      )

      # Check if columns exist and, if not, ...
      if (all(mt_columns[[mt_var]] %in% colnames(raw_data))==FALSE){
        # ... tolerate it for timestamps (and add them later)
        if (mt_var==timestamps){
          mt_labels <- mt_labels[names(mt_labels)!=timestamps]
          # ... return an error for all other variables
        } else {
          stop("No variables found for ",mt_var,".")
        }
      }

    # Extract column names using grep otherwise
    } else {
      mt_columns[[mt_var]] <-  grep(
        paste0("^", mt_labels[[mt_var]],pos_sep),
        colnames(raw_data),
        value=TRUE
      )

      n_variables_found <- length(mt_columns[[mt_var]])

      # If variables are found, return them
      if (n_variables_found>0){
        if(verbose){
          message(n_variables_found," variables found for ",mt_var,".")
        }

      # If no variables are found, ...
      } else {
        # ... tolerate it for timestamps (and add them later)
        if (mt_var==timestamps){
          mt_labels <- mt_labels[names(mt_labels)!=timestamps]
        # ... return an error for all other variables
        } else {
          stop("No variables found for ",mt_var,".")
        }
      }
    }
  }

  # Create array with MT data & drop raw data columns in original data.frame
  max_logs <- max(sapply(mt_columns,length))

  trajectories <- array(
    dim=c(nrow(raw_data), max_logs, length(mt_labels)),
    dimnames=list(raw_data[,"mt_id"], NULL, names(mt_labels)))

  for (mt_var in names(mt_labels)) {
    trajectories[,,mt_var] <- as.matrix(raw_data[,mt_columns[[mt_var]]])
    raw_data <- raw_data[, !colnames(raw_data) %in% mt_columns[[mt_var]], drop=FALSE]
  }

  # If no timestamps are found in the data, create timestamps
  if (!timestamps %in% names(mt_labels)) {
    if (verbose) {
      message(
        "0 variables found for ", timestamps, ". ",
        "Artificial timestamps variable created assuming equidistant time steps."
      )
    }
    timestamps_matrix <- matrix(
      0:(dim(trajectories)[2]-1),
      nrow=nrow(trajectories), ncol=dim(trajectories)[2],
      byrow=TRUE
    )
    # Add NAs for timestamps (corresponding to NAs for first dimension)
    timestamps_matrix[is.na(trajectories[,,1])] <- NA

    # Add timestamps to trajectories
    trajectories <- mt_add_variables(trajectories, variables=list(timestamps=timestamps_matrix))


  # Subtract first timestamp for each trial
  # if real timestamps are provided and option was selected
  } else {
    if (reset_timestamps) {
      trajectories[,,timestamps] <- trajectories[,,timestamps] - trajectories[,1,timestamps]
    }
  }
  
  result <- c(list("data"=raw_data, "trajectories"=trajectories))
  class(result) <- "mousetrap"
  
  return(result)
  
}


#' Import mouse-tracking data saved in long format.
#'
#' \code{mt_import_long} receives a data.frame in which mouse-tracking data are
#' stored in long format, i.e., where one row contains the logging data
#' (timestamp, x- and y-position etc.) at one specific point in the trial. This
#' is, for example, the case when exporting the trajectory data from the
#' mousetrap package using \link{mt_export_long}. From this data.frame,
#' \code{mt_import_long} creates a mousetrap data object containing the
#' trajectories and additional data for further processing within the mousetrap
#' package. Specifically, it returns a list that includes the trajectory data as
#' an array (called \code{trajectories}), and all other data as a data.frame
#' (called \code{data}). This data structure can then be passed on to other
#' functions within this package (see \link{mousetrap} for an overview).
#' 
#' The default arguments are set so that no adjustments have to be made when
#' importing a data.frame that was created using \link{mt_export_long}.
#'
#' The coordinates are ordered according to the values in the column provided in
#' the \code{mt_seq_label} parameter (\code{mt_seq} by default). If the
#' corresponding column does not exist, the coordinates will be imported in the
#' order in which they were stored in the raw_data.
#'
#' If no timestamps are found in the data, \code{mt_import_long} automatically
#' creates a timestamps variable with increasing integers (starting with 0)
#' assuming equally spaced sampling intervals.
#'
#' @inheritParams mt_import_mousetrap
#' @param raw_data a data.frame in long format, containing the raw data.
#' @param xpos_label a character string specifying the column containing the
#'   x-positions.
#' @param ypos_label a character string specifying the column containing the
#'   y-positions.
#' @param zpos_label an optional character string specifying the column
#'   containing the z-positions.
#' @param timestamps_label a character string specifying the column containing
#'   the timestamps. If no timestamps are found in the data, a timestamps
#'   variable with increasing integers will be created (assuming equidistant
#'   time steps).
#' @param add_labels a character vector specifying columns containing additional
#'   mouse-tracking variables.
#' @param mt_id_label a character string (or vector) specifying the name of the
#'   column that provides a unique ID for every trial (the trial identifier). If
#'   more than one variable name is provided, a new ID variable will be created
#'   by combining the values of each variable. The trial identifier will be set
#'   as the \link{rownames} of the resulting trajectories and trial data, and
#'   additionally be stored in the column "mt_id" in the trial data.
#' @param mt_seq_label a character string specifying the column that indicates
#'   the order of the logged coordinates within a trial. If no column of the
#'   specified name is found in the data.frame, the coordinates will be imported
#'   in the order in which they were stored in \code{raw_data}.
#'
#' @return  A mousetrap data object (see \link{mt_example}).
#'
#' @seealso \link{mt_import_mousetrap} and \link{mt_import_wide} for importing
#'   mouse-tracking data in other formats.
#'
#' @examples
#' # Create data in long format for test purposes
#' mt_data_long <- mt_export_long(mt_example,
#'   use2_variables=c("subject_nr","Condition"))
#'
#' # Import the data using mt_import_long
#' mt_data <- mt_import_long(mt_data_long)
#'
#'
#' \dontrun{
#' # Import a hypothetical dataset that contains the
#' # custom mouse-tracking variables angle and velocity
#' mt_data <- mt_import_long(exp_data,
#'   add_labels= c("angle", "velocity"))
#' }
#' 
#' @author
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' Felix Henninger
#' 
#' @export
mt_import_long <- function(raw_data,
  xpos_label="xpos", ypos_label="ypos", zpos_label=NULL,
  timestamps_label="timestamps", add_labels=NULL,
  mt_id_label="mt_id", mt_seq_label="mt_seq",
  reset_timestamps=TRUE,
  verbose=TRUE) {

  # Ensure that raw_data is a data.frame
  raw_data <- as.data.frame(raw_data)

  # Extract values of mt_id variable (and convert them to character)
  ids <- as.character(raw_data[,mt_id_label[[1]]])

  # If more than one trial identifying variable is specified,
  # combine them into one unique identifier.
  if (length(mt_id_label) > 1) {
    for (var in mt_id_label[-1]) {
      ids <- paste0(ids, '_', raw_data[,var])
    }
  }

  # Add mt_id column to raw_data
  raw_data[,"mt_id"] <- ids
  mt_id <- "mt_id"

  # Get order of ids (to preserve original order)
  ids <- unique(ids)

  # Look for mt_seq variable (that indicates the order of the logs)
  if (is.null(mt_seq_label) | (mt_seq_label %in% colnames(raw_data) == FALSE)) {
    if (verbose) {
      message(
        "No mt_seq variable found (that indicates the order of the logs). ",
        "Importing data in sequential order."
      )
    }

    # Sort dataset according to mt_id
    raw_data <- raw_data[order(raw_data[,"mt_id"]),]

  } else {
    # Sort dataset according to mt_id and mt_seq
    raw_data <- raw_data[order(raw_data[,"mt_id"], raw_data[,mt_seq_label]),]

    # Remove mt_seq_label
    raw_data <- raw_data[,colnames(raw_data)!=mt_seq_label]
  }

  # Create mt_seq variable
  raw_data <- raw_data %>%
    dplyr::group_by(mt_id) %>%
    dplyr::mutate_(.dots = stats::setNames(list("dplyr::row_number()"), "mt_seq")) %>%
    dplyr::ungroup()

  # Collect and rename variables
  timestamps <- "timestamps"
  mt_labels = c(
    timestamps=timestamps_label,
    xpos=xpos_label, ypos=ypos_label, zpos=zpos_label
    )

  if (is.null(add_labels) == FALSE) {
    names(add_labels) <- add_labels
    mt_labels <- c(mt_labels, add_labels)
  }
  mt_include <- c()
  for (var in names(mt_labels)) {
    label <- mt_labels[[var]]
    if (label %in% colnames(raw_data)) {
      colnames(raw_data)[colnames(raw_data) == label] <- var
      mt_include <- c(mt_include, var)
    } else if (var!=timestamps) {
      stop("Variable '", label, "' not found in the raw data.")
    }
  }


  # Create array for selected variables
  n_logs <- dplyr::count(raw_data, mt_id)
  n_max <- max(n_logs$n)

  trajectories <- array(
    dim = c(nrow(n_logs),n_max, length(mt_include)),
    dimnames = list(n_logs$mt_id, NULL, mt_include))

  for (var in mt_include) {
    reshaped_data <- raw_data %>%
      dplyr::select_(.dots=c("mt_id", "mt_seq", var)) %>%
      tidyr::spread_("mt_seq", var)
    trajectories[,,var] <- as.matrix(reshaped_data[,-1])
    }

  # If no timestamps are found in the data, create timestamps
  if (!timestamps %in% mt_include) {
    if (verbose) {
      message(
        "No timestamps were found in the data. ",
        "Artificial timestamps variable created assuming equidistant time steps."
      )
    }

    timestamps_matrix <- matrix(
      0:(dim(trajectories)[2]-1),
      nrow=nrow(trajectories), ncol=dim(trajectories)[2],
      byrow=TRUE
    )
    # Add NAs for timestamps (corresponding to NAs for first dimension)
    timestamps_matrix[is.na(trajectories[,,1])] <- NA

    # Add timestamps to trajectories
    trajectories <- mt_add_variables(trajectories, variables=list(timestamps=timestamps_matrix))

  # Subtract first timestamp for each trial
  # if real timestamps are provided and option was selected
  } else {
    if (reset_timestamps) {
      trajectories[,,timestamps] <- trajectories[,,timestamps] - trajectories[,1,timestamps]
    }
  }

  # Order trajectories
  trajectories <- trajectories[ids,,]

  # Create data.frame from leftover variables
  raw_data <- as.data.frame(
    unique(raw_data[,!colnames(raw_data) %in% c(mt_include, "mt_seq")])
  )

  if (max(table(raw_data[,"mt_id"])) > 1) {
    # Issue warning if more than one line per mt_id remains
    warning(
      "After removing trajectory data, ",
      "more than one unique row per mt_id remains."
    )

  } else {
    # Set rownames of raw_data to trial identifier
    rownames(raw_data) <- raw_data[,"mt_id"]

    # Ensure order of raw_data
    raw_data <- raw_data[ids,]
  }
  
  result <- c(list("data"=raw_data, "trajectories"=trajectories))
  class(result) <- "mousetrap"
  
  return(result)
}
