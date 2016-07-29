#' Import mouse-tracking data recorded using the mousetrap plug-ins in
#' OpenSesame.
#'
#' \code{mt_import_mousetrap} accepts a data.frame of (merged) raw data from a
#' mouse-tracking experiment implemented in OpenSesame using one of the
#' \href{https://github.com/pascalkieslich/mousetrap-os}{mousetrap plug-ins}.
#' From this data.frame, \code{mt_import_mousetrap} creates a mousetrap data
#' object containing the trajectories and additional data for further processing
#' within the mousetrap package. \code{mt_import_mousetrap} returns a list,
#' which includes the trajectory data as an array, and all other data as a
#' data.frame. This data structure can then be passed on to other functions
#' within this package, such as \link{mt_time_normalize} or
#' \link{mt_calculate_measures}.
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
#' within a trial: \itemize{ \item{\code{remove_first}: First timestamp and
#' corresponding x-/y-positions are removed (the default).}
#' \item{\code{remove_last}: Last timestamp and corresponding x-/y-positions are
#' removed.} \item{\code{ignore}: Duplicates are kept.} }
#'
#' @param raw_data a data.frame containing the raw data.
#' @param xpos_label a character string specifying the name of the column(s) in
#'   which the x-positions are stored (see Details).
#' @param ypos_label a character string specifying the name of the column(s) in
#'   which the y-positions are stored (see Details).
#' @param timestamps_label a character string specifying the name of the column(s)
#'   in which the timestamps are stored (see Details).
#' @param mt_id_label an optional character string specifying the name of the
#'   column that provides a unique ID for every trial in the raw data. If
#'   unspecified, an ID variable will be generated.
#' @param split a character string indicating how the different timestamps (and
#'   coordinates) within a trial are separated.
#' @param duplicates a character string indicating how duplicate timestamps
#'   within a trial are handled (see Details).
#' @param reset_timestamps logical indicating if the first timestamp should be
#'   subtracted from all timestamps within a trial. Default is TRUE as it is
#'   recommended for all following analyses in mousetrap.
#' @param show_progress logical indicating whether function should report its
#'   progress.
#'
#' @return A mousetrap data object (see \link{mt_example}).
#'
#'   If mouse-tracking data were recorded using the mousetrap plug-ins for
#'   OpenSesame, the unit of the timestamps is milliseconds.
#'
#' @references Mousetrap plug-ins for OpenSesame
#'   (\url{https://github.com/pascalkieslich/mousetrap-os})
#'
#' @seealso \link[readbulk]{read_opensesame} from the \code{readbulk} library
#'   for reading and combining raw data files that were collected with
#'   OpenSesame.
#'
#' \link{mt_import_wide} and \link{mt_import_long} for importing mouse-tracking
#' data from other sources.
#'
#' @examples
#' mt_example <- mt_import_mousetrap(mt_example_raw)
#'
#' @export
mt_import_mousetrap <- function(raw_data,
  xpos_label="xpos", ypos_label="ypos",
  timestamps_label="timestamps",
  mt_id_label=NULL,
  split=",", duplicates="remove_first",
  reset_timestamps=TRUE,
  show_progress=TRUE) {

  # Add mt_id variable
  if (is.null(mt_id_label)) {
    # if no column name for ID variable is provided create one
    ids <- 1:nrow(raw_data)
    # use formatC to add leading 0s
    raw_data[,mt_id] <- paste(
      "id",
      formatC(ids, width=trunc(log10(nrow(raw_data))) + 1, flag="0"),
      sep=""
    )
  } else {
    if(anyDuplicated(raw_data[,mt_id_label]) > 0){
      stop(
        "Values in specified mt_id variable - ",
        mt_id_label,
        " - are not unique."
      )
    }
    raw_data[,mt_id] <- raw_data[,mt_id_label]
  }

  rownames(raw_data) <- raw_data[,mt_id]
  
  # Get length of label variables
  n_labels <- c(length(xpos_label),length(ypos_label),length(timestamps_label))
  
  
  # If more than one label per variable is provided,
  # join data stored in the different variables
  if (any(n_labels>1)) {
    
    if (any(n_labels!=n_labels[1])){
      stop("xpos_label, ypos_label, and timestamps_label differ in their length.")
    }
    
    join_data <- function(m){
      m[is.na(m)] <- ""
      return(apply(m,MARGIN = 1,paste,collapse=split))
    }
    
    raw_data[,timestamps_label[1]] <- join_data(raw_data[,timestamps_label,drop=FALSE])  
    raw_data[,xpos_label[1]] <- join_data(raw_data[,xpos_label,drop=FALSE])  
    raw_data[,ypos_label[1]] <- join_data(raw_data[,ypos_label,drop=FALSE])  
    
    mt_labels <- c(timestamps=timestamps_label[1], xpos=xpos_label[1], ypos=ypos_label[1])
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
    
    mt_labels <- c(timestamps=timestamps_label, xpos=xpos_label, ypos=ypos_label)
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
  if (class(data_list) == "matrix") {
    max_logs <- max(sapply(data_list,length))
  } else {
    max_logs <- dim(data_list)[1]
  }

  # Extract labels for trajectory array
  timestamps <- mt_variable_labels[["timestamps"]]
  xpos <- mt_variable_labels[["xpos"]]
  ypos <- mt_variable_labels[["ypos"]]

  # Create array with raw MT data
  trajectories <- array(
    dim=c(nrow(raw_data), 3, max_logs),
    dimnames=list(raw_data[,mt_id], c(timestamps,xpos,ypos), NULL)
  )

  for (i in 1:dim(trajectories)[1]) {
    # Extract data from list (regular case)
    if (class(data_list) == "matrix") {
      for (j in names(mt_labels)) {
        mt_l <- columns[mt_labels[j]]
        if(length(data_list[i,][[mt_l]] != 0)){ ## no data in that variable  
          trajectories[i, mt_variable_labels[j], 1:length(data_list[i,][[mt_l]])] <- data_list[i,][[mt_l]]
        } 
      }
    # Special case (only one trajectory or equal number of logs)
    } else {
      trajectories[i,timestamps,] <- data_list[,i,columns[timestamps_label]]
      trajectories[i,xpos,] <- data_list[,i,columns[xpos_label]]
      trajectories[i,ypos,] <- data_list[,i,columns[ypos_label]]
    }
    
    
    # Check timestamps
    
    # Extract timestamps
    current_timestamps <- trajectories[i,timestamps,]
    current_timestamps <- current_timestamps[1:sum(!is.na(current_timestamps))]
    
    # Check that timestamps are monotonically increasing
    if (any(diff(current_timestamps)<0)) {
      warning("For some trajectories timestamps are not monotonically increasing.")
    }

    # Check for duplicates
    if (duplicates != "ignore") {
      if (duplicates %in% c("remove_first", "remove_last")) {
        
        if (anyDuplicated(current_timestamps) > 0) {
          current_xpos <- trajectories[i, xpos, 1:length(current_timestamps)]
          current_ypos <- trajectories[i, ypos, 1:length(current_timestamps)]
          trajectories[i,,] <- NA

          keep <- !duplicated(current_timestamps,fromLast = duplicates=="remove_first")
          current_timestamps <- current_timestamps[keep]
          trajectories[i,timestamps,1:length(current_timestamps)] <- current_timestamps
          trajectories[i,xpos,1:length(current_timestamps)] <- current_xpos[keep]
          trajectories[i,ypos,1:length(current_timestamps)] <- current_ypos[keep]
        }

      } else {
        stop(
          "Please specify correct value for duplicates: ",
          "ignore, remove_first, or remove_last"
        )
      }


    }

    if (show_progress && i %% 100 == 0) {
      message(paste(i, "trials finished"))
    }
  }

  if (show_progress) {
    message(paste("all", i, "trials finished"))
  }

  # Check if there are trials with no logs
  if (any(is.na(trajectories[,,1]))) {
    na_trials <- rowSums(is.na(trajectories[,,1,drop=FALSE]))
    warning(paste(
      "The following trials do not contain any logging data for at least one variable:",
      paste(names(na_trials[na_trials>0]), collapse=", ")
    ))
  }

  # Check for each trial, if the number of logs is the same for every variale
  nlogs <- apply(trajectories, c(1,2), function(x) {sum(!is.na(x))})
  nlogs_sd <- apply(nlogs, 1, stats::sd)
  if (any(nlogs_sd > 0)) {
    warning(paste(
      "In the following trials, the number of logs is not the same for all variables:",
      paste(names(nlogs_sd[nlogs_sd>0]), collapse=", ")
      ))
  }


  # Subtract first timestamp for each trial
  if (reset_timestamps){
    trajectories[,timestamps,] <- trajectories[,timestamps,] - trajectories[,timestamps,1]
  }

  # Drop raw data columns
  if (any(n_labels>1)) {
    raw_data <- raw_data[, !colnames(raw_data) %in% 
      c(timestamps_label,xpos_label,ypos_label)]
  } else {
    raw_data <- raw_data[, !colnames(raw_data) %in% columns]
  }

  return(c(list("data"=raw_data, "trajectories"=trajectories)))
}



#' Import mouse-tracking data saved in wide format.
#'
#' \code{mt_import_wide} receives a data.frame where mouse-tracking data are
#' stored in wide format, i.e., where one row contains the data of one trial and
#' every recorded mouse position is saved in a separate variable. This is, e.g.,
#' the case when exporting trajectories from MouseTracker (Freeman & Ambady,
#' 2010). From this data.frame, \code{mt_import_wide} creates a mousetrap data
#' object containing the trajectories and additional data for further processing
#' within the mousetrap package. \code{mt_import_wide} returns a list, which
#' includes the trajectory data as an array, and all other data as a data.frame.
#' This data structure can then be passed on to other functions within this
#' package, such as \link{mt_time_normalize} or \link{mt_calculate_measures}.
#'
#' \code{mt_import_wide} is designed to import mouse-tracking data saved in a
#' wide format. The defaults are set so that usually only the \code{raw_data}
#' and \code{pos_ids} need to be provided when importing trajectory data that
#' stem from a "time normalized analysis" in MouseTracker (Freeman & Ambady,
#' 2010).
#'
#' If no \code{pos_ids} are provided, column labels for the respective variable
#' (e.g., x-positions) are extracted using \link{grep} returning all variables
#' that start with the respective character string (e.g. "X" for x-positions).
#' This is, e.g., useful when importing trajectory data that stem from a "raw
#' time analysis" in MouseTracker (Freeman & Ambady, 2010).
#'
#' If no timestamps are provided, \code{mt_import_wide} automatically assumes
#' equally spaced sampling intervals and creates a timestamp variable with
#' increasing integers (starting with 0).
#'
#' If labels for distance, velocity, and acceleration are provided, these are
#' included in the trajectory array.
#'
#' @param raw_data a data.frame containing the raw data.
#' @param xpos_label a character string specifying the core of the column labels
#'   containing the x-positions (e.g., "X" for "X_1", "X_2", ...).
#' @param ypos_label a character string specifying the core of the column labels
#'   containing the y-positions (e.g., "Y" for "Y_1", "Y_2", ...).
#' @param timestamps_label an optional character string specifying the core of
#'   the column labels containing the timestamps.
#' @param dist_label an optional character string specifying the core of the
#'   column labels containing the distance traveled.
#' @param vel_label an optional character string specifying the core of the
#'   column labels containing the velocity.
#' @param acc_label an optional character string specifying the core of the
#'   column labels containing the acceleration.
#' @param mt_id_label an optional character string specifying the name of the
#'   column that provides a unique ID for every trial. If unspecified, an ID
#'   variable will be generated.
#' @param pos_sep a character string indicating the character that connects the
#'   core label and the position, (e.g., "_" for "X_1", "Y_1", ...).
#' @param pos_ids the vector of IDs used for indexing the x-coordinates,
#'   y-coordinates etc. (e.g., 1:101 for time-normalized trajectories from
#'   MouseTracker). If unspecified, column labels for the respective variable
#'   will be extracted using grep (see Details).
#' @param reset_timestamps logical indicating if the first timestamp should be
#'   subtracted from all timestamps within a trial. Default is TRUE as it is
#'   recommended for all following analyses in mousetrap.
#'
#' @return A mousetrap data object (see \link{mt_example}).
#'
#' @references Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for
#' studying real-time mental processing using a computer mouse-tracking method.
#' \emph{Behavior Research Methods, 42}(1), 226-241.
#'
#' @seealso \link{read_mousetracker} for reading data into R that were exported
#' from MouseTracker (Freeman & Ambady, 2010).
#'
#' \link{mt_import_mousetrap} and \link{mt_import_long} for importing
#' mouse-tracking data from other sources.
#'
#' @examples
#' \dontrun{
#'
#' # Data from "time normalized analysis" in MouseTracker
#' raw_data <- read_mousetracker("tn_data_exported.csv")
#' data <- mt_import_wide(raw_data, pos_ids=1:101)
#'
#' # Data from "raw time analysis" in MouseTracker
#' raw_data <- read_mousetracker("raw_data_exported.csv", last_lines_to_rm=2*8)
#' data <- mt_import_wide(raw_data)
#'
#' }
#' @export
mt_import_wide <- function(raw_data,
  xpos_label="X", ypos_label="Y",
  timestamps_label=NULL,
  dist_label=NULL, vel_label=NULL, acc_label=NULL,
  mt_id_label=NULL,
  pos_sep="_", pos_ids=NULL,
  reset_timestamps=TRUE) {

  # Add mt_id variable
  if (is.null(mt_id_label)) {
    # if no column name for ID variable is provided create one
    ids <- 1:nrow(raw_data)
    # use formatC to add leading 0s
    raw_data[,mt_id] <- paste(
      "id",
      formatC(ids, width=trunc(log10(nrow(raw_data)))+1, flag="0"),
      sep=""
      )
  } else {
    if(anyDuplicated(raw_data[,mt_id_label]) > 0) {
      stop(paste(
        "Values in specified mt_id variable -",
        mt_id_label, "- are not unique."
      ))
    }
    raw_data[,mt_id] <- raw_data[,mt_id_label]
  }

  rownames(raw_data) <- raw_data[,mt_id]

  # Extract timestamps from mt_variable_labels
  timestamps <- mt_variable_labels[["timestamps"]]
  xpos <- mt_variable_labels[["xpos"]]

  # Create list with variables
  mt_variable_labels <- list(
    xpos = list("label"=xpos_label, "name"=mt_variable_labels[["xpos"]]),
    ypos = list("label"=ypos_label, "name"=mt_variable_labels[["ypos"]]),
    timestamps = list("label"=timestamps_label, "name"=mt_variable_labels[["timestamps"]]),
    dist = list("label"=dist_label, "name"=mt_variable_labels[["dist"]]),
    vel = list("label"=vel_label, "name"=mt_variable_labels[["vel"]]),
    acc = list("label"=acc_label, "name"=mt_variable_labels[["acc"]])
  )

  if (is.null(pos_ids)){
    message(
      "No pos_ids provided. ",
      "The following variables were extracting using grep:"
      )
  }

  for (i in names(mt_variable_labels)) {

    if(!is.null(mt_variable_labels[[i]][["label"]])) {

      # Create column names if pos_ids are specified
      if (!is.null(pos_ids)) {
        mt_variable_labels[[i]][["cols"]] <- paste(
          mt_variable_labels[[i]][["label"]],
          pos_ids, sep=pos_sep
        )

      # Extract column names using grep otherwise
      } else {
        mt_variable_labels[[i]][["cols"]] <- grep(
          paste0("^", mt_variable_labels[[i]][["label"]]),
          colnames(raw_data),
          value=TRUE
        )
        message(paste0(i, ":"))
        message(paste(mt_variable_labels[[i]][["cols"]], collapse=","))
      }

    } else {
      if (i != "timestamps") {
        mt_variable_labels[[i]] <- NULL
      }
    }
  }

  max_logs <- max(sapply(mt_variable_labels, function(x){length(x[["cols"]])}))

  # Create array with MT data & drop raw data columns in original data.frame
  trajectories  <- array(
    dim=c(nrow(raw_data), length(mt_variable_labels), max_logs),
    dimnames=list(raw_data[,mt_id], as.vector(sapply(mt_variable_labels, function(x){x[["name"]]})), NULL))

  for (mt_var in mt_variable_labels) {
    if (!is.null(mt_var[["label"]])) {
      trajectories[,mt_var[["name"]],] <- as.matrix(raw_data[,mt_var[["cols"]]])
      raw_data <- raw_data[, !colnames(raw_data) %in% mt_var[["cols"]], drop=FALSE]

    # If no timestamp label is provided, create timestamps
    } else {
      trajectories[,timestamps,] <- matrix(
        0:(max_logs-1),
        nrow=nrow(trajectories), ncol=max_logs,
        byrow=TRUE
      )
      # add NAs for timestamps (corresponding to NAs for xpos)
      trajectories[,timestamps,][is.na(trajectories[,xpos,])] <- NA
    }
  }

  # Subtract first timestamp for each trial
  if (reset_timestamps) {
    trajectories[,timestamps,] <- trajectories[,timestamps,] -
      trajectories[,timestamps,1]
  }

  return(c(list("data"=raw_data, "trajectories"=trajectories)))

}


#' Import mouse-tracking data saved in long format.
#'
#' \code{mt_import_long} receives a data.frame in which mouse-tracking data are
#' stored in long format, i.e., where one row contains the logging data
#' (timestamp, x- and y-position etc.) at one specific point in the trial. This
#' is, for example, the case when exporting the trajectory data from the
#' mousetrap package using \link{mt_reshape}. From this data.frame,
#' \code{mt_import_long} creates a mousetrap data object containing the
#' trajectories and additional data for further processing within the mousetrap
#' package. \code{mt_import_long} returns a list, which includes the trajectory
#' data as an array, and all other data as a data.frame. This data structure can
#' then be passed on to other functions within this package, such as
#' \link{mt_time_normalize} or \link{mt_calculate_measures}. The defaults are
#' set so that no adjustments have to be made when importing a data.frame that
#' was created using \link{mt_reshape}.
#'
#' The coordinates are ordered according to the values in the column provided in
#' the \code{mt_seq_label} parameter (\code{mt_seq} by default). If the
#' corresponding column does not exist, the coordinates will be ordered
#' according to their timestamps (drawn from the \code{timestamps} column by
#' default).
#'
#' @param raw_data a data.frame in long format, containing the raw data.
#' @param xpos_label a character string specifying the column containing the
#'   x-positions.
#' @param ypos_label a character string specifying the column containing the
#'   y-positions.
#' @param timestamps_label a character string specifying the column containing
#'   the timestamps.
#' @param dist_label a character string specifying the column containing the
#'   distance traveled.
#' @param vel_label a character string specifying the column containing the
#'   velocity.
#' @param acc_label a character string specifying the column containing the
#'   acceleration.
#' @param mt_id_label a character string specifying the column that provides a
#'   unique ID for every trial.
#' @param mt_seq_label a character string specifying the column that indicactes
#'   the order of the logged coordinates within a trial. If unspecified, the
#'   coordinates will be ordered according to their timestamps.
#' @param reset_timestamps logical indicating if the first timestamp should be
#'   subtracted from all timestamps within a trial. Default is TRUE as it is
#'   recommended for all following analyses in mousetrap.
#'
#' @return  A mousetrap data object (see \link{mt_example}).
#'
#' @seealso \link{mt_import_mousetrap} and \link{mt_import_wide} for importing
#' mouse-tracking data from other sources.
#'
#' @examples
#' \dontrun{
#' exp_data <- read.csv("exp_data.csv")
#' data <- mt_import_long(exp_data)
#' }
#' @export
mt_import_long <- function(raw_data,
  xpos_label="xpos", ypos_label="ypos",
  timestamps_label="timestamps",
  dist_label="dist", vel_label="vel", acc_label="acc",
  mt_id_label=mt_id, mt_seq_label="mt_seq",
  reset_timestamps=TRUE) {

  # Look for mt_seq variable (that indicates the order of the logs)
  if (is.null(mt_seq_label) | (mt_seq_label %in% colnames(raw_data) == FALSE)) {
    message(
      "No mt_seq variable found (that indicates the order of the logs). ",
      "Automatically created variable based on the timestamps."
    )
    mt_seq_label <- "mt_seq"
    raw_data[,"mt_seq"] <- 0

    # Sort dataset according to mt_id and timestamps
    raw_data <- raw_data[order(raw_data[,mt_id_label], raw_data[,timestamps_label]),]

    # Add mt_seq
    for (current_id in unique(raw_data[,mt_id_label])) {
      raw_data[raw_data[,mt_id_label] == current_id,mt_seq_label] <-
        1:sum(raw_data[,mt_id_label] == current_id)
    }
  }

  # Sort dataset according to mt_id and mt_seq
  raw_data <- raw_data[order(raw_data[,mt_id_label], raw_data[,mt_seq_label]),]

  # Rename and collect variables
  mt_labels <- c(
    timestamps=timestamps_label,
    xpos=xpos_label, ypos=ypos_label,
    dist=dist_label, vel=vel_label, acc=acc_label
  )
  mt_include <- c()
  timestamps <- mt_variable_labels[["timestamps"]]

  for (var in names(mt_labels)) {
    label <- mt_labels[var]
    if (label %in% colnames(raw_data)) {
      colnames(raw_data)[colnames(raw_data) == label] <- mt_variable_labels[[var]]
      mt_include <- c(mt_include, mt_variable_labels[[var]])
    }
  }

  # Create array for selected variables
  trajectories <- reshape2::melt(raw_data,
    measure.vars=mt_include, variable.name="mt_variable")
  custom.formula <- stats::as.formula(paste(
    mt_id, "mt_variable", mt_seq_label, sep="~"
  ))
  trajectories <- reshape2::acast(trajectories,
    custom.formula, value.var="value")

  # Remove dimnames for logs
  dimnames(trajectories)[[3]] <- NULL

  # Subtract first timestamp for each trial
  if (reset_timestamps) {
    trajectories[,timestamps,] <- trajectories[,timestamps,] - trajectories[,timestamps,1]
  }

  # Create data.frame from leftover variables
  raw_data <- raw_data[,!colnames(raw_data) %in% c(mt_include, mt_seq_label), drop=FALSE]
  raw_data <- unique(raw_data)
  # Rename mt_id column
  colnames(raw_data)[colnames(raw_data) == mt_id_label] <- mt_id
  # Issue warning if more than one line per mt_id remains
  if (max(table(raw_data[,mt_id])) > 1) {
    warning(
      "After removing trajectory data, ",
      "more than one unique row per mt_id remains."
    )
  }

  return(list("data"=raw_data, "trajectories"=trajectories))
}
