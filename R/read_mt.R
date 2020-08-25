#' Read MouseTracker raw data.
#' 
#' \code{read_mt} reads raw data that was collected using 
#' \href{http://www.mousetracker.org/}{MouseTracker} (Freeman & Ambady, 2010) 
#' and stored as a file in the ".mt" format. If multiple files should be read 
#' into R, \code{read_mt} can be used in combination with the 
#' \link[readbulk]{read_bulk} function from the
#' \href{http://pascalkieslich.github.io/readbulk/}{readbulk} package (see 
#' Examples). After reading the data into R, \link{mt_import_wide} can be used 
#' to prepare the trajectory data for analyses using the mousetrap library. The 
#' current version of \code{read_mt} has been tested with data from MouseTracker
#' Version 2.84 - but please be sure to double-check.
#' 
#' @param file a character string specifying the filename of the .mt file.
#' @param columns either 'all' or a character vector specifying the to be 
#'   extracted variables. Defaults to 'all' in which case all existing variables
#'   will be extracted.
#' @param add_trialid boolean specifying whether an additional column containing
#'   the trial number should be added.
#' @param add_filename boolean specifying whether an additional column 
#'   containing the file name should be added.
#'   
#' @return A \link{data.frame} with one row per trial. Variables are ordered
#'   according to columns, x-coordinates, y-coordinates, and timestamps.
#'   
#' @references Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for 
#'   studying real-time mental processing using a computer mouse-tracking
#'   method. \emph{Behavior Research Methods, 42}(1), 226-241.
#'
#' @seealso
#' 
#' \link[readbulk]{read_bulk} from the \code{readbulk} package for reading and 
#' combining multiple raw data files.
#' 
#' \link{mt_import_wide} to prepare the imported data for analyses in mousetrap.
#' 
#' @examples
#' \dontrun{
#' # Read a single raw data file from MouseTracker
#' # (stored in the current working directory)
#' mt_data_raw <- read_mt("example.mt")
#'
#' # Use read_bulk to read all raw data files ending with ".mt" that are
#' # stored in the folder "raw_data" (in the current working directory)
#' library(readbulk)
#' mt_data_raw <- read_bulk("raw_data", fun=read_mt, extension=".mt")
#'
#' # Import the data into mousetrap
#' mt_data <- mt_import_wide(mt_data_raw)
#' }
#'
#'
#' @author
#' Dirk U. Wulff
#'
#' @export

read_mt <- function(file,
                   columns='all',
                   add_trialid=FALSE,
                   add_filename=FALSE
                   ){


  # Define block names of raw data and variable names
  block_names <- c('RAW TRACKS (X coordinates)',
                   'RAW TRACKS (Y coordinates)',
                   'RAW TRACKS (time)')
  variable_labels <- c('X', 'Y', 'T')

  # Read
  # read file
  con  <- base::file(file, 'rb')
  lins <- readLines(con)
  close(con)

  # Loop over blocks
  # One block per coordinate/variable (X, Y, & T)
  for (i in 1:length(block_names)) {

    # pull block name
    block      <- block_names[i]

    # find start line of block
    start      <- which(lins == block)

    # find end line of block (next empty line)
    end        <- start + which(lins[start:length(lins)] == '')[1] - 2

    # collect variable names (non-coordinate names)
    nams       <- strsplit(lins[start+1], ',')[[1]]

    # determine to be extracted variables
    if (length(columns) == 1) {
      if (columns == 'all') {
        vars <- nams
      } else {
        vars <- columns
      }
    } else {
      vars <- columns
    }

    # determine number of variables and number of trials
    n_nams <- length(nams)
    trials <- (start + 2) : end


    # ---- process trials
    # count trials
    ind <- 0

    # define containers for coordinates and qualifying trial variables
    positions <- list()
    # the front matrix contains the trial data, which are stored in the first
    # columns, whereas the actual coordinates are stored in the later columns
    if (i == 1) front <- matrix(NA, nrow=length(trials), ncol=length(vars))

    # iterate over trials
    for (trial in trials) {
      ind <- ind + 1

      # split trial (one row) into separate entries (and encode in utf8)
      lin <- strsplit(enc2utf8(lins[trial]), ',')[[1]]

      # collect only coordinates and store
      positions[[ind]] <- as.numeric(lin[-c(1:n_nams)])

      # store front
      if (i == 1) {
        front[ind,] <- lin[which(nams %in% vars)]
      }
    }

    if (i == 1) {
      colnames(front) <- vars
    }

    # ---- store trials

    # store in matrix

    # initialize indicator
    ind <- 0

    # identify valid trials (trials with tracking)
    valid_trials <- which(sapply(positions, length) > 0)

    # setup matrix to store coordinate/time data
    pos_matrix <- matrix(
      NA,
      nrow=length(valid_trials),
      ncol=max(sapply(positions, length))
    )
    colnames(pos_matrix) <- paste(
      variable_labels[i],
      1:ncol(pos_matrix),
      sep='_'
    )

    # iterate over valid trials and store coordinate/time info in matrix
    for (j in valid_trials) {
      ind <- ind + 1
      pos <- positions[[j]]
      pos_matrix[ind, 1:length(pos)] <- pos
    }

    # combine coordinates and trial variables
    if (i == 1) {
      data <- cbind(front[valid_trials,], pos_matrix)
    } else {
      data <- cbind(data, pos_matrix)
    }
  } # end of loop over blocks

  # Add file name and trial number if desired
  if (add_filename) {
    data <- cbind(File=file, data)
  }
  if (add_trialid) {
    data <- cbind(Trial=1:nrow(data), data)
  }

  # Convert to data.frame
  data <- data.frame(data, stringsAsFactors=FALSE)

  # Convert all variables to their appropriate data type
  data <- dplyr::mutate_all(data, .funs=utils::type.convert, as.is=TRUE)

  return(data)
}
