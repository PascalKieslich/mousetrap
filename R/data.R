#' Raw mouse-tracking dataset for demonstrations of the mousetrap package
#'
#' An exemplary mouse-tracking dataset collected in OpenSesame using the
#' mousetrap plug-ins. A preprocessed (as opposed to raw) version of the same
#' data can be found in \link{mt_example}.
#'
#' The data stem from a study based on experiment 1 by Dale et al. (2007). In
#' this experiment, participants have to assign exemplars (e.g., "shark") to one
#' of two categories (e.g., "fish" or "mammal") by clicking on the button
#' corresponding to the correct category. All exemplars and categories were
#' translated to and presented in German.
#'
#' Across the 19 trials of the experiment, participants categorized 13 exemplars
#' that were typical of their category and 6 atypical exemplars for which this
#' was not the case. For the atypical exemplars (e.g., "whale"), the competing
#' category ("fish") was selected to compete with the correct category
#' ("mammal"). The hypothesis under investigation is whether participants' mouse
#' trajectories deviate more towards the competing category for the atypical
#' exemplars, indicating increased conflict between the response options.
#'
#' Please note that \code{mt_example_raw} should only be used for exploring the
#' features of the mousetrap package and not for any substantive analysis.
#'
#' @references Mousetrap
#'
#'   Dale, R., Kehoe, C., & Spivey, M. J. (2007). Graded motor responses in the
#'   time course of categorizing atypical exemplars. \emph{Memory & Cognition,
#'   35}(1), 15-28.
#'
#' @format A \link{data.frame} with 38 rows and 19 variables. The data.frame is 
#'   based on the combined raw data that were created using 
#'   \link[readbulk]{read_opensesame} from the \code{readbulk} library. For ease
#'   of use, unnecessary columns were excluded.
#'
#'   The variables included relate to the item that was presented
#'   (\code{Exemplar}), the answer categories (\code{Category1} and
#'   \code{Category2}), the subject identifier (\code{subject_nr}) the subjects'
#'   response (\code{response_track_mouse}), as well as the mouse-tracking
#'   variables (\code{timestamps_track_mouse}, \code{xpos_track_mouse} and
#'   \code{ypos_track_mouse}). Besides, a number of additional variables are
#'   included, e.g., some variables relating to the general settings of the
#'   experiment (e.g., the \code{width} and \code{height} of the screen in
#'   pixels).
#'
#'   Each mouse-tracking variable contains a list of values (separated by ', ')
#'   - one entry for each recorded position of the mouse. The position
#'   coordinates are given in pixels, such that values of zero for both
#'   \code{xpos_track_mouse} and \code{ypos_track_mouse} indicate that the
#'   cursor is located in the center of the screen. Both variables increase in
#'   value as the mouse moves toward the bottom right. Timestamps are given in
#'   milliseconds.
#'
"mt_example_raw"

#' A mousetrap data object.
#'
#' A mousetrap data object with example data created by importing
#' \link{mt_example_raw} and applying basic post-processing.
#'
#' The raw data set was imported using \link{mt_import_mousetrap}. Trajectories
#' were then remapped using \link{mt_remap_symmetric} so that all trajectories
#' end in the top-left corner.
#'
#' @format A mousetrap data object is a \link{list} containing  at least the
#'   following objects:
#'   \itemize{
#'     \item{\code{data}: \link{data.frame} containing the trial data (from
#'     which the mouse-tracking data columns have been removed). More
#'     information about the content of the trial data in \code{mt_example} can
#'     be found in \link{mt_example_raw}.}
#'     \item{\code{trajectories}: \link{array} containing raw mouse-tracking
#'     trajectories. The first dimension represents the different trials. It
#'     uses the ID of the trial as names (this ID is by default logged under
#'     \link{mt_id} in \code{data}). The second dimension corresponds to the
#'     different variables (timestamps, x-positions, y-positions) with the names
#'     as specified in \link{mt_variable_labels} (by default: \code{timestamps},
#'     \code{xpos}, \code{ypos}). The third dimension corresponds to the samples
#'     taken over time, which are included in chronological order and carry
#'     successive integers as labels.}
#'   }
#'
#'   Some functions in this package (e.g., \link{mt_time_normalize} and
#'   \link{mt_average}) add additional trajectory arrays (e.g.,
#'   \code{tn_trajectories} and \code{av_trajectories}) to the mousetrap data
#'   object. Other functions modify the existing arrays (e.g.,
#'   \link{mt_calculate_derivatives} adds distance, velocity, and acceleration
#'   to an existing dataset). Finally \link{mt_calculate_measures} adds an
#'   additional data.frame with mouse-tracking measures to it.
#'
"mt_example"
