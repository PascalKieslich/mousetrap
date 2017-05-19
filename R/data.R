#' Raw mouse-tracking dataset for demonstrations of the mousetrap package
#'
#' An exemplary mouse-tracking dataset collected 
#' \href{http://osdoc.cogsci.nl/}{OpenSesame} using the 
#' \href{https://github.com/pascalkieslich/mousetrap-os}{mousetrap plugin} 
#' (Kieslich & Henninger, in press). A preprocessed (as opposed to raw) version of
#' the same data can be found in \link{mt_example}.
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
#'@references Dale, R., Kehoe, C., & Spivey, M. J. (2007). Graded motor 
#'  responses in the time course of categorizing atypical exemplars. 
#'  \emph{Memory & Cognition, 35}(1), 15-28.
#'  
#'  Kieslich, P. J., & Henninger, F. (in press). Mousetrap: An integrated,
#'  open-source mouse-tracking package. \emph{Behavior Research Methods}.
#'  doi:10.3758/s13428-017-0900-z
#'   
#' @format A \link{data.frame} with 38 rows and 19 variables. The data.frame is 
#'   based on the combined raw data that were created using 
#'   \link[readbulk]{read_opensesame} from the 
#'   \href{http://pascalkieslich.github.io/readbulk/}{readbulk} library. For 
#'   ease of use, unnecessary columns were excluded.
#'
#'   The variables included relate to the item that was presented
#'   (\code{Exemplar}), the answer categories (\code{Category1} and
#'   \code{Category2}), the subject identifier (\code{subject_nr}) the subjects'
#'   response (\code{response_get_response}), as well as the mouse-tracking
#'   variables (\code{timestamps_get_response}, \code{xpos_get_response} and
#'   \code{ypos_get_response}). Besides, a number of additional variables are
#'   included, e.g., some variables relating to the general settings of the
#'   experiment (e.g., the \code{width} and \code{height} of the screen in
#'   pixels).
#'
#'   Each mouse-tracking variable contains a list of values (separated by ', ')
#'   - one entry for each recorded position of the mouse. The position
#'   coordinates are given in pixels, such that values of zero for both
#'   \code{xpos_get_response} and \code{ypos_get_response} indicate that the
#'   cursor is located in the center of the screen. Both variables increase in
#'   value as the mouse moves toward the bottom right. Timestamps are given in
#'   milliseconds.
#'
"mt_example_raw"

#' A mousetrap data object.
#'
#' A data object of class "mousetrap" with example data created by importing 
#' \link{mt_example_raw} and applying basic post-processing.
#'
#' The raw data set was imported using \link{mt_import_mousetrap}. Trajectories
#' were then remapped using \link{mt_remap_symmetric} so that all trajectories
#' end in the top-left corner and their starting point was aligned using
#' \link{mt_align_start} to a common value (0,0).
#'
#' @format A mousetrap data object is a \link{list} containing  at least the
#'   following objects:
#'   \itemize{
#'     \item{\code{data}: a \link{data.frame} containing the trial data (from 
#'     which the mouse-tracking data columns have been removed). More 
#'     information about the content of the trial data in \code{mt_example} can 
#'     be found in \link{mt_example_raw}. The \link{rownames} of \code{data} 
#'     correspond to the trial identifier. For convenience, the trial identifier
#'     is also stored in an additional column called "mt_id".} 
#'     \item{\code{trajectories}: an \link{array} containing the raw 
#'     mouse-tracking trajectories. The first dimension represents the different
#'     trials and the dimension names (which can be accessed using 
#'     \link{rownames}) correspond to the trial identifier (the same identifier 
#'     that is used as the \code{rownames} in \code{data}). The second dimension
#'     corresponds to the samples taken over time which are included in
#'     chronological order. The third dimension corresponds to the different
#'     mouse-tracking variables (timestamps, x-positions, y-positions) which are
#'     usually called \code{timestamps}, \code{xpos}, and \code{ypos}. }
#'   }
#'
#'   Some functions in this package (e.g., \link{mt_time_normalize} and
#'   \link{mt_average}) add additional trajectory arrays (e.g.,
#'   \code{tn_trajectories} and \code{av_trajectories}) to the mousetrap data
#'   object. Other functions modify the existing arrays (e.g.,
#'   \link{mt_derivatives} adds distance, velocity, and acceleration
#'   to an existing dataset). Finally \link{mt_measures} adds an
#'   additional data.frame with mouse-tracking measures to it.
#'
"mt_example"

#' Mouse trajectory prototypes.
#'
#' A core set of five mouse trajectory prototypes including the 'straight'
#' trajectory, the mildly curved trajectory, the continuous change-of-mind
#' trajectory, the discrete change-of-mind trajectory, and the double discrete
#' change-of-mind trajectory.
#' 
#' Mouse- and hand-trajectories often occur in types (Wulff, Haslbeck, & 
#' Schulte-Mecklenbeck, 2017). In such cases, movement trajectory data should be
#' analyzed in terms of discrete type assignments. To this end \link{mt_map} can
#' be used to map mouse- or hand-trajectory to the closest of several predefined
#' prototypes. \code{mt_prototypes} provides a core set of prototypes that has 
#' been shown to represent well a large fraction of empirical movement 
#' trajectories.
#' 
#' To tailor the set of prototypes to a given study, \code{mt_prototypes} can be
#' extended using \link{mt_add_trajectory}.
#' 
#' @references Wulff, D. U., Haslbeck, J. M. B., Schulte-Mecklenbeck, M. (2017).
#'   Measuring the (dis-)continuous mind. Manuscript in preparation.
#'
"mt_prototypes"
NULL

