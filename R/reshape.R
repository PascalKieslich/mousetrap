#' General-purpose reshape and aggregation function for mousetrap data.
#' 
#' \code{mt_reshape} is the general function used in the \code{mousetrap} 
#' package for filtering, merging, reshaping, and aggregating mouse-tracking 
#' measures or trajectories in combination with other trial data. Several
#' additional (wrapper) functions for more specific purposes (cf. "See Also")
#' are available.
#' 
#' \code{mt_reshape} uses the \link{rownames} of \code{data[[use]]} and 
#' \code{data[[use2]]} for merging the trajectories / measures and the trial 
#' data. For convenience (and for trajectories in long format also of 
#' necessity), an additional column (labelled as specified in the \code{mt_id}
#' argument) is added to the reshaped data containing the rownames as trial
#' identifier.
#' 
#' The main purpose of this function is to reshape the trajectory data into a 
#' two-dimensional data.frame, as this format is required for many further
#' analyses and plots in R.
#' 
#' Besides, it should aid the user in combining data contained in different
#' parts of the mousetrap data object, e.g., a condition variable stored in 
#' \code{data[["data"]]} with trajectory data stored in 
#' \code{data[["trajectories"]]} (or mouse-tracking measures stored in 
#' \code{data[["measures"]]}).
#' 
#' Finally, it offers the possibility to aggregate trajectories and measures for
#' different conditions and/or subjects.
#' 
#' The package also includes several functions that wrap \code{mt_reshape} and
#' serve specific purposes. They are often easier to use, and thus recommended
#' over \code{mt_reshape} unless the utmost flexibility is required. These
#' functions are described in the section "See Also".
#' 
#' Note also that many merging, reshaping, and aggregation procedures can be 
#' performed directly by using some of the basic R functions, e.g., \link{merge}
#' and \link{aggregate}, or through the R packages \code{dplyr} or 
#' \code{reshape2}, if desired.
#' 
#' 
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which data should be reshaped. The 
#'   corresponding data are selected from data using \code{data[[use]]}. 
#'   Usually, this value corresponds to either "trajectories", 
#'   "tn_trajectories", or "measures", depending on whether the analysis 
#'   concerns raw trajectories, time-normalized trajectories, or derived 
#'   measures.
#' @param use_variables a character vector specifying which mouse-tracking 
#'   variables should be reshaped. Corresponds to the column names in case a 
#'   data.frame with mouse-tracking measures is provided. Corresponds to the 
#'   labels of the array dimensions in case a trajectory array is provided. If 
#'   unspecified, all variables will be reshaped.
#' @param use2 a character string specifying where the other trial data can be 
#'   found. Defaults to "data" as \code{data[["data"]]} usually contains all non
#'   mouse-tracking trial data. Alternatively, a data.frame can be provided 
#'   directly.
#' @param use2_variables a character string (or vector) specifying the variables
#'   (in \code{data[[use2]]}) that should be merged with the data. If 
#'   \code{aggregate==TRUE}, the trajectories / measures will be aggregated 
#'   separately for each of the levels of these variables using 
#'   \link[dplyr]{summarize_at}.
#' @param subset a logical expression (passed on to \link{subset}) indicating 
#'   elements or rows to keep. If specified, \code{data[[use2]]} will be 
#'   subsetted using this expression, and, afterwards, \code{data[[use]]} will 
#'   be filtered accordingly.
#' @param subject_id a character string specifying which column contains the 
#'   subject identifier in \code{data[[use2]]}. If specified and 
#'   \code{aggregate==TRUE}, aggregation will be performed within subjects 
#'   first.
#' @param aggregate logical indicating whether data should be aggregated. If 
#'   \code{use2_variables} are specified, aggregation will be performed 
#'   separately for each of the levels of the \code{use2_variables}.
#' @param aggregate_subjects_only logical indicating whether data should only be
#'   aggregated per subject (if \code{subject_id} is specified and 
#'   \code{aggregate==TRUE}).
#' @param .funs the aggregation function(s) passed on to 
#'   \link[dplyr]{summarize_at}. By default, the \link{mean} is calculated.
#' @param trajectories_long logical indicating if the reshaped trajectories 
#'   should be returned in long or wide format. If \code{TRUE}, every recorded 
#'   position in a trajectory is placed in another row (whereby the order of the
#'   positions is logged in the variable \code{mt_seq}). If \code{FALSE}, every 
#'   trajectory is saved in wide format and the respective positions are indexed
#'   by adding an integer to the corresponding label (e.g., \code{xpos_1}, 
#'   \code{xpos_2}, ...). Only relevant if \code{data[[use]]} contains 
#'   trajectories.
#' @param mt_id a character string specifying the name of the column that will 
#'   contain the trial identifier in the reshaped data. The values for the trial
#'   identifier correspond to the \code{rownames} of \code{data[[use]]} and 
#'   \code{data[[use2]]}.
#' @param mt_seq a character string specifying the name of the column that will 
#'   contain the integers indicating the order of the mouse positions per 
#'   trajectory in the reshaped data. Only relevant if \code{data[[use]]}
#'   contains trajectories and \code{trajectories_long==TRUE}.
#' @param aggregation_function Deprecated. Please use \code{.funs} instead.
#'   
#' @return A \link{data.frame} containing the reshaped data.
#' 
#' @seealso
#' \link{mt_aggregate} for aggregating mouse-tracking measures and trajectories.
#' 
#' \link{mt_aggregate_per_subject} for aggregating mouse-tracking measures and
#' trajectories per subject.
#'  
#' \link[dplyr]{inner_join} for merging data and \link[dplyr]{summarize_at} for
#' aggregating data using the \code{dplyr} package.
#' 
#' 
#' @examples
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'   
#' # Reshape time-normalized trajectories data into long format
#' # adding Condition variable
#' trajectories_long <- mt_reshape(mt_example, 
#'  use="tn_trajectories",
#'  use2_variables="Condition"
#'  )
#'                                 
#' # Reshape time-normalized trajectories data into wide format
#' # only keeping xpos and ypos
#' # and adding Condition variable
#' trajectories_wide <- mt_reshape(mt_example,
#'   use="tn_trajectories", use_variables = c("xpos","ypos"),
#'   use2_variables = "Condition",
#'   trajectories_long = FALSE
#'   )
#'   
#' @export
mt_reshape <- function(data,
  use="trajectories", use_variables=NULL,
  use2="data", use2_variables=NULL,
  subset=NULL, subject_id=NULL,
  aggregate=FALSE, aggregate_subjects_only=FALSE,
  .funs="mean",
  trajectories_long=TRUE,
  mt_id="mt_id", mt_seq="mt_seq",
  aggregation_function=NULL) {
  
  # Use substitute to allow that arguments in subset
  # can be specified like the arguments in the subset function
  subset <- substitute(subset)
  
  if(is.null(aggregation_function)==FALSE){
    warning("The argument aggregation_function is deprecated. ",
            "Please use .funs instead and consult the function documentation for its enhanced functionality.",
            call. = FALSE)
    .funs <- aggregation_function
  }
  
  # Assume trajectories are provided in case class is an array
  type_trajectories <- (class(data[[use]]) == "array")
  
  # Extract relevant data for trajectories
  if (type_trajectories) {
    
    # Extract trajectory data
    dataset <- extract_data(data=data,use=use)
    
    # Select all variables if use_variables is not specified
    if (is.null(use_variables)) {
      use_variables <- dimnames(dataset)[[2]]
    
    # Otherwise select relevant dimensions
    } else{
      dataset <- dataset[,use_variables,,drop=FALSE]
    }
    
    # Get dim variables
    dim_count <- dim(dataset)
    dim_names <- dimnames(dataset)
    
    # Reshape array into long format
    dataset <- aperm(dataset,c(3,1,2))
    dim(dataset) <- c(dim_count[1]*dim_count[3],dim_count[2])
    
    # Create data.frame adding mt_id and mt_seq columns
    dataset <- data.frame(
      rep(dim_names[[1]],each=dim_count[3]),  #mt_id
      rep(1:dim_count[3],times=dim_count[1]), #mt_seq
      dataset,
      stringsAsFactors=FALSE
    )
    colnames(dataset) <- c(mt_id,mt_seq,dim_names[[2]])
    
    # Remove columns that only contain NAs
    dataset <- dataset[rowSums(is.na(dataset[,use_variables,drop=FALSE]))<length(use_variables),,drop=FALSE]
    
    
  # Extract relevant data for measures
  } else {
    
    # Extract trajectory measures data
    dataset <- extract_data(data=data,use=use)
    
    # Add mt_id column based on the rownames
    dataset[,mt_id] <- rownames(dataset) 
    
    # Select all variables if use_variables is not specified
    if (is.null(use_variables)) {
      use_variables <- colnames(dataset)
      use_variables <- use_variables[use_variables!=mt_id]
    }
    dataset <- dataset[,c(mt_id,use_variables),drop=FALSE]
    
    # Set mt_seq to NULL as it is only required for trajectories
    # (simplifies grouping_variables specification later on)
    mt_seq <- NULL
    
  }
  
  
  # If subset or additional variables are specified, look at data[[use2]]
  if (is.null(use2_variables)==FALSE | is.null(subset) == FALSE | is.null(subject_id)==FALSE) {
    
    # Retrieve data for filtering and merging
    # and merge it with trajectory data
    if(class(use2) == "character") {
      data <- extract_data(data=data,use=use2)
    } else {
      data <- use2
    }
    
    # Add mt_id column to data based on rownames
    data[,mt_id] <- rownames(data)
    
    # Filter data (optional)
    if (is.null(subset) == FALSE) {
      data <- base::subset(data, subset=eval(subset))
    }
    
    # Select relevant columns
    data <- data[,c(mt_id,subject_id,use2_variables),drop=FALSE]
    
    # Merge data (== perform filtering for dataset)
    # check first if all IDs from data are included in the dataset
    # and if not, return a warning
    if (all(data[,mt_id] %in% dataset[,mt_id]) == FALSE) {
      warning(
        "For some trials in data[[use2]], ",
        "no corresponding trials in data[[use]] were found."
      )
    }
    
    # Merge datasets
    dataset <- dplyr::inner_join(data, dataset, by=mt_id)
    
  }
  
  
  # Perform (optional) aggregation
  if (aggregate) {
    
    # For trajectories, check number of obervations per trajectory
    if (type_trajectories) {
      if (length(table(table(dataset[,mt_id]))) > 1) {
        warning(
          "Trajectories differ in the number of logs. ",
          "Aggregate trajectory data may be incorrect."
        )
      }
    }
    
    # If subject variable is specified, always aggregate within subjects first
    if (is.null(subject_id) == FALSE) {
      
      grouping_variables <- c(subject_id, use2_variables, mt_seq)
      
      dataset <- dplyr::group_by_(dataset,.dots=grouping_variables)
      dataset <- dplyr::summarize_at(dataset,.funs=.funs, .cols=use_variables)
      
      if (aggregate_subjects_only == FALSE){
        if(length(.funs)>1){
          stop("More than one function was passed on to .funs. ",
               "This does not work if aggregation should be performed ",
               "both first within and then across subjects.")
        }
      }
      
    }
    
    
    # Aggregate trajectories per group (if this is desired)
    if (aggregate_subjects_only == FALSE | is.null(subject_id)){
      
      # Optionally group data
      grouping_variables <- c(use2_variables,mt_seq)
      if(is.null(grouping_variables)==FALSE){
        dataset <- dplyr::group_by_(dataset,.dots=grouping_variables)
      }
      
      # Perform aggregation
      dataset <- dplyr::summarize_at(dataset, .funs=.funs, .cols=use_variables)
      
    }
    
  }
  
  # Convert to wide format if specified
  if (trajectories_long==FALSE) {
    
    dataset <- tidyr::gather_(dataset, key_col="key", value_col="val", gather_cols=use_variables)
    dataset <- tidyr::unite_(dataset, col="key", from=c("key",mt_seq),sep="_")
    # convert to factor to insure correct column order
    dataset$key <- factor(dataset$key,levels=unique(dataset$key))
    dataset <- tidyr::spread_(dataset, key_col="key", value_col="val")
   
  }
  
  return(dataset)
  
}

#' Aggregate mouse-tracking data per condition.
#' 
#' \code{mt_aggregate} can be used for aggregating mouse-tracking measures (or
#' trajectories) per condition. One or several condition variables can be
#' specified using \code{use2_variables}. Aggregation will be performed
#' separately for each level of the condition variables. \code{mt_aggregate} is
#' a wrapper function for \link{mt_reshape}.
#' 
#' @inheritParams mt_reshape
#' @param use a character string specifying which data should be reshaped. The 
#'   corresponding data are selected from data using \code{data[[use]]}. 
#'   Usually, this value corresponds to either "tn_trajectories" or "measures", 
#'   depending on whether the time-normalized trajectories or derived measures 
#'   should be aggregated.
#' @param use_variables a character vector specifying which mouse-tracking 
#'   variables should be aggregated. Corresponds to the column names in case a 
#'   data.frame with mouse-tracking measures is provided. Corresponds to the 
#'   labels of the array dimensions in case a trajectory array is provided. If 
#'   unspecified, all variables will be aggregated.
#' @param use2 a character string specifying where the data containing the 
#'   condition information can be found. Defaults to "data" as 
#'   \code{data[["data"]]} usually contains all non mouse-tracking trial data. 
#'   Alternatively, a data.frame can be provided directly.
#' @param use2_variables a character string (or vector) specifying the variables
#'   (in \code{data[[use2]]}) across which the trajectories / measures will be 
#'   aggregated. For each combination of levels of the grouping variable(s), 
#'   aggregation will be performed separately using \link[dplyr]{summarize_at}.
#' @param subject_id a character string specifying which column contains the 
#'   subject identifier. If specified, aggregation will be performed within 
#'   subjects first (that is, within subjects for all available values of the 
#'   grouping variables specified in \code{use2_variables}).
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
#' several condition variables can be specified using \code{use2_variables}. 
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
