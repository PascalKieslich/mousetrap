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
#' and \link{aggregate}, or through the R packages \code{reshape2} or 
#' \code{dplyr}, if desired.
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
#'   \link[reshape2]{dcast}.
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
#' @param aggregation_function the aggregation function passed on to 
#'   \link[reshape2]{dcast}. By default, the \link{mean} is calculated.
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
#'   
#' @return A \link{data.frame} containing the reshaped data.
#' 
#' @seealso
#' \link{mt_aggregate} for aggregating mouse-tracking measures and trajectories.
#' 
#' \link{mt_aggregate_per_subject} for aggregating mouse-tracking measures and
#' trajectories per subject.
#' 
#' \link{merge} for merging data in R.
#' 
#' \link{aggregate} for aggregating data in R.
#' 
#' \link[reshape2]{dcast} for reshaping and aggregating data using the
#' \code{reshape2} package.
#' 
#' Regarding the dangers of using \code{mt_reshape}, please consider the
#' following limerick before applying the function:
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
  aggregation_function=mean,
  trajectories_long=TRUE,
  mt_id="mt_id", mt_seq="mt_seq") {
  
  # Use substitute to allow that arguments in subset
  # can be specified like the arguments in the subset function
  subset <- substitute(subset)
  
  # Collect variables
  selected_variables <- c(mt_id)
  if (is.null(subject_id) == FALSE) {
    selected_variables <- c(selected_variables, subject_id)
  }
  if (is.null(use2_variables) == FALSE) {
    selected_variables <- c(selected_variables, use2_variables)
  }
  
  # Assume trajectories are provided in case class is an array
  type_trajectories <- (class(data[[use]]) == "array")
  
  # Extract relevant data for trajectories
  if (type_trajectories) {
    # Extract trajectory data
    dataset <- extract_data(data=data,use=use)
    
    # Melt trajectory data
    dataset <- reshape2::melt(dataset, 
      varnames=c(mt_id, "mt_variable", mt_seq),
      na.rm=TRUE
    )
    
    # Select variables
    if (!is.null(use_variables)) {
      dataset <- dataset[dataset[,"mt_variable"] %in% use_variables,]
    }
    
    use_variables <- as.character(unique(dataset[,"mt_variable"]))
    
  # Extract relevant data for measures
  } else {
    
    # Extract trajectory measures data
    dataset <- extract_data(data=data,use=use)
    
    # Add mt_id column based on the rownames
    dataset[,mt_id] <- rownames(dataset) 
    
    # Select variables
    if (!is.null(use_variables)){
      dataset <- dataset[,c(mt_id,use_variables)]
    }
    
    # Melt measures data
    dataset <- reshape2::melt(dataset,
      id.vars=mt_id,
      variable.name="mt_variable",
      na.rm=TRUE
    )
    
    # Set mt_seq to NULL as it is only required for trajectories
    # (simplifies formula specification later on)
    mt_seq <- NULL
    
  }
  
  
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
  data <- data[,selected_variables,drop=FALSE]
  
  # Merge data (== perform filtering for dataset)
  # check first if all IDs from data are included in the dataset
  # and if not, return a warning
  if (all(data[,mt_id] %in% dataset[,mt_id]) == FALSE) {
    warning(
      "For some trials in the filtered dataset in data[[use2]], ",
      "no corresponding trials in data[[use]] were found."
    )
  }
  dataset <- merge(data, dataset, by=mt_id, all=FALSE)
  
  
  # Case: no aggregation desired
  if (aggregate == FALSE) {
    
    # If no data variables should be included
    # simply return formatted trajectory data
    if (length(selected_variables) == 1) {
      custom_formula <- paste(c(mt_id, mt_seq), collapse="+")  
      
    # If variables are specified
    # include variables when reshaping the data
    } else {  
      custom_formula <- paste(c(mt_id, mt_seq, selected_variables[-1]), collapse="+")
    }
    
    custom_formula <- stats::as.formula(paste(custom_formula, "mt_variable", sep="~"))
    dataset <- reshape2::dcast(
      dataset,
      custom_formula,
      fun.aggregate=aggregation_function,
      value.var="value"
    )
    
    
  # Case: Aggregation desired  
  } else {
    
    if (type_trajectories) {
      # Check number of obervations per trajectory
      if (length(table(table(dataset$mt_id))) > 1) {
        warning(
          "Trajectories differ in the number of logs. ",
          "Aggregate trajectory data may be incorrect."
        )
      }
    }
    
    # If subject variable is specified, always aggregate within subjects first
    if (is.null(subject_id) == FALSE) {
      
      # Adjust aggregation procedure depending on whether aggregation
      # should only be performed within subjects
      if (aggregate_subjects_only) {
        custom_formula <- paste(c(subject_id, use2_variables, mt_seq), collapse="+")
        custom_formula <- stats::as.formula(paste(custom_formula, "mt_variable", sep="~"))
        dataset <- reshape2::dcast(dataset, 
          custom_formula,
          fun.aggregate=aggregation_function,
          value.var="value"
        )
      
      # ... or also across subjects afterwards
      } else {
        custom_formula <- paste(c(subject_id, use2_variables, mt_seq, "mt_variable"), collapse="+")
        custom_formula <- stats::as.formula(paste(custom_formula, ".", sep="~"))
        dataset <- reshape2::dcast(dataset,
          custom_formula,
          fun.aggregate=aggregation_function,
          value.var="value"
        )
        colnames(dataset)[ncol(dataset)] <- "value"  
      }
      
    }
    
    
    # Aggregate trajectories per group (if this is desired)
    if (aggregate_subjects_only == FALSE | is.null(subject_id)){
      
      # Case aggregation should be performed across all trials
      if (is.null(use2_variables)){
        if (type_trajectories){
          custom_formula <- mt_seq  
        } else {
          custom_formula <- "."
        }
      
      # Case aggregation should be performed separately for each level of use_variables2
      } else {
        custom_formula <- paste(c(use2_variables,mt_seq),collapse="+")
      }
      
      # Perform aggregation
      custom_formula <- stats::as.formula(paste(custom_formula, "mt_variable", sep="~"))
      dataset <- reshape2::dcast(dataset,
        custom_formula,
        fun.aggregate=aggregation_function,
        value.var="value"
      )  
      
      # Remove empty first column in case formula ".~mt_variable" was used
      if (is.null(use2_variables) & type_trajectories == FALSE) {
        dataset <- dataset[,-1]
      }
    }
    
  }
  
  if (trajectories_long) {
    dataset[is.na(dataset)] <- NA
    return(dataset)
  } else {
    dataset <- reshape2::melt(dataset,
      measure.vars=use_variables,
      variable.name = "mt_variable"
    )
    custom_formula_rows <- paste(colnames(dataset)[!colnames(dataset) %in% c("mt_variable", "value", mt_seq)], collapse="+")
    custom_formula_cols <- paste(c("mt_variable", mt_seq), collapse="+")
    custom_formula <- stats::as.formula(paste(custom_formula_rows, custom_formula_cols, sep="~"))
    dataset <- reshape2::dcast(dataset,
      custom_formula,
      fun.aggregate=aggregation_function,
      value.var="value"
    )  
    dataset[is.na(dataset)] <- NA
    return(dataset)
  }
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
#'   aggregation will be performed separately using \link[reshape2]{dcast}.
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
#' \link{aggregate} for aggregating data in R.
#' 
#' \link[reshape2]{dcast} for reshaping and aggregating data using the
#' \code{reshape2} package.
#' 
#' @examples
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'    
#' # Aggregate time-normalized trajectories per condition
#'   average_trajectories <-  mt_aggregate(mt_example,
#'     use="tn_trajectories",
#'     use2_variables="Condition"
#'   )
#' 
#' 
#' # Calculate mouse-tracking measures
#' mt_example <- mt_measures(mt_example)
#' 
#' # Aggregate measures per condition
#' average_measures <- mt_aggregate(mt_example,
#'     use="measures", use_variables=c("MAD", "AD"),
#'     use2_variables="Condition"
#'   )
#'       
#' # Aggregate measures per condition
#' # first within subjects and then across subjects
#' average_measures <- mt_aggregate(mt_example,
#'     use="measures", use_variables=c("MAD", "AD"),
#'     use2_variables="Condition",
#'     subject_id="subject_nr"
#'   )
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
#' \link{aggregate} for aggregating data in R.
#' 
#' \link[reshape2]{dcast} for reshaping and aggregating data using the
#' \code{reshape2} package.
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
