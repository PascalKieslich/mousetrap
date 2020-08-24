#' Calculate bimodality coefficient.
#' 
#' Calculate the bimodality coefficient for a numeric vector as specified in
#' Pfister et al. (2013).
#' 
#' The calculation of the bimodality coefficient involves calculating the
#' skewness and kurtosis of the distribution first. For this, the
#' \link[psych]{skew} and \link[psych:skew]{kurtosi} functions of the \code{psych}
#' package are used.
#' Note that type is set to "2" for these functions in accordance with 
#' Pfister et al. (2013).
#' 
#' @param x a numeric vector.
#' @param na.rm logical specifying whether missing values should be removed.
#' 
#' @return A numeric value.
#' 
#' @references
#' Pfister, R., Schwarz, K. A., Janczyk, M., Dale, R., & Freeman, J. B. (2013).
#' Good things peak in pairs: A note on the bimodality coefficient.
#' \emph{Frontiers in Psychology, 4}, 700.
#' \url{http://dx.doi.org/10.3389/fpsyg.2013.00700}
#' 
#' @seealso \link[psych]{skew} for calculating skewness and kurtosis.
#'
#'   \link{mt_check_bimodality} for assessing bimodality using several methods in
#'   a mousetrap data object.
#' 
#' @examples
#' pfister_data_a <- rep(1:11, times=c(3,5,5,10,17,20,17,10,5,5,3))
#' bimodality_coefficient(pfister_data_a) #.34
#' pfister_data_b <- rep(1:11, times=c(2,26,14,6,2,0,2,6,14,26,2))
#' bimodality_coefficient(pfister_data_b) #.79
#' 
#' @author
#' Pascal J. Kieslich (\email{pascal.kieslich@@gmail.com})
#' 
#' Felix Henninger
#' 
#' @export
bimodality_coefficient <- function(x, na.rm=FALSE) {
  
  # Remove missing values, if desired
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  
  n <- length(x)
  if (n == 0) {
    # The coefficient is not defined for
    # empty data
    return(NaN)  
  } else {
        
    m3 <- psych::skew(x, type=2)
    m4 <- psych::kurtosi(x, type=2)
    
    # Calculate the coefficient based on
    # the above
    bc <- (m3^2 + 1) /
      (m4 + 3 * ((n - 1)^2 / ((n - 2) * (n - 3))))
    
    return(bc)
  }
  
}


#' Assess bimodality of mouse-tracking measure distributions.
#' 
#' Assess bimodality of the distribution of mouse-tracking measures using the 
#' bimodality coefficient and Hartigan's dip statistic (see Details). If 
#' bimodality should be assessed separately for different conditions, the 
#' corresponding variables can be specified under \code{grouping_variables}.
#' 
#' Different methods have been suggested for assessing the bimodality of 
#' mouse-tracking measure distributions, each of which has advantages and
#' disadvantages (see Freeman & Dale, 2013).
#' 
#' Hehman et al. (2015) focus on two specific methods (bimodality coefficient 
#' and Hartigan's dip statistic) which are implemented here.
#' 
#' If \code{methods} include \code{BC}, the bimodality coefficient is calculated
#' using the \link{bimodality_coefficient} function in this package. According 
#' to Freeman and Ambady (2010), a distribution is considered bimodal if 
#' \code{BC > 0.555}.
#' 
#' Note that MouseTracker (Freeman & Ambady, 2010) standardizes variables within
#' each subject before computing the BC. This is also possible here using
#' \link{mt_standardize} (see Examples).
#' 
#' If \code{methods} include \code{HDS}, Hartigan's dip statistic is calculated 
#' using the \link[diptest]{dip.test} function of the \code{diptest} package. 
#' The corresponding p value (computed via linear interpolation) is returned.
#' 
#' If \code{methods} include \code{HDS_sim}, Hartigan's dip statistic is 
#' calculated using the \link[diptest]{dip.test} function with the additional 
#' argument \code{simulate.p.values=TRUE}. In this case, the p value is computed
#' from a Monte Carlo simulation of a uniform distribution with B (default: 
#' 2000) replicates.
#' 
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which data should be used. By
#'   default, points to the \code{measures} data.frame created using
#'   \link{mt_measures}.
#' @param use_variables a vector specifying for which mouse-tracking measures
#'   bimodality should be assessed.
#' @param methods a character string (or vector) specifying which methods should
#'   be used for assessing bimodality (see Details).
#' @param B an integer specifying the number of replicates used in the Monte
#'   Carlo test (only relevant if "HDS_sim" is included in methods, see
#'   Details).
#' @param grouping_variables a character string (or vector) specifying one or
#'   more variables in \code{data[["data"]]}. If specified, bimodality will be
#'   assessed separately for each level of the variable. If unspecified (the
#'   default), bimodality is checked across all trials.
#' @param ... additional arguments passed on to \link{mt_reshape} (such as
#'   \code{subset}).
#' 
#' @return A list of several data.frames. Each data.frame contains the value
#'   returned by the respective method for assessing bimodality (see Details) - 
#'   separately per condition (specified in the row dimension) and measure
#'   (specified in the column dimension).
#' 
#' @references
#' Freeman, J. B., & Ambady, N. (2010). MouseTracker: Software for studying
#' real-time mental processing using a computer mouse-tracking method.
#' \emph{Behavior Research Methods, 42}(1), 226-241.
#' 
#' Freeman, J. B., & Dale, R. (2013).
#' Assessing bimodality to detect the presence of a dual cognitive process.
#' \emph{Behavior Research Methods, 45}(1), 83-97.
#' 
#' Hehman, E., Stolier, R. M., & Freeman, J. B. (2015). Advanced mouse-tracking
#' analytic techniques for enhancing psychological science. \emph{Group
#' Processes & Intergroup Relations, 18}(3), 384-401.
#' 
#' @seealso
#'   \link{bimodality_coefficient} for more information about the bimodality
#'   coefficient.
#'
#'   \link[diptest]{dip.test} for more information about Hartigan's dip test.
#' 
#' @examples
#' # Calculate measures
#' mt_example <- mt_measures(mt_example)
#' 
#' # Assess bimodality for untransformed variables
#' mt_check_bimodality(mt_example,
#'   use_variables=c("MAD", "AD"))
#' 
#' # Standardize variables per participant
#' mt_example <- mt_standardize(mt_example,
#'   use_variables=c("MAD", "AD"), within="subject_nr")
#'   
#' # Assess bimodality for standardized variables
#' mt_check_bimodality(mt_example,
#'   use_variables=c("z_MAD", "z_AD"))
#' 
#' # Assess bimodality with simulated p values for HDS
#' mt_check_bimodality(mt_example,
#'   use_variables=c("z_MAD", "z_AD"),
#'   methods=c("BC", "HDS_sim"))
#'  
#' # Assess bimodality per condition
#' mt_check_bimodality(mt_example,
#'   use_variables=c("z_MAD", "z_AD"),
#'   grouping_variables="Condition")
#'                     
#' @author
#' Pascal J. Kieslich (\email{pascal.kieslich@@gmail.com})
#' 
#' Felix Henninger
#' 
#' @export
mt_check_bimodality <- function(data,
  use="measures", use_variables=NULL,
  methods=c("BC", "HDS"), B=2000,
  grouping_variables=NULL, ...) {
    
  results <- list()
  
  for (method in methods) {
    
    if (method == "BC") {
      # Bimodality coefficient
      method_label <- "BC"
      aggregation_function <- bimodality_coefficient
    
    } else if (method == "HDS") {
      # Hartigan's dip statistic
      # with p-value computed via linear interpolation
      method_label <- "HDS_p_value"
      aggregation_function <- function(x) {
        return(diptest::dip.test(x)$p)
      }
      
    } else if (method == "HDS_sim") {
      # Hartigan's dip statistic
      # with p-value computed from a Monte Carlo simulation of a uniform
      # distribution
      method_label <- "HDS_simulated_p_value"
      
      aggregation_function <- function(x) {
        return(
          diptest::dip.test(x, simulate.p.value=TRUE, B=B)$p
        )
      }
      
    } else {
      stop("Argument for methods may only contain BC, HDS, or HDS_sim")
    }
    
    results[[method_label]] <- mt_reshape(
      data=data,
      use=use, use_variables=use_variables,
      use2_variables=grouping_variables,
      aggregate=TRUE,
      .funs=aggregation_function,
      ...
    )
  }
    
  return(results)
}
