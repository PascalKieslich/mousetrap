#' Calculate sample entropy.
#'
#' Calculate sample entropy for each trajectory as a measure of the complexity
#' of movements along one specific dimension.
#' 
#' \code{mt_sample_entropy} calculates the sample entropy for each trajectory as
#' a measure of its complexity. Hehman et al (2015) provide details on how 
#' sample entropy can be calculated and applied in mouse-tracking (following 
#' Dale et al., 2007). They apply the sample entropy measure to the
#' differences between adjacent x-positions 
#' (which is also the default here, as in a standard mouse-tracking task with 
#' buttons located in the top-left and right corners mostly the movements in the
#' horizontal direction are of interest). Besides, they recommend using the 
#' time-normalized trajectories so all trajectories have the same length.
#' 
#' Sample entropy is computed by comparing windows of a fixed size (specified 
#' using \code{m}) across all recorded positions. Sample entropy is the 
#' negative natural logarithm of the conditional probability that this window 
#' remains similar across the trial (Hehman et al., 2015). A window is 
#' considered to be similar to another if their distance is smaller than a 
#' specified tolerance value (which can be specified using \code{r}). Hehman et 
#' al. (2015) use a tolerance value of 0.2 * standard deviation of all 
#' differences between adjacent x-positions in the dataset (which is the default
#' implemented here).

#'
#' @inheritParams mt_time_normalize
#' @param save_as a character string specifying where the calculated measures
#'   should be stored.
#' @param dimension a character string specifying the dimension based on which
#'   sample entropy should be calculated. By default (xpos), the x-positions are
#'   used.
#' @param m an integer passed on to the sample entropy function (see Details).
#' @param r a numeric value passed on to the sample entropy function (see
#'   Details).
#' @param use_diff logical indicating if the differences of the dimension values
#'   should be computed before calculating sample entropy (which is done by
#'   default, see Details).
#'   
#' @return A mousetrap data object (see \link{mt_example}).
#'   
#'   If a data.frame with label specified in \code{save_as} (by default
#'   "measures") already exists, the sample entropy values are added as
#'   additional column.
#'   
#'   If not, an additional \link{data.frame} will be added.
#'   
#'   If a trajectory array was provided directly as \code{data}, only the 
#'   data.frame will be returned.
#'   
#' @references Dale, R., Kehoe, C., & Spivey, M. J. (2007). Graded motor
#'   responses in the time course of categorizing atypical exemplars.
#'   \emph{Memory & Cognition, 35}(1), 15-28.
#' 
#' Hehman, E., Stolier, R. M., & Freeman, J. B. (2015). Advanced mouse-tracking
#' analytic techniques for enhancing psychological science. \emph{Group
#' Processes & Intergroup Relations, 18}(3), 384-401.
#' 
#' @seealso \link{mt_measures} for calculating other mouse-tracking
#' measures.
#' 
#' @examples
#' # Calculate sample entropy based on time-normalized
#' # trajectories and merge results with other meausres
#' # derived from raw trajectories
#' mt_example <- mt_measures(mt_example)
#' mt_example <- mt_time_normalize(mt_example,
#'   save_as="tn_trajectories", nsteps=101)
#' mt_example <- mt_sample_entropy(mt_example,
#'   use="tn_trajectories", save_as="measures",
#'   dimension="xpos", m=3)
#' 
#' @author
#' Pascal J. Kieslich (\email{pascal.kieslich@@gmail.com})
#' 
#' Dirk Wulff
#' 
#' Felix Henninger
#' 
#' @export
mt_sample_entropy <- function(data,
  use="tn_trajectories", save_as="measures",
  dimension="xpos", m=3, r=NULL,
  use_diff=TRUE,
  verbose=FALSE) {
  
  # Function to calculate sample entropy
  # based on Hehman et al. (2015)
  sample_entropy <- function(x, m, r) {
    
    if (length(x) >= (m + 2 + use_diff)) { # check if number of logs is sufficient
      
      if (use_diff) {
        dx <- diff(x)
      } else {
        dx <- x
      }
      
      # Note: original windowmaker Python function by Hehman et al.
      # excludes last 2 elements of vector
      # (see commented out code that implements exactly this below)
      # we think that they should be included
      # and have modified the function accordingly
      windowmaker <- function(xs, m) {
        sapply(
          # 1:(length(xs) - m - 1),
          1:(length(xs) - m + 1),
          function(i) { xs[i:(i+m-1)] }
        )
      }
      
      wm <- windowmaker(dx, m)
      wm <- wm[,-ncol(wm)]
      wm1 <- windowmaker(dx, m+1)
      
      Mm <- 0
      Mm1 <- 0
      
      for (i in 1:(ncol(wm) - 1)) {
        for (j in (i+1):ncol(wm)) {
          if(max(abs(wm[,i] - wm[,j])) <= r) {
            Mm <- Mm + 1
            if(max(abs(wm1[,i] - wm1[,j])) <= r) {
              Mm1 <- Mm1 + 1
            }
          }
        }
      }
      
      return(-log(Mm1 / Mm))
    } else {
      return(NA)
    }
  }
  
  # Prepare data
  trajectories <- extract_data(data=data, use=use)
  if (!(dimension %in% dimnames(trajectories)[[3]])) {
    stop(paste("Dimension", dimension, "not found in trajectory array."))
  }
  
  # Calculate number of logs
  nlogs <- mt_count(trajectories,dimensions=dimension)
  
  # Set r for sample entropy function
  # cf. Dale et al., 2007, p. 20
  if (is.null(r)) {
    if (length(nlogs) == 1) {
      
      if (use_diff){
        r <- .2 * stats::sd(diff(trajectories[,,dimension]), na.rm=TRUE)
      } else {
        r <- .2 * stats::sd(trajectories[,,dimension], na.rm=TRUE)
      }
      
      
    } else {
      
      if (use_diff){
        r <- .2 * stats::sd(diff(t(trajectories[,,dimension])), na.rm=TRUE)
      } else {
        r <- .2 * stats::sd(trajectories[,,dimension], na.rm=TRUE)
      }
      
    }
  }
  
  measures <- matrix(
    data=NA,
    nrow=nrow(trajectories),
    ncol=1,
    dimnames=list(
      row.names(trajectories),
      "sample_entropy"
    )
  )
  
  # Calculate measures
  for (i in 1:nrow(trajectories)){

    # Calculate sample entropy
    measures[i,"sample_entropy"] <- sample_entropy(
      x=trajectories[i, 1:nlogs[i], dimension],
      r=r, m=m
    )

    if (verbose) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }
  
  if (verbose) {
    message(paste("all", i, "trials finished"))
  }
  
  return(create_results(
    data=data, results=measures, use=use, save_as=save_as,
    ids=rownames(trajectories), overwrite=FALSE))
  
}
