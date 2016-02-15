#' Calculate sample entropy.
#'
#' Calculate sample entropy for each trajectory.
#' 
#' \code{mt_sample_entropy} calculates the sample entropy for each trajectory as
#' a measure of its complexity. Hehman et al (2015) provide details on how 
#' sample entropy can be calculated and applied in mouse-tracking (following 
#' Dale et al., 2007). They apply the sample entropy measure to the x-positions 
#' (which is also the default here, as in a standard mouse-tracking task with 
#' buttons located in the top-left and right corners mostly the movements in the
#' horizontal direction are of interest). Besides, they recommend using the 
#' time-normalized trajectories so all trajectories have the same length.
#' 
#' Sample entropy is computed by comparing windows of a fixed size (specified 
#' using \code{lag}) across all recorded positions. Sample entropy is the 
#' negative natural logarithm of the conditional probability that this windows 
#' remains similar across the trial (Hehman et al., 2015). A window is 
#' considered to be similar to another if their distance is smaller than a 
#' specified tolerance value (which can be specified using \code{r}). Hehman et 
#' al. (2015) use a tolerance value of 0.2 * standard deviation of all 
#' differences between adjacent x-positions in the dataset (which is the default
#' implemented here).
#' 
#' The specific formula for sample entropy depends on the function specified in 
#' \code{method}. Per default ("pracma"), the \link[pracma]{sample_entropy} 
#' function from the pracma package is used. Alternatively ("hehman") the 
#' function by Hehman et al. (2015) is used. Finally, it is also possible to 
#' calculate sample_entropy values using both functions ("both").
#'
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (see \link{mt_example} for details).
#' @param use a character string specifying which trajectory data should be
#'   used.
#' @param save_as a character string specifying where the calculated measures
#'   should be stored.
#' @param method a character string specifying the method used for calculating
#'   sample entropy (see Details).
#' @param dimension a character string specifying the dimension based on which
#'   sample entropy should be calculated. By default (xpos), the x-positions are
#'   used.
#' @param lag an integer passed on to the sample entropy function (see Details).
#' @param r a numeric value passed on to the sample entropy function (see
#'   Details).
#' @param show_progress logical indicating whether function should report on the
#'   progress.
#'   
#' @return A mousetrap data object (see \link{mt_example}).
#'   
#'   If a data.frame with label specified in \code{save_as} (by default
#'   "measures") already exists, the sample entropy values are added as
#'   additional column(s) (by merging them using the \link{mt_id} variable).
#'   
#'   If not, an additional \link{data.frame} will be added.
#'   
#' @references Mousetrap
#' 
#' Dale, R., Kehoe, C., & Spivey, M. J. (2007). Graded motor responses in the
#' time course of categorizing atypical exemplars. \emph{Memory & Cognition,
#' 35}(1), 15-28.
#' 
#' Hehman, E., Stolier, R. M., & Freeman, J. B. (2015). Advanced mouse-tracking
#' analytic techniques for enhancing psychological science. \emph{Group
#' Processes & Intergroup Relations, 18}(3), 384-401.
#' 
#' @seealso \link{mt_calculate_measures} for calculating other mouse-tracking
#' measures.
#' 
#' @examples
#' # Calculate sample entropy based on time-normalized
#' # trajectories and merge results with other meausres
#' # derived from raw trajectories
#' mt_example <- mt_calculate_measures(mt_example)
#' mt_example <- mt_time_normalize(mt_example,
#'   save_as="tn_trajectories", nsteps=101)
#' mt_example <- mt_sample_entropy(mt_example,
#'   use="tn_trajectories", save_as="measures",
#'   method="pracma", dimension="xpos", lag=3)
#' 
#' @export
mt_sample_entropy <- function(data,
  use="tn_trajectories", save_as="measures",
  method="pracma", dimension="xpos", lag=3, r=NULL,
  show_progress=TRUE) {
  
  # Function to calculate sample entropy
  # based on Hehman et al. (2015)
  sample_entropy_hehman <- function(x, m, r) {
    
    if (length(x) >= m + 3) { # check if number of logs is sufficient
      dx = diff(x)
      
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
  if (!(dimension %in% colnames(data$trajectories))) {
    stop(paste("Dimension", dimension, "not found in trajectory array."))
  }
  
  # Calculate number of logs
  nlogs <- rowSums(!is.na(trajectories[,dimension,,drop=FALSE]))
  
  # Set r for sample entropy function
  # cf. Dale et al., 2007, p. 20
  if (is.null(r)) {
    if (length(nlogs) == 1) {
      r <- .2 * sd(diff(trajectories[,dimension,]), na.rm=TRUE)
    } else {
      r <- .2 * sd(diff(t(trajectories[,dimension,])), na.rm=TRUE)
    }
  }
    
  if (method %in% c("pracma", "hehman")) {
    mt_measures <- c("sample_entropy")
  } else if (method == "both") {
    mt_measures <- c("sample_entropy_pracma", "sample_entropy_hehman")
  } else {
    stop("method can only have the values pracma, hehman, or both.")
  }
  
  measures <- matrix(
    data=NA,
    nrow=nrow(trajectories),
    ncol=length(mt_measures),
    dimnames=list(
      row.names(trajectories),
      mt_measures
    )
  )
  
  # Calculate measures
  for (i in 1:nrow(trajectories)){
    
    current_values <- trajectories[i, dimension, 1:nlogs[i]]

    # Calculate sample entropy
    
    # ... using pracma function
    if (method %in% c("pracma", "both")) {
      
      if (length(current_values) >= 2 + lag * 4) { # check if number of logs is sufficient
        se_pracma <- pracma::sample_entropy(
          diff(current_values),
          r=r, tau=lag
        )
      } else {
        se_pracma <- NA
      }
      
    }
    
    # ... using function proposed by Hehman et al (2015)
    if (method %in% c("hehman", "both")) {
      se_hehman <- sample_entropy_hehman(
        x=current_values,
        r=r, m=lag
      )
    }
    
    if (method == "pracma") {
      measures[i,"sample_entropy"] <- se_pracma
    } else if (method == "hehman") {
      measures[i,"sample_entropy"] <- se_hehman
    } else {
      measures[i,"sample_entropy_pracma"] <- se_pracma
      measures[i,"sample_entropy_hehman"] <- se_hehman
    }

    if (show_progress) {
      if (i %% 100 == 0) message(paste(i, "trials finished"))
    }
  }
  
  if (show_progress) {
    message(paste("all", i, "trials finished"))
  }
  
  results <- data.frame(row.names(trajectories))
  colnames(results) <- mt_id
  rownames(results) <- results[,mt_id]
  results <- cbind(results,data.frame(measures))
  
  if (save_as %in% names(data)) {
    data[[save_as]] <- merge(data[[save_as]], results, by=mt_id)
  } else {
    data[[save_as]] <- results
  }
  
  return(data)
}
