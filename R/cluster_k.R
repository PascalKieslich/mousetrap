#' Estimate optimal number of clusters.
#'
#' Estimates the optimal number of clusters (\code{k}) using various methods.
#'
#' \code{mt_cluster_k} estimates the number of clusters (\code{k}) using four
#' commonly used k-selection methods (specified via \code{compute}): cluster
#' stability (\code{stability}), the gap statistic (\code{gap}), the jump
#' statistic (\code{jump}), and the slope statistic (\code{slope}).
#'
#' Cluster stability methods select \code{k} as the number of clusters for which
#' the assignment of objects to clusters is most stable across bootstrap
#' samples. This function implements the model-based and model-free methods
#' described by Haslbeck & Wulff (2016). See references.
#'
#' The remaining three methods select \code{k} as the value that optimizes the
#' gap statistic (Tibshirani, Walther, & Hastie, 2001), the jump statistic
#' (Sugar & James, 2013), and the slope statistic (Fujita, Takahashi, &
#' Patriota, 2014), respectively.
#'
#' For clustering trajectories, it is often useful that the endpoints of all
#' trajectories share the same direction, e.g., that all trajectories end in the
#' top-left corner of the coordinate system (\link{mt_remap_symmetric} or
#' \link{mt_align} can be used to achieve this). Furthermore, it is recommended
#' to use spatialized trajectories (see \link{mt_spatialize}; Wulff et al., in
#' press; Haslbeck et al., 2018).
#'
#' @inheritParams mt_cluster
#' @param kseq a numeric vector specifying set of candidates for k. Defaults to
#'   2:15, implying that all values of k within that range are compared using
#'   the metrics specified in \code{compute}.
#' @param compute character vector specifying the to be computed measures. Can
#'   be any subset of \code{c("stability","gap","jump","slope")}.
#' @param method character string specifying the type of clustering procedure
#'   for the stability-based method. Either \code{hclust} or \code{kmeans}.
#' @param n_bootstrap an integer specifying the number of bootstrap comparisons
#'   used by \code{stability}. See \link[cstab]{cStability}.
#' @param model_based boolean specifying whether the model-based or the
#'   model-free should be used by \code{stability}, when method is
#'   \code{kmeans}. See \link[cstab]{cStability} and Haslbeck & Wulff (2016).
#' @param n_gap integer specifying the number of simulated datasets used by
#'   \code{gap}. See Tibshirani et al. (2001).
#'
#' @return A list containing two lists that store the results of the different
#'   methods. \code{kopt} contains the estimated \code{k} for each of the
#'   methods specified in \code{compute}. \code{paths} contains the values for
#'   each \code{k} in \code{kseq} as computed by each of the methods specified
#'   in \code{compute}. The values in \code{kopt} are optima for each of the
#'   vectors in \code{paths}.
#'
#' @seealso \link{mt_distmat} for more information about how the distance matrix
#'   is computed when the hclust method is used.
#'
#'   \link{mt_cluster} for performing trajectory clustering with a specified
#'   number of clusters.
#'
#' @examples
#'
#' \dontrun{
#' # Spatialize trajectories
#' KH2017 <- mt_spatialize(KH2017)
#'
#' # Find k
#' results <- mt_cluster_k(KH2017, use="sp_trajectories")
#'
#' # Retrieve results
#' results$kopt
#' results$paths
#' }
#'
#' @author
#' Dirk U. Wulff
#'
#' Jonas M. B. Haslbeck
#'
#' @references Haslbeck, J., & Wulff, D. U. (2016). Estimating the Number of
#'   Clusters via Normalized Cluster Instability. \emph{arXiv preprint}
#'   arXiv:1608.07494.
#'
#'   Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F., &
#'   Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in movement
#'   trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G. Johnson
#'   (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145). New
#'   York, NY: Routledge.
#'
#'   Haslbeck, J. M. B., Wulff, D. U., Kieslich, P. J., Henninger, F., &
#'   Schulte-Mecklenbeck, M. (2018). \emph{Advanced mouse- and hand-tracking
#'   analysis: Detecting and visualizing clusters in movement trajectories}.
#'   Manuscript in preparation.
#'   
#'   Tibshirani, R., Walther, G., & Hastie, T. (2001). Estimating the number of
#'   clusters in a data set via the gap statistic. \emph{Journal of the Royal
#'   Statistical Society: Series B (Statistical Methodology), 63}(2), 411-423.
#'
#'   Sugar, C. A., & James, G. M. (2013). Finding the number of clusters in a
#'   dataset. \emph{Journal of the American Statistical Association, 98}(463),
#'   750-763.
#'
#'   Fujita, A., Takahashi, D. Y., & Patriota, A. G. (2014). A non-parametric
#'   method to estimate the number of clusters. \emph{Computational Statistics &
#'   Data Analysis, 73}, 27-39.
#'
#' @export
mt_cluster_k <- function(
  data,
  use = 'sp_trajectories',
  dimensions = c('xpos','ypos'),

  # k-selection type
  kseq    = 2:15, # considered k-sequence
  compute = c('stability','gap','jump','slope'),
  method  = 'hclust', # or 'kmeans'

  # distance arguments
  weights = rep(1,length(dimensions)),
  pointwise = TRUE, #
  minkowski_p = 2,

  # stability arguments
  hclust_method  = 'ward.D', # hclust_method
  kmeans_nstart  = 10, # number of reinitializations for k-means algorithm
  n_bootstrap    = 10, # bootstrap samples
  model_based    = FALSE, # stab_predict

  # arguments distance-based k-selection methods
  n_gap = 10, # simulated datasets for Gap Statistic

  na_rm = FALSE,
  verbose=FALSE
){

  # Extract data
  trajectories <- extract_data(data,use)

  # Tests
  if (method=="hclust") {
    method <- "hierarchical"
  } else if (method != "kmeans") {
    stop('Method must either be "hclust" or "kmeans"')
  }
  if(model_based == TRUE & method == 'hierarchical'){
    stop('Model-based instability methods are only available for k-means clustering.')
  }

  # prepare trajectories
  trajectories = prepare_trajectories(trajectories = trajectories, 
                                      dimensions = dimensions, 
                                      weights = weights,
                                      na_rm = na_rm)
  
  # Transform data structure for clustering input
  rearranged_trajectories = cbind(trajectories[,,dimensions[1]],trajectories[,,dimensions[2]])

  # Define containers
  kopt = list()
  paths = list()


  # Stability-based k-selection methods
  if('stability' %in% compute) {

    if(verbose == TRUE) message('Calculating stability-based k-selection methods.')
    cStab_obj <- cstab::cStability(data = rearranged_trajectories,
                            kseq = kseq,
                            nB = n_bootstrap,
                            norm = TRUE,
                            predict = model_based,
                            method = method,
                            linkage = hclust_method,
                            kmIter = kmeans_nstart,
                            pbar = FALSE)

    kopt[['stab_kopt']] <- unlist(cStab_obj$k_instab_norm)
    paths[['stab_seq']] <- cStab_obj$instab_path_norm

  }

  # ---- Distance-based k-selection methods
  if(any(compute %in% c('gap','jump','slope'))) {

    if(verbose == TRUE) message('Calculating distance-based k-selection methods.')
    cDist_obj <- cstab::cDistance(data = rearranged_trajectories,
                           kseq = kseq,
                           method = method,
                           linkage = hclust_method,
                           kmIter = kmeans_nstart,
                           gapIter = n_gap)

    if('gap' %in% compute){
      kopt[['gap_kopt']] <- unlist(cDist_obj$k_Gap)
      paths[['gap_seq']] <- cDist_obj$Gaps
    }
    if('jump' %in% compute){
      kopt[['jump_kopt']] <- unlist(cDist_obj$k_Jump)
      paths[['jump_seq']] <- cDist_obj$Jumps
    }
    if('slope' %in% compute){
      kopt[['slope_kopt']] <- unlist(cDist_obj$k_Slope)
      paths[['slope_seq']] <- cDist_obj$Slopes
    }
  }

  #output
  output = list(kopt=kopt,paths=paths)
  return(output)

  }





