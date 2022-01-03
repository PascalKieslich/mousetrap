#' Cluster trajectories.
#' 
#' Performs trajectory clustering. It first computes distances between each pair
#' of trajectories and then applies off-the-shelf clustering tools to explain 
#' the resulting dissimilarity matrix using a predefined number of clusters.
#' 
#' \code{mt_cluster} uses off-the-shelf clustering tools, i.e., 
#' \link[fastcluster]{hclust} and \link[stats]{kmeans}, for cluster estimation. 
#' Cluster estimation using \link[fastcluster]{hclust} relies on distances 
#' computed by \link{mt_distmat}.
#' 
#' Mouse trajectories often occur in distinct, qualitative types (see Wulff et
#' al., 2019; Wulff et al., 2022). Common trajectory types are linear
#' trajectories, mildly and strongly curved trajctories, and single and multiple
#' change-of-mind trials (see also \link{mt_map}). \code{mt_cluster} can tease
#' these types apart.
#' 
#' \code{mt_cluster} uses \link[fastcluster]{hclust} or \link[stats]{kmeans} to 
#' explain the distances between every pair of trajectories using a predefined 
#' number of clusters. If method is "hclust", \code{mt_cluster} computes the 
#' dissimiliarity matrix for all trajectory pairs using \link{mt_distmat}. If
#' method is "kmeans", this is done internally by \link[stats]{kmeans}.
#' 
#' We recommend setting \code{method} to \link[fastcluster]{hclust} using 
#' \code{ward.D} as the linkage criterion (via \code{hclust_method}). Relative 
#' to \link[stats]{kmeans}, the other implemented clustering method, and other 
#' linkage criteria, this setup handles the skewed distribution cluster sizes 
#' and trajectory outliers found in the majority of datasets best.
#' 
#' For clustering trajectories, it is often useful that the endpoints of all
#' trajectories share the same direction, e.g., that all trajectories end in the
#' top-left corner of the coordinate system (\link{mt_remap_symmetric} or
#' \link{mt_align} can be used to achieve this). Furthermore, it is recommended
#' to use length normalized trajectories (see \link{mt_length_normalize}; Wulff
#' et al., 2019, Wulff et al., 2021).
#'
#' @inheritParams mt_time_normalize
#' @param save_as a character string specifying where the resulting data should
#'   be stored.
#' @param dimensions a character vector specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3, for two-dimensional or 
#'   three-dimensional trajectories respectively.
#' @param n_cluster an integer specifying the number of clusters to estimate.
#' @param method character string specifying the clustering procedure. Either
#'   \link[fastcluster]{hclust} (the default) or \link[stats]{kmeans}.
#' @param weights numeric vector specifying the relative importance of the 
#'   variables specified in \code{dimensions}. Defaults to a vector of 1s 
#'   implying equal importance. Technically, each variable is rescaled so that
#'   the standard deviation matches the corresponding value in \code{weights}.
#'   To use the original variables, set \code{weights = NULL}.
#' @param pointwise boolean specifying the way in which dissimilarity between
#'   the trajectories is measured. If \code{TRUE} (the default),
#'   \code{mt_distmat} measures the average dissimilarity and then sums the
#'   results. If \code{FALSE}, \code{mt_distmat}  measures dissimilarity once
#'   (by treating the various points as independent dimensions). This is only
#'   relevant if \code{method} is "hclust". See \link{mt_distmat} for further
#'   details.
#' @param minkowski_p an integer specifying the distance metric for the cluster
#'   solution. \code{minkowski_p = 1} computes the city-block distance,
#'   \code{minkowski_p = 2} (the default) computes the Euclidian distance,
#'   \code{minkowski_p = 3} the cubic distance, etc. Only relevant if
#'   \code{method} is "hclust". See \link{mt_distmat} for further details.
#' @param hclust_method character string specifying the linkage criterion used. 
#'   Passed on to the \code{method} argument of \link[stats]{hclust}. Default is
#'   set to \code{ward.D}. Only relevant if \code{method} is "hclust".
#' @param kmeans_nstart integer specifying the number of reruns of the kmeans 
#'   procedure. Larger numbers minimize the risk of finding local minima. Passed
#'   on to the \code{nstart} argument of \link[stats]{kmeans}. Only relevant if 
#'   \code{method} is "kmeans".
#' @param na_rm logical specifying whether trajectory points containing NAs 
#'   should be removed. Removal is done column-wise. That is, if any trajectory 
#'   has a missing value at, e.g., the 10th recorded position, the 10th position
#'   is removed for all trajectories. This is necessary to compute distance
#'   between trajectories.
#' @param cluster_output logical. If \code{FALSE} (the default), the mousetrap 
#'   data object with the cluster assignments is returned (see Value). If 
#'   \code{TRUE}, the output of the cluster method (\code{kmeans} or 
#'   \code{hclust}) is returned directly.
#'
#' @return A mousetrap data object (see \link{mt_example}) with an additional
#'   \link{data.frame} added to it (by default called \code{clustering}) that
#'   contains the cluster assignments. If a trajectory array was provided
#'   directly as \code{data}, only the clustering \code{data.frame} will be
#'   returned.
#'
#' @references Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in
#'   movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G.
#'   Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145).
#'   New York, NY: Routledge.
#'   
#'   Wulff, D. U., Kieslich, P. J., Henninger, F., Haslbeck, J. M. B., &
#'   Schulte-Mecklenbeck, M. (2021). \emph{Movement tracking of cognitive
#'   processes: A tutorial using mousetrap.} PsyArXiv.
#'   \doi{10.31234/osf.io/v685r}
#'    
#'   Wulff, D. U., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M. (2022).
#'   \emph{Measuring the (dis-)continuous mind: What movement trajectories
#'   reveal about cognition}. Manuscript in preparation.
#'
#' @seealso \link{mt_distmat} for more information about how the distance matrix is 
#'   computed when the hclust method is used.
#'
#'   \link{mt_cluster_k} for estimating the optimal number of clusters.
#'
#' @examples
#' # Length normalize trajectories
#' KH2017 <- mt_length_normalize(KH2017)
#'
#' # Cluster trajectories
#' KH2017 <- mt_cluster(KH2017, use="ln_trajectories")
#' 
#' # Plot clustered trajectories
#' mt_plot(KH2017,use="ln_trajectories",
#'   use2="clustering",facet_col="cluster")
#'
#' @author
#' Dirk U. Wulff
#'
#' Jonas M. B. Haslbeck
#'
#' @export

mt_cluster <- function(data,
                      use='ln_trajectories',
                      save_as='clustering',
                      dimensions=c('xpos', 'ypos'),
                      n_cluster=5, # k value
                      method='hclust',

                      # distance arguments
                      weights = rep(1,length(dimensions)),
                      pointwise=TRUE,
                      minkowski_p=2,

                      # cluster arguments
                      hclust_method='ward.D',
                      kmeans_nstart=10,

                      na_rm = FALSE,
                      cluster_output=FALSE,
                      verbose=FALSE
                      ){

  # Extract data
  trajectories <- extract_data(data, use)
  
  # Tests
  if (!length(dimensions) %in% c(2, 3))
    stop('Dimensions must be of length 2 or 3.')
  if (!all(c(dimensions) %in% dimnames(trajectories)[[3]]))
    stop('Not all dimensions exist.')
  if (!method %in% c("hclust", "kmeans"))
    stop('Method must either be "hclust" or "kmeans".')
  
  
  # prepare trajectories
  trajectories = prepare_trajectories(trajectories = trajectories, 
                                      dimensions = dimensions, 
                                      weights = weights,
                                      na_rm = na_rm)
  
  # Cluster trajectories
  if (method == 'hclust') {
    # ... using hclust method
    distm <- mt_distmat(
      trajectories,
      dimensions=dimensions,
      weights = NULL,
      pointwise=pointwise,
      minkowski_p=minkowski_p
    )
    distm <- stats::as.dist(distm)
    
    # Actual clustering
    cl_obj <- fastcluster::hclust(distm, method=hclust_method)
    cl_ass <- stats::cutree(cl_obj, n_cluster)
  } else {
    # ... using kmeans method
    # rearrange data structure for clustering input
    rearranged_trajectories <- c()
    for (i in 1:length(dimensions)) {
      rearranged_trajectories <- cbind(
        rearranged_trajectories,
        trajectories[,,dimensions[i]]
      )
    }

    # k-means clustering
    cl_obj <- stats::kmeans(
      rearranged_trajectories,
      centers=n_cluster,
      nstart=kmeans_nstart
    )
    cl_ass <- cl_obj$cluster
  }

  # Return data
  if (cluster_output == TRUE) {
    return(cl_obj)
  } else {
    return(create_results(
      data=data, results=data.frame(cluster=cl_ass),
      use=use, save_as=save_as,
      ids=rownames(trajectories), overwrite=FALSE
    ))
  }
}
