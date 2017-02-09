#' Cluster trajectories.
#' 
#' Performs trajectory clustering. It first computes distances between each pair
#' of trajectories and then applies off-the-shelf clustering tools to explain 
#' the resulting dissimiliarity matrix using a predefined number of clusters.
#' 
#' \code{mt_cluster} uses off-the-shelf clustering tools, i.e., 
#' \link[fastcluster]{hclust} and \link[stats]{kmeans}, for cluster estimation. 
#' Cluster estimation using \link[fastcluster]{hclust} relies on distances 
#' computed by \link{mt_distmat}.
#' 
#' Mouse trajectories often occur in distinct, qualitative types (see Wulff, 
#' Haslbeck, Schulte-Mecklenbeck, 2017; Haslbeck, Wulff, Kieslich, Henninger, & 
#' Schulte-Mecklenbeck, 2017). Common trajectory types are linear trajectories, 
#' mildly and strongly curved trajctories, and single and multiple 
#' change-of-mind trials (see also \link{mt_map}). \code{mt_cluster} can tease
#' these types apart.
#' 
#' \code{mt_cluster} computes distances between each pair of mouse trajectories 
#' (see \link{mt_distmat}) and applies \link[fastcluster]{hclust} or 
#' \link[stats]{kmeans} to explain the resulting dissimiliarity matrix using a 
#' predefined number of clusters.
#' 
#' We recommend setting \code{method} to \link[fastcluster]{hclust} using 
#' \code{ward.D} as the linkage criterion (via \code{hclust_method}). Relative 
#' to \link[stats]{kmeans}, the other implemented clustering method, and other 
#' linkage criteria, this setup handles the skewed distribution cluster sizes 
#' and trajectory outliers found in the majority of datasets best.
#' 
#' Clustering trajectories requires that the endpoints of all trajectories share
#' the same direction, e.g., that all trajectories end in the top-left corner of
#' the coordinate system (\link{mt_remap_symmetric} or \link{mt_align} can be
#' used to achieve this). Furthermore, it is recommended to use spatialized
#' trajectories (see \link{mt_spatialize}; Haslbeck, Wulff, Kieslich, Henninger,
#' & Schulte-Mecklenbeck, 2017).
#'
#' @inheritParams mt_time_normalize
#' @param save_as a character string specifying where the resulting data should
#'   be stored.
#' @param dimensions a character vector specifying which trajectory variables 
#'   should be used. Can be of length 2 or 3, for two-dimensional or 
#'   three-dimensional trajectories respectively.
#' @param n_cluster an integer specifying the number of clusters to estimate.
#' @param method character string specifiying the clustering procedure. Either
#'   \link[fastcluster]{hclust} (the default) or \link[stats]{kmeans}.
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
#' @references Wulff, D. U., Haslbeck, J. M. B., & Schulte-Mecklenbeck, M.
#'   (2017). Measuring the (dis-)continuous mind. Manuscript in preparation.
#' 
#' Haslbeck, J. M. B., Wulff, D. U., Kieslich, P. J., Henninger, F., & 
#' Schulte-Mecklenbeck, M. (2017). Advanced mouse- and hand-tracking analysis: 
#' Detecting and visualizing clusters in movement trajectories. Manuscript in 
#' preparation.
#'
#' @seealso
#' 
#' \link{mt_distmat} for more information about how the distance matrix is 
#' computed when the hclust method is used.
#' 
#' \link{mt_cluster_k} for estimating the optimal number of clusters.
#'
#' @examples
#' # Spatialize trajectories
#' mt_example <- mt_spatialize(mt_example)
#'
#' # Cluster trajectories
#' mt_example <- mt_cluster(mt_example, use="sp_trajectories")
#'
#' @author
#' Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#'
#' Jonas M. B. Haslbeck (\email{jonas.haslbeck@@gmail.com})
#'
#' @export

mt_cluster <- function(data,
                      use='sp_trajectories',
                      save_as='clustering',
                      dimensions=c('xpos', 'ypos'),
                      n_cluster=5, # k value
                      method='hclust',

                      # distance arguments
                      pointwise=TRUE,
                      minkowski_p=2,

                      # cluster arguments
                      hclust_method='ward.D',
                      kmeans_nstart=10,

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

  # Ensure that there are no NAs
  if (any(is.na(trajectories[,,dimensions]))) {
    stop("Missing values in trajectories not allowed for mt_distmat ",
         "as all trajectories must have the same number of observations.")
  }

  # Cluster trajectories
  if (method == 'hclust') {
    # ... using hclust method
    distm <- mt_distmat(
      trajectories,
      dimensions=dimensions,
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
