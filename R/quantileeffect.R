#' Create quantile-effect plot
#' 
#' 
#' @inheritParams mt_time_normalize
#' @param compare either a vector, the label of a variable in , or a mousetrap object.  
#' @param dimensions a character vector specifying the trajectory variables used
#'   to create the heatmap. The first two entries are used as x and 
#'   y-coordinates, the third, if provided, will be added as color information.
#' @param measure character string specifying the variable used to calculate the
#'   effect between 
#'   
#' @examples
#' # Plot regular heatmap
#' #SpiveyEtAl2005 = mt_import_long(SpiveyEtAl2005_raw,'x','y',NULL,'t',
#' #mt_id_label = c('ptp','trial'))
#' #heatmap = mt_heatmap_raw(SpiveyEtAl2005,xres = 2000)
#' #mt_heatmap(heatmap,file = NULL)
#' 
#' # compute measures
#' #SpiveyEtAl2005 = mt_measures(SpiveyEtAl2005)
#' 
#' # Plot heatmap using velocity
#' #mt_heatmap(SpiveyEtAl2005)
#' 
#' @author Dirk U. Wulff (\email{dirk.wulff@@gmail.com})
#' 
#' @return Nothing, when image is plotted using an external device. Otherwise an
#'   object of class \code{mt_object_raw} containing in a matrix format the
#'   image's pixel information.
#'   
#' @export

mt_qeffect = function(
  data,
  compare,
  use = 'measures',
  measure = 'MAD',
  direction = 'upward',
  n_steps = 100,
  return_data = FALSE,
  ...
  ){
  
  # extract use
  if(measure %in% names(data[[use]])){
    use_data = data[[use]]
    } else {
    stop(paste0('Could not find ',use,' in data'))
    }
  
  # extract measure from data
  if(measure %in% names(use_data)){
    measure1 = use_data[[measure]]
    } else {
    stop(paste0('Could not find ',measure,' in data$',use))
    }
  
  # figure our comparison object
  if(is.vector(compare)){
    measures = split(measure1,compare)
    measure1 = measures[[1]]
    messure2 = measures[[2]]
    
    # if compare is label
    } else if(is.character(compare)){
    if(compare %in% names(data$data)){
      measures = split(measure1,data$data[[compare]])
      measure1 = measures[[1]]
      messure2 = measures[[2]]
      } else {
      stop(paste0('Could not find ',measure,' in data$data'))
      }
    
    # if compare is class mousetrap  
    } else if(class(data) == 'mousetrap'){
      
      # extract use
      if(measure %in% names(compare[[use]])){
        use_data = compare[[use]]
        } else {
        stop(paste0('Could not find ',use,' in compare'))
        }
      
      # extract measure from data
      if(measure %in% names(use_data)){
        measure2 = use_data[[measure]]
        } else {
        stop(paste0('Could not find ',measure,' in data$',use))
        }
      } else {
      stop(paste0('Failed to process ',compare))
      }
  
  # full distribution
  all = c(measure1,measure2)
  
  # set quantile steps
  if(direction == 'upward'){
    quantiles = seq(.1,1,length = n_steps)
    } else if(direction == 'downward'){
    quantiles = seq(1,.1,length = n_steps)      
    } else {
    stop('Direction mus be upward or downward')  
    }
  
  # loop over quantiles
  res = matrix(NA,nrow = 3, ncol = n_steps)
  for(i in 1:n_steps){
    lim = quantile(all,quantiles[i])
    if(direction == 'upward'){
      m1 = measure1[measure1 < lim]
      m2 = measure2[measure2 < lim]
      } else {
      m1 = measure1[measure1 > lim]
      m2 = measure2[measure2 > lim]
      }
    res[,i] = cohen(m1,m2)
    }
  
  # reverse effect vector if downward
  if(direction == 'upward') {
    effect = res[1,]
    se = res[2,]
    nu = res[3,]
    } else {
    effect = rev(res[1,])
    se = rev(res[2,])
    nu = rev(res[3,])
    }
  
  # loop over quantils for comparison
  res_comp = matrix(NA,nrow = 3, ncol = n_steps)
  sim1 = rnorm(50000, mean(measure1), sd(measure1))
  sim2 = rnorm(50000, mean(measure2), sd(measure2))
  sim  = c(sim1,sim2)
  for(i in 1:n_steps){
    lim = quantile(sim,quantiles[i])
    if(direction == 'upward'){
      s1 = sim1[sim1 < lim]
      s2 = sim2[sim2 < lim]
      } else {
      s1 = sim1[sim1 > lim]
      s2 = sim2[sim2 > lim]
      }
    res_comp[,i] = cohen(s1,s2)
    }
  
  if(return_data == TRUE){
  
    # out object
    out = rbind(effect,se)
    
    # return
    return(out)
    } else {
    
    # plot
    q = qt(.975,nu[i])
    xlim = c(.5,n_steps + .5)
    ylim = c(floor(min(effect-se*q)*10)/10,ceiling(max(effect+se*q)*10)/10)
    plot.new();plot.window(xlim,ylim)
    
    # plot chance
    lines(xlim,c(0,0),col='black',lwd=2)
    
    # lines sim
    lines(res_comp[1,],lwd=2,col='red',lty=1)
    
    # plot points
    points(effect,pch=16,...)
    
    # plot error bars
    sapply(1:n_steps,function(i) lines(c(i,i),effect[i]+se[i]*c(-q,q)))
    
    # axes
    mtext(round(seq(xlim[1]+.5,xlim[2]-.5,length=10)),at=round(seq(xlim[1]+.5,xlim[2]-.5,length=10)),side=1)
    mtext(round(seq(ylim[1],ylim[2],length=10),1),at=round(seq(ylim[1],ylim[2],length=10),1),side=2,las=1)
    mtext(c(paste0(measure,' percentile'),"Cohens'd"),side=c(1,2),line=c(1.5,1.5),cex=1.2)
    
    
    }
  }





