#' Creates high-resolution heatmap of trajectory data.
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' \code{mt_heatmap_raw} creates a high-resolution heatmap image of the 
#' trajectory data using gaussian smoothing. Note that this function has beta
#' status.
#' 
#' @details 
#' 
#' To create the image, \code{mt_heatmap_raw} takes the following steps. First, 
#' the function maps the trajectory points to a pixel space with x ranging from 
#' 1 to xres and y ranging from 1 to xres divided by the ratio of x and y's 
#' value range. Second, the function counts and normalizes the number of 
#' trajectory points occupying each of the x,y-pixels to yield image intensities
#' between 0 and 1. Third, the function smooths the image using an approximative
#' guassian approach governed by \code{smooth_radius}, which controls the 
#' dispersion of the gaussian smoothing. Fourth, the function automatically 
#' enhances the image (unless \code{auto_enhance = FALSE}) using a non-linear 
#' transformation in order to yield a desired \code{mean_image} intensity. 
#' Fifth, the function translates the image intensity into color using the 
#' colors specified in \code{colors}. Finally, the function returns the image 
#' data in a long format containing the x, y, and color information.
#' 
#' \code{mt_heatmap_raw} also offers the possibility to overlay the heatmap with 
#' an additional variable, such as for instance velocity, so that both the 
#' density of mouse trajectories and the information of the additional variable 
#' are visible. In order to do this, specify a third variable label in 
#' \code{dimensions} and control its appearance using the \code{color} and 
#' \code{mean_color} arguments.
#' 
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the trajectory variables used
#'   to create the heatmap. The first two entries are used as x and 
#'   y-coordinates, the third, if provided, will be added as color information.
#' @param variable boolean or numeric vector matching the number of trajectories
#'   that if provided will be used as color information. \code{variable} is only
#'   considered when \code{length(dimensions) < 3}.
#' @param bounds numeric vector specifying the corners (xmin, ymin, xmax, ymax) 
#'   of the plot region. By default (\code{bounds = NULL}), bounds are 
#'   determined based on the data input.
#' @param xres an integer specifying the number of pixels along the x-dimension.
#'   An \code{xres} of 1000 implies an 1000*N px, where N is determined so that 
#'   the trajectories aspect ratio is preserved (provided the \code{bounds} are 
#'   unchanged).
#' @param upsample a numeric value by which the number of points used to 
#'   represent individual trajectories are increased or decreased. Values of 
#'   smaller than one will improve speed but also introduce a certain level of 
#'   granularity.
#' @param norm a logical specifying whether the data should be warped into 
#'   standard space. If \code{norm = TRUE}, this overrules \code{bounds}.
#' @param colors a character vector specifying two or three colors used to color
#'   the background, the foreground (trajectories), and the values of a third
#'   dimension (if specified).
#' @param n_shades an integer specifying the number of shades for the color 
#'   gradient between the first and second, and the second and third color in 
#'   \code{colors}.
#' @param smooth_radius a numeric value specifying the standard deviation of the
#'   gaussian smoothing. If zero, smoothing is omitted.
#' @param low_pass an integer specifying the allowed number of counts per pixel.
#'   This arguments limits the maximum pixel color intensity.
#' @param auto_enhance boolean. If \code{TRUE} (the default), the image is 
#'   adjusted so that the mean color intensity matches \code{mean_image} and 
#'   \code{mean_color}.
#' @param mean_image a numeric value between 0 and 1 specifying the average 
#'   foreground color intensity across the entire image. Defaults to 0.1.
#' @param mean_color a numeric value between 0 and 1 specifying the average 
#'   third dimension's color intensity across the entire image. Defaults to 0.1.
#'   Only relevant if a third dimension is specified in \code{colors}.
#' @param aggregate_lwd an integer specifying the width of the aggregate 
#'   trajectory. If \code{aggregate_lwd} is 0 (the default), the aggregate 
#'   trajectory is omitted.
#' @param aggregate_col a character value specifying the color of the aggregate 
#'   trajectory.
#' @param n_trajectories an optional integer specifying the number of 
#'   trajectories used to create the image. By default, all trajectories are 
#'   used. If \code{n_trajectories} is specified and smaller than the number of 
#'   trajectories in the trajectory array, then \code{n_trajectories} are 
#'   randomly sampled.
#' @param seed an optional integer specifying the seed used for the trajectory 
#'   sampling.
#' 
#' @author Dirk U. Wulff
#' 
#' @return An object of class \code{mt_object_raw} containing in a matrix format
#'   the image's pixel information, the aggregate trajectory, and the colors.
#' 
#' @seealso \link{mt_heatmap} and  \link{mt_heatmap_ggplot} for plotting
#'   trajectory heatmaps.
#'
#'   \link{mt_diffmap} for plotting trajectory difference-heatmaps.
#' 
#' @references Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in
#'   movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G.
#'   Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145). New York, NY:
#'   Routledge.
#'
#'   Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B., &
#'   Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to
#'   implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J.
#'   G. Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp.
#'   111-130). New York, NY: Routledge.
#'   
#' @export
mt_heatmap_raw <- function(
  data,
  use = 'trajectories',
  dimensions = c('xpos', 'ypos'),
  variable = NULL,
  
  # plot arguments
  bounds    = NULL,
  xres      = 1000,
  upsample  = 1,
  norm      = FALSE,
  
  # color arguments
  colors   = c('black','blue', 'white'),
  n_shades = c(1000, 1000),
  
  # image processing
  smooth_radius  = 1.5,
  low_pass  = 200,
  auto_enhance = TRUE,
  mean_image = .15,
  mean_color = .25,
  
  # plot aggregate
  aggregate_lwd = 0,
  aggregate_col = 'black',
  
  # subsample arguments
  n_trajectories = NULL,
  seed = NULL,
  
  # control
  verbose = TRUE
){
  
  # Data checks
  if (!length(dimensions) %in% c(1, 2, 3)) {
    stop('Dimensions must of length 1, 2 or 3!')
  }
  
  # extract trajectories  
  trajectories = extract_data(data,use)
  
  if (!all(dimensions %in% dimnames(trajectories)[[3]])) {
    stop('Not all dimensions exist in data')
  }
  trajectories <- trajectories[,,dimensions,drop = F]
  
  # Add first dimension -----------------------------------------------------
  
  if(length(dimensions) == 1){
    add = matrix(rep(1:ncol(trajectories),nrow(trajectories)), 
                 ncol = ncol(trajectories), byrow = T)
    trajectories <- mt_add_variables(trajectories, variables = list('add' = add))
    trajectories <- trajectories[,,2:1]
    dimensions <- c('add',dimensions)
  }
  
  
  # Subsample trajectories -----------------------------------------------------
  # If n_trajectories is smaller than number of trajectories,
  # subsample data to n_trajectories
  if(is.null(n_trajectories)==FALSE){
    if (n_trajectories < dim(trajectories)[1]) {
      if (verbose == TRUE) cat('subset trajectories','\n')
      seed <- ifelse(is.null(seed),
                     round(stats::runif(1,0,stats::runif(1,0,.Machine$integer.max))),
                     seed)
      trajectories <- subsample(trajectories, n=n_trajectories, seed=seed)
    }
  }
  
  # Get aggregate --------------------------------------------------------------
  # Compute aggregate x and y
  aggregate = cbind(
    colMeans(trajectories[,,dimensions[1]],na.rm=TRUE),
    colMeans(trajectories[,,dimensions[2]],na.rm=TRUE)
  )
  
  # Determine Dimensions ----------------------------------------------------
  # Rescale trajectories so that there are sufficient points to fill
  # the diagonal, i.e, length of diagonal divided by px
  
  if (verbose == TRUE) cat('spatializing trajectories','\n')
  
  if(is.null(bounds) & norm == FALSE){
    range_x = range(trajectories[,,dimensions[1]],na.rm=TRUE)
    range_y = range(trajectories[,,dimensions[2]],na.rm=TRUE)
    mid_x   = range_x[1] + diff(range_x) / 2
    mid_y   = range_y[1] + diff(range_y) / 2
    range_x = mid_x + (range_x - mid_x) * 1.02
    range_y = mid_y + (range_y - mid_y) * 1.02
    bounds = c(range_x[1],range_y[1],range_x[2],range_y[2])
  } else if(norm == TRUE){
    trajectories = mt_align(trajectories,coordinates = 'mt')
    bounds = c(-1.4, -.2, 1.4, 1.9)
    range_x = c(bounds[1],bounds[3])
    range_y = c(bounds[2],bounds[4])
  } else {
    range_x = bounds[c(1,3)]
    range_y = bounds[c(2,4)]
  }
  
  # Determine pixel size
  margin  = ceiling(smooth_radius * 2) + ceiling(smooth_radius * 2)
  xres_m  = xres - margin - 1
  px_size = diff(range_x) / xres_m
  
  # Determine number of resc
  xres_m  = xres_m
  yres_m  = ceiling(xres_m * diff(range_y) / diff(range_x))
  n_resc  = ceiling(sqrt(xres_m * xres_m + yres_m * yres_m))
  l_diag  = sqrt(diff(range_x)**2 + diff(range_y)**2)
  
  
  # Add third dimension -----------------------------------------------------
  
  if(length(dimensions) < 3 & !is.null(variable)){
    if(length(variable) == nrow(trajectories)){
      add = matrix(rep(variable,rep(ncol(trajectories),length(variable))), 
                   ncol = ncol(trajectories), byrow = T)
      trajectories <- mt_add_variables(trajectories, variables = list('add' = add))
      dimensions <- c(dimensions, 'add')
    } else {
      stop('Variable does not match number of trajectories.')
    }
  }
  
  # Determine Dimensions ----------------------------------------------------
  
  # Determine number of resc
  lengths  = getLengths(
    trajectories[,,dimensions[1]],
    trajectories[,,dimensions[2]]
  )
  
  n_points = round(n_resc * (lengths / l_diag))
  n_points = n_points * upsample
  
  spatialized_trajectories = mt_spatialize_tolong(
    trajectories,
    dimensions = dimensions,
    n_points = n_points
  )
  
  if (aggregate_lwd > 0) {
    agg_x = aggregate[,1]
    agg_y = aggregate[,2]
    agg_l = getLength(agg_x, agg_y)
    spatialized_aggregate = spatialize(
      agg_x, agg_y,
      round(2 * n_resc * agg_l / l_diag)
    )
  }
  
  # Compute raw image ----------------------------------------------------------
  if (verbose == TRUE) cat('calculate image','\n')
  
  # retrieve image
  pts = spatialized_trajectories
  
  # range of pixels plus white space around image
  xs = 0 : (xres_m + margin) + 1
  ys = 0 : (yres_m + ceiling(smooth_radius * 2) + ceiling(smooth_radius * 2)) + 1
  
  # Remove points outside of bounds
  pts = pts[pts[,1] >= bounds[1] &
            pts[,1] <= bounds[3] &
            pts[,2] >= bounds[2] &
            pts[,2] <= bounds[4],]
  
  
  # Determine pixel locations
  x  = round(((pts[,1] - bounds[1]) / px_size) + 1)
  y  = round(((pts[,2] - bounds[2]) / px_size) + 1)
  
  # Determine table of pixels
  #img_df = data_frame('x'=x,'y'=y)
  #img_tb = img_df %>% group_by(x,y) %>% tally() %>% ungroup()
  img_tb = tab(x, y)
  
  # Map pixels into matrix of zeros
  img_mat = matrix(0, ncol=length(xs), nrow=length(ys))
  img_mat[as.matrix(img_tb[,2:1]) + ceiling(smooth_radius * 2)] = img_tb[,3]
  
  # Store raw image, pixel locations and
  raw_img = c(t(img_mat))
  xys     = expand.grid(1:length(xs), 1:length(ys))
  
  # Calculate overlay information and create ovrlay image
  if (length(dimensions) == 3) {
    a = pts[,3]
    if (any(is.na(a))) {
      a[is.na(a)] = 0
      message('NAs in third dimension replaced by 0')
    }
    
    #img_df = data_frame('x'=x,'y'=y,'a'=a)
    #img_tb = img_df %>% group_by(x,y) %>% summarize(a = mean(a)) %>% ungroup()
    img_tb = tab_mean(x,y,a)
    
    img_mat = matrix(0, ncol=length(xs), nrow=length(ys))
    img_mat[as.matrix(img_tb[,2:1]) + smooth_radius * 2] = img_tb[,3]
    add_img = c(t(img_mat))
  } else {
    add_img = rep(1, length(raw_img))
  }
  
  # get aggregate points
  agg = NULL
  if (aggregate_lwd > 0) {
    agg_x = round(((spatialized_aggregate[,1] - bounds[1]) / px_size) + 1) + smooth_radius * 2
    agg_y = round(((spatialized_aggregate[,2] - bounds[2]) / px_size) + 1) + smooth_radius * 2
    test  = (agg_x > 0 & agg_x <= max(xs) &
               agg_y > 0 & agg_y <= max(ys))
    agg_x = agg_x[test]
    agg_y = agg_y[test]
    agg   = data.frame(
      'x'=agg_x, 'y'=agg_y,
      'col'=aggregate_col, 'lwd'=aggregate_lwd,
      stringsAsFactors = F
    )
  }
  
  # Smooth image ---------------------------------------------------------------
  
  smooth_img = raw_img
  if (smooth_radius > 0) {
    if(verbose == TRUE)
      cat('smooth image','\n')
    
    smooth_img = gaussBlur(
      smooth_img, smooth_img,
      max(xs), max(ys),
      smooth_radius
    )
    
    if (length(dimensions) == 3) {
      add_img = gaussBlur(
        add_img, add_img,
        max(xs), max(ys),
        smooth_radius
      )
    }
  }
  
  # Create, normalize, and enhance image ---------------------------------------
  # Low-pass: shave off max color intensities
  # Enhance contrast
  # Normalize image
  
  # create image object
  img = data.frame(xys, smooth_img, add_img)
  names(img) = c('x','y','img','a')
  
  # Low-pass
  img$img[img$img > low_pass * upsample] = low_pass  * upsample
  
  # Normalize image
  img$img = (img$img - min(img$img)) / max(img$img - min(img$img))
  
  if (length(dimensions) == 3) {
    img$a = (img$a - min(img$a)) / max(img$a - min(img$a))
  }
  
  # Enhance image
  if (auto_enhance == TRUE) {
    ms = c(); steps = c(.1, 2, 5, 10, 20, 50, 100)
    
    for (i in steps) {
      ms = c(ms, abs(mean(abs(img$img-1)**i - 1)));
    }
    
    enhance = exp(stats::predict(fields::qsreg(ms,log(steps)),mean_image))
    if (enhance > max(steps)) enhance = max(steps)
    if (enhance < 0) enhance = 1
    img$img = abs(abs(img$img - 1)**enhance - 1)
    
    if (verbose == TRUE) cat('enhance image by', round(enhance, 1), '\n')
    
    if(length(dimensions) == 3){
      ms = c(); steps = c(.1, .5, 1, 2, 5, 10, 20, 50, 100)
      for (i in steps) {
        ms = c(ms, abs(mean(abs(img$a-1)**i - 1)));
      }
      enhance = exp(stats::predict(fields::qsreg(ms,log(steps)),mean_color))
      if (enhance > max(steps)) enhance = max(steps)
      if (enhance < 0) enhance = 1
      img$a = abs(abs(img$a - 1)**enhance - 1)      
      
      if (verbose == TRUE) cat('enhance image color by', round(enhance, 1), '\n')
    }
    
  }
  
  # Determine colors -----------------------------------------------------------
  
  img$img = group(img$img, n_shades[1])
  
  if (length(dimensions) == 2) {
    img$col = colormixer(
      colors[1], colors[2], img$img, format='hex'
    )
  }
  
  if (length(dimensions) == 3) {
    if (length(colors) < 3 | length(n_shades) < 2) {
      stop('Colors and n_shades must be (at least) of length 2 and 3, respectively.')
    }
    img$a = group(img$a, n_shades[2])
    color_tone = colormixer(colors[2], colors[3], img$a)
    img$col = colormixer(colors[1], color_tone, img$img, format='hex')
    
  }
  
  # create output -----------------------------------------------------------
  heatmap = list('img' = img[,c('x', 'y', 'img', 'col')], 'agg' = agg, 'colors' = colors)
  class(heatmap) = 'mt_heatmap_raw'
  
  # Export raw data
  return(heatmap)
  
}


#' Plot trajectory heatmap.
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' \code{mt_heatmap} plots high resolution raw trajectory maps. Note that this
#' function has beta status.
#' 
#' @details 
#' 
#' \code{mt_heatmap} wraps \link{mt_heatmap_raw} and provides direct plotting 
#' output in \link[grDevices:png]{tiff}, \link[grDevices]{png},
#' \link[grDevices]{pdf}, or R's default window output. For further details on
#' how the trajectory heatmaps are constructed, see \link{mt_heatmap_raw}.
#' 
#' @inheritParams mt_heatmap_raw
#' @param x usually an object of class \code{mousetrap}. Alternatively, a
#'   trajectory array or an object of class \code{mt_heatmap_raw}.
#' @param filename a character string giving the name of the file. If 
#'   \code{NULL} (the default), the R standard device is used for plotting.
#'   Otherwise, the plotting device is inferred from the file extension. Only
#'   supports devices \link[grDevices:png]{tiff}, \link[grDevices]{png},
#'   \link[grDevices]{pdf}.
#' @param ... arguments passed to \link{mt_heatmap_raw}.
#' @param upscale a numeric value by which the output resolution of the image is
#'   increased or decreased. Only applies if device is one of tiff, png, or pdf.
#' @param plot_dims adds the coordinates of the four image corners to the plot. 
#'   Helps setting \code{bounds}.
#'   
#' @seealso \link{mt_heatmap_ggplot} for plotting a trajectory heatmap using
#'   ggplot2.
#'
#'   \link{mt_diffmap} for plotting trajectory difference-heatmaps.
#' 
#' @author Dirk U. Wulff
#' 
#' @references Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in
#'   movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G.
#'   Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145). New York, NY:
#'   Routledge.
#'
#'   Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B., &
#'   Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to
#'   implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J.
#'   G. Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp.
#'   111-130). New York, NY: Routledge.
#'    
#' @examples
#' mt_heatmap(KH2017, xres=500, n_shades=5, mean_image=0.2)
#' 
#' 
#' 
#' @export
mt_heatmap <- function(
  x,
  use = 'trajectories',
  dimensions = c('xpos', 'ypos'),
  filename   = NULL,
  ...,
  upscale = 1,
  plot_dims = FALSE,
  verbose = TRUE
){
  
  # --------- collect device
  if (!is.null(filename)) {
    device = strsplit(filename, '[.]')[[1]]
    device = device[length(device)]
    if (!device[length(device)] %in% c('pdf', 'png', 'tiff')) {
      stop('filename != NULL requires one of .pdf, .png, or .pdf as file extension.')
    }
  } else {
    device = 'none'
  }
  
  # take time
  t_start = proc.time()[3]
  
  # --------- get heatmap
  agg = NULL
  if(is_mousetrap_data(x) | is.array(x)){
    if(is_mousetrap_data(x)) x = extract_data(data = x, use = use)
    if(!all(dimensions %in% dimnames(x)[[3]])) stop('Not all dimensions found.')
    heatmap = mt_heatmap_raw(x,use,dimensions,verbose=verbose,...)
    img = heatmap$img
    agg = heatmap$agg
    bg  = heatmap$colors[1]
  } else if(inherits(x,'mt_heatmap_raw')){
    img = x$img
    agg = x$agg
    bg  = heatmap$colors[1]
  } else if(is.data.frame(x)){
    if(all(c('x','y','col') %in% names(x))){
      img = x
    } else {
      stop('x is non-usable data frame. See documentation.')
    }
  } else {
    stop('x has non-usable data format. see documentation.')
  }
  
  if (verbose == TRUE) {
    cat('creating heatmap: ', max(img$x), 'x', max(img$y), 'px', '\n')
  }
  
  # --------- collect device
  if (device == 'pdf') {
    grDevices::pdf(
      filename,
      width=10 * (max(img$x) / max(img$y)) * upscale,
      height=10 * upscale,
      bg = bg
    )
  } else if(device == 'png') {
    grDevices::png(
      filename,
      width=max(img$x) * upscale,
      height=max(img$y) * upscale,
      bg = bg
    )
  } else if (device == 'tiff') {
    grDevices::tiff(
      filename,
      width=max(img$x) * upscale,
      height=max(img$y) * upscale,
      bg = bg
    )
  }
  
  # get dimensions
  range_x = range(img$x)
  range_y = range(img$y)
  
  # remove 0s
  p_img = img
  p_img = p_img[img$img>0,]
  
  # set canvas
  graphics::par(bg = bg)
  graphics::plot.new()
  graphics::par( mar=c(0, 0, 0, 0))
  graphics::plot.window(xlim=range_x + c(-.5, .5),
                        ylim=range_y + c(-.5, .5)
  )
  graphics::par(usr = c(range_x + c(-.5, .5),range_y + c(-.5, .5)))
  
  # plot points
  graphics::rect(
    p_img$x - .5, p_img$y - .5,
    p_img$x + .5, p_img$y + .5,
    col=p_img$col,
    border=NA
  )
  
  if (!is.null(agg)) {
    graphics::points(agg[1:2],cex=agg$lwd,col=agg$col,pch=16)
  }
  
  # plot dimensions
  if(plot_dims){
    d_x  = diff(range_x)
    d_y  = diff(range_y)    
    xpos = range_x[1] + d_x * c(.05,.95)
    ypos = range_y[1] + d_y * c(.05,.95)
    cord = round(expand.grid(xpos,ypos))
    if(heatmap$colors[1] == 'black'){
      graphics::text(cord,labels = paste0('(',cord[,1],',',cord[,2],')'),col = 'white')
    } else {
      graphics::text(cord,labels = paste0('(',cord[,1],',',cord[,2],')'),col = 'black')        
    }
  }
  
  if(device %in% c('pdf','png','tiff')) {
    grDevices::dev.off()
  }
  
  # Finalization ---------------------------------------------------------------
  # Give feedback
  if (verbose == TRUE) {
    t_end = proc.time()[3] - t_start
    cat('heatmap created in ', round(t_end), 's\n', sep='')
  }
}


#' Plot trajectory heatmap using ggplot.
#' 
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' \code{mt_heatmap_ggplot} plots high resolution raw trajectory maps. Note that
#' this function has beta status.
#' 
#' @details 
#'
#' \code{mt_heatmap_ggplot} wraps \link{mt_heatmap_raw} and returns a ggplot
#' object containing the plot. In contrast to \code{mt_heatmap_plot} plots
#' created by \code{mt_heatmap_ggplot} can be extended using ggplot's \code{+}
#' operator. For further details on how the trajectory heatmaps are constructed,
#' see \link{mt_heatmap_raw}.
#'
#' @inheritParams mt_heatmap_raw
#' @param use2 an optional character string specifying where the data that
#'   contain the variables used for faceting can be found (in case these
#'   arguments are specified). Defaults to "data" as \code{data[["data"]]}
#'   usually contains all non mouse-tracking trial data.
#' @param facet_row an optional character string specifying a variable in
#'   \code{data[[use2]]} that should be used for (row-wise) faceting.
#' @param facet_col an optional character string specifying a variable in
#'   \code{data[[use2]]} that should be used for (column-wise) faceting.
#' @param ... arguments passed to \link{mt_heatmap_raw}.
#' 
#' @author
#' Pascal J. Kieslich
#'
#' Felix Henninger
#'
#' Dirk U. Wulff
#' 
#' @seealso \link{mt_heatmap} for plotting a trajectory heatmap using base
#'   plots.
#'
#'   \link{mt_diffmap} for plotting trajectory difference-heatmaps.
#' 
#' @references Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in
#'   movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G.
#'   Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145). New York, NY:
#'   Routledge.
#'
#'   Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B., &
#'   Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to
#'   implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J.
#'   G. Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp.
#'   111-130). New York, NY: Routledge.
#'   
#' @examples
#' mt_heatmap_ggplot(KH2017, xres=500, n_shades=5, mean_image=0.2)
#' 
#' @export
mt_heatmap_ggplot <- function(data,
  use="trajectories",
  dimensions = c("xpos", "ypos"),
  use2="data",
  facet_row=NULL, facet_col=NULL,
  ...) {
  
  # Extract trajectory data
  trajectories <- extract_data(data=data,use=use)
  
  # Setup bounds (code taken from mt_heatmap_raw)
  range_x <- range(trajectories[,,dimensions[1]],na.rm=TRUE)
  range_y <- range(trajectories[,,dimensions[2]],na.rm=TRUE)
  mid_x <- range_x[1] + diff(range_x) / 2
  mid_y <- range_y[1] + diff(range_y) / 2
  range_x <- mid_x + (range_x - mid_x) * 1.02
  range_y <- mid_y + (range_y - mid_y) * 1.02
  bounds <- c(range_x[1],range_y[1],range_x[2],range_y[2])
  
  # Combine faceting variables
  use2_variables <- c(facet_col,facet_row)
  
  # If faceting variables are specified, create separate heatmaps per facet level
  if (is.null(use2_variables) == FALSE) {
    
    factor_levels <- unique(data[[use2]][,use2_variables,drop=FALSE])
    for (var in use2_variables){
      factor_levels <- factor_levels[order(factor_levels[,var]),,drop=FALSE]
    }
    
    plot_data <- data.frame()

    for (i in 1:nrow(factor_levels)){
      
      # Select the relevant trajectories
      keep <- rep(TRUE,nrow(data[[use2]]))
      for (var in use2_variables){
        keep <- keep & data[[use2]][,var]==factor_levels[i,var]
      }
      current_trajectories <- trajectories[rownames(trajectories) %in% rownames(data[[use2]])[keep],,,drop=FALSE]
      
      # Create heatmap
      current_plot_data <- mt_heatmap_raw(data=current_trajectories,use=use,dimensions=dimensions,bounds=bounds,...)
      current_plot_data$img[,use2_variables] <- factor_levels[i,use2_variables]
      plot_data <- rbind(plot_data,current_plot_data$img)
    }
    
    
  # If no facets are specified, create a single heatmap
  } else{
    
    
    plot_data <- mt_heatmap_raw(data=data,use=use,dimensions=dimensions,bounds=bounds,...)
    plot_data <- plot_data$img
    
  }
  
  
  
  # Setup plot
  current_plot <- ggplot2::ggplot(
    ggplot2::aes_string(x="x", y="y"),
    data=plot_data
  ) +
    ggplot2::scale_x_continuous(
      expand=c(0,0), limits=range(plot_data$x)
    ) +
    ggplot2::scale_y_continuous(
      expand=c(0,0), limits=range(plot_data$y)
    ) +
    ggplot2::geom_tile(
      fill=plot_data$col
    ) +
    ggplot2::theme(
      panel.grid=ggplot2::element_blank(),
      panel.border=ggplot2::element_blank(),
      plot.margin=ggplot2::unit(c(0,0,0,0), "lines"),
      axis.title=ggplot2::element_blank(),
      axis.text=ggplot2::element_blank(),
      axis.ticks=ggplot2::element_blank(),
      axis.line =ggplot2::element_blank()
    ) +
    ggplot2::labs(x=NULL, y=NULL)
  

  # Add facets (optional)
  if(is.null(use2_variables) == FALSE) {
    facet_row <- ifelse(is.null(facet_row),".",facet_row)
    facet_col <- ifelse(is.null(facet_col),".",facet_col)
    facet_formula <- stats::as.formula(paste(facet_row,facet_col,sep="~"))
    current_plot <- current_plot + ggplot2::facet_grid(facet_formula)
  }

  
  return(current_plot)
}

#' Generic print for class mt_heatmap_raw 
#'
#' \code{print.mt_heatmap_raw} shows \link[utils]{str}.
#'
#' @param x an object of class mt_heatmap_raw.
#' @param ... further arguments passed to or from other methods.
#'
#' @method print mt_heatmap_raw
#' @export
print.mt_heatmap_raw = function(x,...){
  utils::str(x)
}


#' Creates a difference-heatmap of two trajectory heatmap images.
#'
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' \code{mt_diffmap} creates a difference-heatmap of the trajectory data using
#' gaussian smoothing. Note that this function has beta status.
#' 
#' @details 
#' 
#' \code{mt_diffmap} takes two objects that either contain trajectory heatmaps
#' or from which trajectory heatmaps can be computed. Difference-heatmaps are
#' constructed analogously to \link{mt_heatmap_raw}.  
#' 
#' @inheritParams mt_heatmap_raw
#' @inheritParams mt_heatmap
#' @param x an object of class \code{mousetrap}), a trajectory object of class 
#'   \code{array}, or an object of class \code{mt_heatmap_raw} (as created by
#'   \link{mt_heatmap_raw}).
#' @param y an object of class \code{mousetrap}), a trajectory object of class 
#'   \code{array}, or an object of class \code{mt_heatmap_raw} (as created by
#'   \link{mt_heatmap_raw}). The class of \code{y} must match the class of 
#'   \code{x}, unless \code{y} is \code{NULL}.
#' @param condition either a character value specifying which variable codes the
#'   two conditions (in \code{x[[use2]]}) that should be compared - or a vector
#'   matching the number of trajectories in \code{x[[use]]} that has exactly two
#'   levels. \code{mt_diffmap} will create a difference-heatmap comparing all
#'   trajectories between the two conditions. If \code{condition} is specified,
#'   \code{y} will be ignored (unless \code{x} and \code{y} are of class
#'   \code{heatmap_raw}).
#' @param use2 an optional character string specifying where the data that
#'   contain the condition variable can be found. Defaults to "data" as
#'   \code{x[["data"]]} usually contains all non mouse-tracking trial data.
#' @param colors a character vector specifying the colors used to color
#'   cases of \code{image1 > image2, image1 ~ image2, image1 < image2},
#'   respectively. Note that the colors are used in that specific order.
#'   Defaults to c("#00863F", "#FFFFFF", "#FF1900") which specifies 
#'   a green-black-red color gradient.
#' @param n_shades integer specifying the number of shades for the color
#'   gradient between the first and second, and the second and third color in
#'   \code{colors}.
#' @param plot logical specifying whether resulting image should be plotted 
#'   (\code{plot = TRUE}, the default). If (\code{plot = FALSE}), an object of
#'   class \code{mt_object_raw} is returned.
#' @param ... arguments passed to \link{mt_heatmap_raw}.
#'
#' @author
#' Dirk U. Wulff
#'
#' Pascal J. Kieslich
#' 
#' @seealso
#' \link{mt_heatmap} and \link{mt_heatmap_ggplot} for plotting trajectory 
#' heatmaps.
#' 
#' @references Wulff, D. U., Haslbeck, J. M. B., Kieslich, P. J., Henninger, F.,
#'   & Schulte-Mecklenbeck, M. (2019). Mouse-tracking: Detecting types in
#'   movement trajectories. In M. Schulte-Mecklenbeck, A. Kühberger, & J. G.
#'   Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp. 131-145). New York, NY:
#'   Routledge.
#'
#'   Kieslich, P. J., Henninger, F., Wulff, D. U., Haslbeck, J. M. B., &
#'   Schulte-Mecklenbeck, M. (2019). Mouse-tracking: A practical guide to
#'   implementation and analysis. In M. Schulte-Mecklenbeck, A. Kühberger, & J.
#'   G. Johnson (Eds.), \emph{A Handbook of Process Tracing Methods} (pp.
#'   111-130). New York, NY: Routledge.
#' 
#' @examples
#' mt_diffmap(
#'   KH2017, condition="Condition",
#'   xres=400, smooth_radius=6, n_shades=5
#' )
#' 
#' @export
mt_diffmap <- function(
  x,
  y = NULL,
  condition = NULL,
  use = 'trajectories',
  dimensions = c('xpos','ypos'),
  use2 = 'data',
  filename = NULL,
  bounds = NULL,
  xres = 500,
  upscale = 4,
  smooth_radius = 10,
  colors = c("#00863F", "#000000", "#FF1900"),
  n_shades = 1000,
  plot = TRUE,
  ...,
  verbose = TRUE
) {
  
  # --------- Take time
  t_start = proc.time()[3]
  
  # --------- collect device
  if(!is.null(filename)){
    device = strsplit(filename,'[.]')[[1]]
    device = device[length(device)]
    if(!device %in% c('pdf','png','tiff')){
      stop('filename != NULL requires one of .pdf, .png, or .pdf as file extension.')
    }
  } else {
    device = 'none'
  }
  
  # --------- collect heatmaps
  agg_x = NULL; agg_y = NULL
  if(is.null(y) & is.null(condition)) stop('Either y or condition must be specified.')
  if((is_mousetrap_data(x)| is.array(x)) &
     ((is_mousetrap_data(y) | is.array(y)) | !is.null(condition))){
    if(is.array(x)) if(!all(dimensions %in% dimnames(x)[[3]])) stop('Not all dimensions found in x.')
    if(!is.null(y) == !is.null(condition)) stop('Specify either y or condition, but not both.')
    if(!is.null(condition)){
      
      # If condition label is provided, extract condition values
      if(length(condition)==1){
        condition <- x[[use2]][,condition]
      }
      
      # If condition values are not of class logical, convert them
      if(inherits(condition,"logical") == FALSE){
        condition_levels <- levels(factor(condition))
        if (length(condition_levels)!=2) stop('The condition variable can only have two levels.')
        condition <- condition==condition_levels[1]
      }
      
      y = x[[use]][!condition,,] 
      x = x[[use]][ condition,,] 
    }
  } else if(inherits(x,'mt_heatmap_raw')) {
    if(!is.null(class(y))) stop('y must must be specified, if x is a heatmap object.')
    if(inherits(y,'mt_heatmap_raw')==FALSE) stop('y must match class of x.')
  }
  
  # --------- extract data
  x = extract_data(x, use = use)
  y = extract_data(y, use = use)
  
  # --------- determine bounds
  if(is.null(bounds)){
    if(verbose == TRUE) cat('Determine joint bounds','\n')
    range_x1 = range(x[,,dimensions[1]],na.rm=TRUE)
    range_x2 = range(x[,,dimensions[2]],na.rm=TRUE)
    range_y1 = range(y[,,dimensions[1]],na.rm=TRUE)
    range_y2 = range(y[,,dimensions[2]],na.rm=TRUE)
    range_1  = c(c(range_x1[1],range_y1[1])[which.min(c(range_x1[1],range_y1[1]))],
                 c(range_x1[2],range_y1[2])[which.max(c(range_x1[2],range_y1[2]))])
    range_2  = c(c(range_x2[1],range_y2[1])[which.min(c(range_x2[1],range_y2[1]))],
                 c(range_x2[2],range_y2[2])[which.max(c(range_x2[2],range_y2[2]))])
    mid_1   = range_1[1] + diff(range_1) / 2
    mid_2   = range_2[1] + diff(range_2) / 2
    range_x = mid_1 + (range_1 - mid_1) * 1.02
    range_y = mid_2 + (range_2 - mid_2) * 1.02
    bounds = c(range_x[1],range_y[1],range_x[2],range_y[2])
  }
  
  # --------- calculate image
  if(verbose == TRUE) cat('Calculating heatmap for x','\n')
  img_x = mt_heatmap_raw(x, dimensions = dimensions, bounds = bounds, xres = xres, smooth_radius =  0, aggregate_lwd = 0, verbose = FALSE,...)
  agg_x = img_x$agg ; img_x = img_x$img
  if(verbose == TRUE) cat('Calculating heatmap for y','\n')
  img_y = mt_heatmap_raw(y, dimensions = dimensions, bounds = bounds, xres = xres, smooth_radius =  0, aggregate_lwd = 0, verbose = FALSE,...)
  agg_y = img_y$agg ; img_y = img_y$img
  
  # get bg
  bg = img_x$colors[1]
  
  # --------- compute difference
  img = img_x
  img$img = img_x$img - img_y$img
  
  # --------- smooth image
  if(verbose == TRUE) cat('smooth image','\n')
  if (smooth_radius > 0) img$img = gaussBlur(
    img$img, img$img,
    max(img$x), max(img$y),
    smooth_radius
  )
  
  # --------- normalize
  v = img$img
  v = v / max(abs(v))
  
  if (n_shades > 0) {
    vp = v[v > 0]
    vm = v[v <= 0]
    vp = group(vp, n_shades)
    vm = group(vm, n_shades)
    v[v > 0]  = vp
    v[v <= 0] = vm
  }
  
  # set colors
  col1 = colormixer(colors[2], colors[1], abs(v), 'hex')
  col2 = colormixer(colors[2], colors[3], abs(v), 'hex')
  img$col = ifelse(v > 0, col1, col2)
  img$img = v
  
  # Plot image (if desired)
  cat('creating heatmap: ', max(img$x), 'x', max(img$y), 'px', '\n')
  if (plot == TRUE) {
    if (device == 'pdf') {
      grDevices::pdf(
        filename,
        width=10 * (max(img$x) / max(img$y)) * upscale,
        height=10 * upscale,
        bg = bg
      )
    } else if(device == 'png') {
      grDevices::png(
        filename,
        width=max(img$x) * upscale,
        height=max(img$y) * upscale,
        bg = colors[2]
      )
    } else if (device == 'tiff') {
      grDevices::tiff(
        filename,
        width=max(img$x) * upscale,
        height=max(img$y) * upscale,
        bg = colors[2]
      )
    } 
    
    graphics::par(bg = colors[2])
    graphics::plot.new()
    graphics::par(mar=c(0, 0, 0, 0))
    graphics::plot.window(
      xlim=range(img$x) + c(-.5, .5),
      ylim=range(img$y) + c(-.5, .5)
    )
    graphics::par(usr = c(range(img$x) + c(-.5, .5),range(img$y) + c(-.5, .5)))
    
    
    # plot points
    graphics::rect(
      img$x - .5, img$y - .5,
      img$x + .5, img$y + .5,
      col=img$col, border=NA
    )
  }
  
  # plot agg_x
  if (!is.null(agg_x)) {
    graphics::points(agg_x[1:2],cex=agg_x$lwd,col=agg_x$col,pch=16)
  }
  # plot agg_y
  if (!is.null(agg_y)) {
    graphics::points(agg_y[1:2],cex=agg_y$lwd,col=agg_y$col,pch=16)
  }
  
  # create output -----------------------------------------------------------
  heatmap = list('img' = img[,c('x', 'y', 'img', 'col')], 'agg' = NULL, 'colors' = colors)
  class(heatmap) = 'mt_heatmap_raw'
  
  if (device %in% c('pdf','png','tiff') & plot == TRUE) grDevices::dev.off()
  if (plot == FALSE) return(heatmap)
  
  if (verbose == TRUE) {
    t_end = proc.time()[3] - t_start
    cat('heatmap created in ', round(t_end), 's\n', sep='')
  }
}
