#' Create gif trajectory animation.
#' 
#' \code{mt_animate} animates trajectories using the animation package. Note
#' that this function has beta status.
#' 
#' \code{mt_animate} produces a .gif file showing a continuous stream of 
#' animated trajectories. The function first produces a series of \emph{.png} 
#' images, which then are combined into a \emph{.gif} animation using 
#' \emph{ImageMagick} (see \url{https://www.imagemagick.org/}).
#' 
#' In order to run this function, ImageMagick must be installed (download from 
#' \url{https://www.imagemagick.org/}). Under Unix systems (Linux and Apple's
#' OSX) the function will look for ImageMagick using its default installation
#' path. Alternatively, the location of ImageMagick's \emph{convert} function
#' can be provided using the \code{im_path} argument. Under Windows, 
#' \code{im_path} must always be specified explicitly (e.g., it might look
#' something like this \code{im_path = "C:/Program
#' Files/ImageMagick-7.0.5-Q16/convert.exe"}).
#' 
#' During the animation trajectories are sampled from the data without
#' replacement. The function stops when it reaches the last trajectory contained
#' in \code{data}.
#' 
#' By default, \code{mt_animate} animates trajectories using the original 
#' timestamps. Timestamps are expected to be expressed in milliseconds. By
#' setting \code{timestamps = NULL}, the function can also assume timestamps to
#' be regualar, i.e., of constant interval, in this case the longest duration is
#' set to exactly one second.
#' 
#' In order to create high-resolution (large) animations in a relatively short
#' time increase \code{upscale} in favor of \code{xres}. However, note that this
#' will decrease the sharpness of the image.
#' 
#' In order to increase or decrease the overall color intensity decrease or
#' increase the \code{max_intensity}, respectively.
#' 
#' 
#' @inheritParams mt_time_normalize
#' @param dimensions a character vector specifying the two dimensions in the 
#'   trajectory array that contain the mouse positions. Usually (and by 
#'   default), the first value in the vector corresponds to the x-positions 
#'   (\code{xpos}) and the second to the y-positions (\code{ypos}).
#' @param timestamps a character string specifying the trajectory dimension 
#'   containing the timestamps. If \code{NULL} linearly increasing timestamps 
#'   are assumed, producing a perfectly constant timestamp interval.
#' @param filename character string specifying the path and filename of the 
#'   resulting \emph{.gif}. If the extension of \code{filename} is not
#'   \emph{.gif}, \emph{.gif} is added at the end. Must not contain spaces.
#' @param xres numeric specifying the resolution of the \emph{.gif} file.
#' @param seconds numeric specifying the duration of the \emph{.gif} file.
#' @param framerate numeric specifying the framerate of the \emph{.gif} file. 
#'   Defaults to 24 implying smooth non-discrete frame transitions for the human
#'   eye.
#' @param speed numeric specifying the speed of the trajectories with regard to 
#'   their original velocity profile. I.e., a value of .5 shows trajectories in 
#'   half of the original velocities, whereas a value of 2 shows trajectories in
#'   double of the original velocities.
#' @param density integer specifiying the number of trajectories to be added 
#'   each frame. I.e., if \code{density = 10}, \code{seconds = 10}, 
#'   \code{framerate = 24} and \code{speed = .5} then the animation will show 10
#'   x 10 x 24 x .5 = 1200 trajectories.
#' @param jitter logical specifying whether the density should be jittered. If 
#'   \code{TRUE}, \code{density} varies according to
#'   \link[stats]{rgeom}(\code{1/density}).
#' @param remove logical specifying whether trajectories that reached their end 
#'   points should be removed from the rest of the animation. Defaults to 
#'   \code{FALSE} implying that all finished trajectories remain visible.
#' @param bg character string specifying the background color.
#' @param col character string specifiyng the foreground color, i.e., the color 
#'   used to draw the trajectories.
#' @param lwd numeric specifying the line width of the trajectories.
#' @param loop logical specifying whether gif should be looped. If \code{FALSE}
#'   (the default), the last frame will remain visible after the animation is
#'   finished. If \code{TRUE}, the gif will infinitely repeat itself.
#' @param bounds numeric vector specifying the xleft, xright, ybottom, and ytop 
#'   limits of the animation canvas. Defaults to \code{NULL} in which case the 
#'   animation canvas is set to include all existing trajectory points,
#'   irrespective of how extreme they may be.
#' @param norm logical specifying wether the trajectories should be remapped to
#'   the \emph{mt-space}. See \link{mt_align}. Note that aligning often requires
#'   that that all trajectories are flipped to one side first (see
#'   \link{mt_remap_symmetric}).
#' @param upscale numeric specifying a scaling factor for the animation
#'   resolution. E.g, \code{upscale = 2} implies that the x-resolution in
#'   \emph{.gif} file is \code{2*xres}.
#' @param decay numeric defining a within-trajectory gradient of color intensity.
#'   Specifically, values larger than 1 will give more recent movements higher 
#'   color intensities than movements that lie longer in the past, and vice
#'   versa.
#' @param max_intensity numeric specifying the maximum color intensity. A value
#'   of, e.g., 5, implies that color intensity is limited to 5 overlapping
#'   trajectories. I.e., a point at which 4 trajectories overlap will in that
#'   case have a smaller color intensity than a point at which 5 trajectories
#'   overlap, but there will be no difference between the latter and a point at
#'   which 6 trajectories overlap. If \code{decay} is unequal 1, this metric
#'   refers to the most intense color point within the trajectory.
#' @param discard_images logical specifying whether the temporary folder
#'   containing the temporary \emph{.png} images should be deleted. Defaults to
#'   TRUE.
#' @param im_path character string specifying the location of ImageMagick's 
#'   \emph{convert} function. If \code{NULL}, the \emph{convert} function is 
#'   expected in \code{'/usr/local/bin/convert'}, the default location for Linux
#'   and OSX operating systems. The location has to be specified explicitly for
#'   Windows (see Details and Examples).
#' @param parallel logical specifying whether the temporary \emph{.png} images
#'   should be created using parallel processing (uses
#'   \link[parallel]{clusterApplyLB}). Process will be run on the maximum
#'   number of available cores (as determined by \link[parallel]{detectCores}).
#' @param verbose logical indicating whether function should report its 
#'   progress.
#'   
#' @return NULL
#'   
#' @examples
#' \dontrun{
#' # Preprocess trajectory data
#' mt_example <- mt_align_start(mt_example)
#' mt_example <- mt_remap_symmetric(mt_example) 
#' 
#' # Create animated trajectory gif
#' # (under Linux / OSX)
#' mt_animate(mt_example,filename = "MyMovie.gif")
#' 
#' # Increase duration and density while decreasing speed
#' mt_animate(mt_example, filename = "MyMovie2.gif",
#'   seconds = 10, speed = .3, density = 10)
#' 
#' # Create animated trajectory gif
#' # (under Windows - ImageMagick version specific example)
#' mt_animate(mt_example,filename = "MyMovie.gif",
#'   im_path = "C:/Program Files/ImageMagick-7.0.5-Q16/convert.exe")
#' 
#' }
#' 
#' @author Dirk U. Wulff <dirk.wulff@gmail.com>
#' 
#' @export

mt_animate = function(
  
  # data
  data,
  use = 'trajectories',
  dimensions = c('xpos', 'ypos'),
  timestamps = 'timestamps',
  
  # plot arguments
  filename = 'trajectory_animation.gif',
  xres      = 1000,
  seconds   = 3,
  framerate = 24,
  speed     = .5,
  density   = 3,
  jitter    = TRUE,
  remove    = FALSE,
  bg        = 'black',
  col       = 'white',
  lwd       = 1,
  
  # background settings
  loop           = FALSE,
  bounds         = NULL,
  norm           = FALSE,
  upscale        = 1,
  decay          = 10,
  max_intensity  = 5,
  discard_images = TRUE,
  im_path        = NULL,
  parallel       = TRUE,
  verbose        = FALSE){
  
  # constants
  edge  = 1
  chars = c(0:9,letters,LETTERS)
  
  # Data checks
  if (!length(dimensions) %in% c(2,3)) {
    stop('Dimensions must of length 2 or 3!')
  }
  
  # extract trajectories  
  trajectories = extract_data(data,use)
  
  # check if all dimensions are present
  if (!all(dimensions %in% dimnames(trajectories)[[3]])) {
    stop('Not all dimensions exist in data')
  }
  
  # add uniform timestamps if null or not found
  if(is.null(timestamps)){
    
    # setup uniforms timestamps
    timestamps = matrix(rep(seq(0,1000,length = ncol(trajectories)),nrow(trajectories)),
                        ncol = ncol(trajectories),
                        byrow = T)
    timestamps[is.na(trajectories[,,dimensions[1]])] = NA
    
    # strip trajectory to dimensions
    trajectories = trajectories[,,dimensions]
    
    # add new timestamps
    trajectories = mt_add_variables(trajectories,variables = list('timestamps'=timestamps))
    timestamps = 'timestamps'
    
  } else {
    if(!timestamps %in% dimnames(trajectories)[[3]]){
      stop('timestamps must be in dimnames or NULL.')
    }
  }
  
  # check existence of imagemagick
  if(is.null(im_path)){
    if(.Platform$OS.type == 'unix'){
      im_path = '/usr/local/bin/convert'
      if(!file.exists(im_path)) stop('Could not find imagemagick. Install imagemagick (if you have not) or explicitly specify im_path.')
    } else {
      stop('Under Windows please explicitly specify im_path.')
    }
  } else {
    if(!file.exists(im_path)) stop('Could not find imagemagick. Check if im_path is correct.')
  }
  
  # Determine Dimensions ----------------------------------------------------
  # Rescale trajectories so that there are sufficient points to fill
  # the diagonal, i.e, length of diagonal divided by px
  
  if (verbose == TRUE) cat('preprocess trajectories','\n')
  
  if(is.null(bounds) & norm == FALSE){
    range_x = range(trajectories[,,dimensions[1]],na.rm=T)
    range_y = range(trajectories[,,dimensions[2]],na.rm=T)
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
  margin  = ceiling(edge * 2) + ceiling(edge * 2)
  xres_m  = xres - margin - 1
  px_size = diff(range_x) / xres_m
  
  # Determine number of resc
  xres_m  = xres_m
  yres_m  = ceiling(xres_m * diff(range_y) / diff(range_x))
  n_resc  = ceiling(sqrt(xres_m * xres_m + yres_m * yres_m))
  l_diag  = sqrt(diff(range_x)**2 + diff(range_y)**2)
  
  
  # Determine Dimensions ----------------------------------------------------
  
  # Determine number of resc
  lengths  = getLengths(
    trajectories[,,dimensions[1]],
    trajectories[,,dimensions[2]]
  )
  
  # determine number of poinst per trajectory
  n_points = round(n_resc * (lengths / l_diag))
  n_points = n_points
  
  # blow up trajectories
  spatialized_trajectories = mt_spatialize_tolong(
    trajectories,
    dimensions = c(dimensions,timestamps),
    n_points = n_points
  )
  colnames(spatialized_trajectories) = c(dimensions,timestamps)
  
  # range of pixels plus white space around image
  xs = 0 : (xres_m + margin) + 1
  ys = 0 : (yres_m + ceiling(edge * 2) + ceiling(edge * 2)) + 1
  
  # retrieve image
  pts = spatialized_trajectories
  
  # Remove points outside of bounds
  pts = pts[pts[,1] >= bounds[1] &
              pts[,1] <= bounds[3] &
              pts[,2] >= bounds[2] &
              pts[,2] <= bounds[4],]
  
  
  # Determine pixel locations
  pts[,1]  = round(((pts[,1] - bounds[1]) / px_size) + 1) + ceiling(edge * 2)
  pts[,2]  = round(((pts[,2] - bounds[2]) / px_size) + 1) + ceiling(edge * 2)
  
  # split trajectories
  
  split_by = rep(1:nrow(trajectories),n_points)
  trajectory_list = split(data.frame(pts),split_by)
  
  # extract maximum timestamp
  max_time = sapply(trajectory_list,function(x) max(x[,timestamps]))
  if(length(dimensions) == 3) max_dim3 = sapply(trajectory_list,function(x) max(x[,dimensions[3]]))
  
  
  # Determine Jobs ----------------------------------------------------
  
  # determine number of frames
  n_frames = seconds * framerate
  
  # determine indices of active trajectories
  # init
  all_is = 1:length(trajectory_list)
  jobs = list()
  is = 1
  spawns = 0
  
  # loop
  for(i in 1:n_frames){
    
    # identify time
    time = ((floor(i / framerate) + (i %% framerate / framerate)) * 1000) / (1/speed)
    
    # test if trajectories remain
    n_left = length(all_is)
    if(n_left == 0 & all(max_time[is] <= time - spawns)) break
    
    if(i == 1){
      
      # add selected trajectories to active
      is = sample(all_is,density)
      
      # set spawn time
      spawns = rep(0,density)
      
      # store job
      jobs[[i]] = list(i,is,spawns)
      
    } else {
      
      if(n_left > 0){ 
        
        # remove is chosen
        rem = length(trajectory_list) + 1
        if(remove == T & length(is) > 0){
          for(j in 1:length(is)){
            indx = is[j]
            test = max_time[indx] <= time - spawns[j]
            if(test == T) rem = c(rem,j)
          }
        }
        
        # add new trajectories according to pgeom
        if(jitter == TRUE) {
          if(density != 1){
            nnew = min(stats::rgeom(1,1/density),n_left)
          } else {
            nnew = 1
          }
        } else {
          nnew = density  
        }
        if(nnew > 0){
          is = c(is[-rem],sample(all_is,nnew))
          spawns = c(spawns[-rem],rep(time,nnew))
        } else {
          is = c(is[-rem])
          spawns = c(spawns[-rem])
        }
        
      } 
        
      # store job
      jobs[[i]] = list(i,is,spawns)
      
    }  
      
    # remove selected trajectories from trajectory_list
    all_is = all_is[!all_is %in% is]
    
  }
  
  
  # Define plot function ----------------------------------------------------
  
  plot_frame = function(jobs, trajectory_list){
    
    for(i in 1:length(jobs)){
      
      # extract job
      job = jobs[[i]]
      
      # ------ setup or add to list of active trajectories
      
      # add selected trajectories to active
      active = trajectory_list[job[[2]]]
      
      # set spawn time
      spawn_time = job[[3]]
      
      
      # ----- setup image 
      
      # reset image
      img_mat = matrix(0, ncol=max(xs), nrow=max(ys))
      if(length(dimensions) == 3) a_mat = matrix(0, ncol=max(xs), nrow=max(ys))
      
      # identify time
      time = ((floor(job[[1]] / framerate) + (job[[1]] %% framerate / framerate)) * 1000) / (1/speed)
      
      # loop over active trajectories 
      rem = c()
      for(j in 1:length(active)){
        
        # extract trajectory
        traj = active[[j]]
        
        # limit to points in time
        subs = traj[,'timestamps'] <= time - spawn_time[j]
        traj = traj[subs,]
        
        # add trajetcory piece to image
        img_mat[as.matrix(traj[,2:1])] = img_mat[as.matrix(traj[,2:1])] + seq(0,1,length = nrow(traj))**decay + 1
        if(length(dimensions) == 3) {
          a = traj[,dimensions[3]]
          a = ((1 + a - min(a)) / (1 + max_dim3[job[[2]][j]] - min(a)))
          a_mat[as.matrix(traj[,2:1])] = select_max(a_mat[as.matrix(traj[,2:1])],a)
        }
        
        # test if to be removed
        if(mean(subs) == 1) rem = c(rem,j)
        
      }
      
      # cut-off at max intensity
      img_mat[img_mat > max_intensity] = max_intensity
      
      # normalize to 0 to 1  
      img_mat = img_mat / max_intensity
      
      # transformt to long
      img = data.frame(expand.grid(xs,ys),c(t(img_mat)),1)
      if(length(dimensions) == 3) img[,4] = c(t(a_mat))
      names(img) = c('x','y','col','lwd')
      
      # reduce to non-zero elements
      img = img[img[,3] > 0,]
      
      
      # ----- plot
      
      # create png
      image_no = paste0(paste(rep(0,5 - nchar(job[[1]])),collapse=''),job[[1]])
      grDevices::png(paste0(tmp_path,'/',image_no,'.png'),
          width = max(xs) * upscale,
          height = max(ys) * upscale,
          bg = bg)
      
      graphics::plot.new()
      graphics::par( mar=c(0, 0, 0, 0) )
      graphics::plot.window(xlim=range(xs) + c(-.5, .5),
                  ylim=range(ys) + c(-.5, .5)
      )
      graphics::par(usr = c(range(xs) + c(-.5, .5),range(ys) + c(-.5, .5)))
      
      # plot points
      graphics::rect(
        img$x - .5 * img$lwd * lwd, img$y - .5 * img$lwd * lwd,
        img$x + .5 * img$lwd * lwd, img$y + .5 * img$lwd * lwd,
        col = colormixer(col, bg, 1-img$col,format = 'hex'),
        border = NA
      )
      
      grDevices::dev.off()
      
    }
  }
  
  # Execute plot ----------------------------------------------------
  
  # create tmp folder name
  tmp_char = paste0(sample(chars,5),collapse='')
  tmp_path = paste0(getwd(),'/',tmp_char)
  if(!dir.exists(tmp_path)) dir.create(tmp_path)
  
  # execute jobs in parallel or serial
  if(parallel == TRUE){
    
    # report
    if(verbose == TRUE) cat('Creating frames (in parallel)')
    
    # take the time
    t = proc.time()[3]

    # split jobs
    ncores = parallel::detectCores()
    jobs_split = split(sample(jobs),cut(1:length(jobs),ncores))  
    
    # call colormixer so that it is available
    colormixer = colormixer

    # generate individual frames as pngs
    # this operation is parallelized via the parallel package
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(cl, c(
      'framerate', 'speed', 'decay', 'max_intensity',
      'xs', 'ys', 'upscale', 'bg', 'colormixer',
      'tmp_path', 'filename'
    ))
    parallel::clusterApplyLB(cl, jobs_split, plot_frame, trajectory_list=trajectory_list)
    parallel::stopCluster(cl)

    # report time
    if(verbose == TRUE) cat('completed in',round(proc.time()[3] - t,2),'seconds','\n')
    
  } else {
    
    # report
    if(verbose == TRUE) cat('Creating frames (in serial)')
    
    # take the time
    t = proc.time()[3]
    
    # split jobs
    jobs_split = list(jobs)
    
    # serial execution
    for(i in 1:length(jobs_split)) plot_frame(jobs_split[[i]],trajectory_list)
    
    # report time
    if(verbose == TRUE) cat('completed in',round(proc.time()[3] - t,2),'seconds','\n')
    
  }
  
  
  
  # Make gif ----------------------------------------------------
  
  # report
  if(verbose == TRUE) cat('creating',filename,'\n')
  
  # taking the time
  t = proc.time()[3]
  
  # add .gif to filename if necessary
  if(substr(filename,nchar(filename)-3,nchar(filename)) != '.gif'){
    filename = paste0(filename,'.gif')
  } 

  # setup conversion command
  command = paste(
    paste0('"',im_path,'"'),
    ifelse(loop == TRUE,'-loop 0','-loop 1'),
    '-delay',1/framerate,
    paste0(tmp_char,'/*.png'),
    filename)

  # ff_path = '/usr/local/bin/ffmpeg'
  # command = paste(
  #   ff_path,
  #   '-i',    paste0(tmp_char,'/*.png'),
  #   filename)    
  # 
  
  # convert
  system(command)

  # remove images 
  if(discard_images == TRUE) unlink(tmp_char,recursive = TRUE)
  
  # report
  if(verbose == TRUE) cat('\tcompleted in ', round((proc.time()[3] - t), 2), ' s\n', sep='')
  
}

