#' @importFrom magrittr %>%
magrittr::`%>%`

# Check that data is a mousetrap object
is_mousetrap_data <- function(data){

  return(class(data)=="mousetrap")

}


# Extract data from mousetrap object
extract_data <- function(data, use) {

  if (is_mousetrap_data(data)){

    extracted <- data[[use]]
    if(is.null(extracted)){
      stop("No data called '",use,"' found.")
    }
    return(extracted)

  } else if (is.array(data)){

    return(data)
    
  } else {
    
    stop("Data can either be of class mousetrap or array.")
    
  }

}

# Function to create results
create_results <- function(data, results, use, save_as, ids=NULL, overwrite=TRUE) {

  # Procedure for trajectories
  if (length(dim(results)) > 2) {
    if (is_mousetrap_data(data)) {
      data[[save_as]] <- results
      return(data)
    } else {
      return(results)
    }


  # Procedure for measures
  } else {
    results <- as.data.frame(results)

    # Extract / set ids
    if (is.null(ids)) {
      if (is.null(rownames(results))) {
        stop("No ids for create_results function provided.")
      } else {
        ids <- as.character(rownames(results))
      }
    } else {
      ids <- as.character(ids)
      rownames(results) <- ids
    }

    # Return results depending on type of data and overwrite setting
    if (is_mousetrap_data(data)){
      if (save_as %in% names(data) & overwrite == FALSE) {
        # check if columns already exist in data[[save_as]]
        if (any(colnames(results)%in%colnames(data[[save_as]]))){
          # if so, remove them and issue warning
          data[[save_as]] <- data[[save_as]][,colnames(data[[save_as]])[!colnames(data[[save_as]])%in%colnames(results)],drop=FALSE]
          warning("Columns of same name already exist and have been removed.")
        }
        # ensure id column is present
        data[[save_as]][,"mt_id"] <- as.character(rownames(data[[save_as]]))
        results[,"mt_id"] <- as.character(rownames(results))
        # merge by rownames
        data[[save_as]] <- dplyr::inner_join(data[[save_as]], results, by="mt_id")
        # set rownames again
        rownames(data[[save_as]]) <- data[[save_as]][,"mt_id"]
        # sort data
        data[[save_as]] <- data[[save_as]][ids,]
      } else {
        data[[save_as]] <- cbind(mt_id=ids, results)
      }
      # ensure rownames are characters
      data[[save_as]][,"mt_id"] <- as.character(data[[save_as]][,"mt_id"])
      return(data)
      
    } else {
      return(cbind(mt_id=ids, results))
    }
  }
}


# Function to determine the point on the line between P1 and P2
# that forms a line with P0 so that it is orthogonal to P1-P2.
# For details regarding the formula, see
# http://paulbourke.net/geometry/pointlineplane/
#
# The function expects P0 to be a matrix of points.
point_to_line <- function(P0, P1, P2){

  u <- ( (P0[,1]-P1[1]) * (P2[1]-P1[1]) +
           (P0[,2]-P1[2]) * (P2[2]-P1[2]) ) /
    ( (P2[1]-P1[1])^2 + (P2[2]-P1[2])^2 )

  P <- matrix(c(
    P1[1] + u * (P2[1] - P1[1]),
    P1[2] + u * (P2[2] - P1[2])),
    ncol = 2, byrow = FALSE
  )

  colnames(P) <- colnames(P0)

  return(P)
}


# Function to determine points on the straight line connecting the start and end
# points that result from an orthogonal projection of the individual points on
# the curve
points_on_ideal <- function(points, start=NULL, end=NULL) {

  # Fill start and end values if otherwise unspecified
  if (is.null(start)) {
    start <- points[1,]
  }
  if (is.null(end)) {
    end <- points[nrow(points),]
  }

  if (all(start == end)) {
    # If start and end points are identical,
    # no projection can be computed.
    # Therefore, we return the start/end point
    # as a fallback result.
    warning(
      "Start and end point identical in trajectory. ",
      "This might lead to strange results for some measures (e.g., MAD)."
    )
    result <- points
    result[,1] <- start[1]
    result[,2] <- start[2]
  } else {
    # compute the projection onto the idealized straight line for all points
    result <- point_to_line(P0 = points, P1 = start, P2 = end)
  }

  return(result)
}


# Function to calculate the number of flips
count_changes <- function(pos, threshold=0, zero_threshold=0) {

  # Calculate differences in positions between subsequent steps
  # (pos is a one-dimensional vector)
  changes <- diff(pos, lag=1)

  # Only keep logs with changes (above zero_threshold)
  changes <- changes[abs(changes) > zero_threshold]

  # Initialize variables
  cum_changes <- c() # vector of accumulated deltas
  cum_delta <- changes[1] # current accumulated delta

  # Iterate over the changes, and summarize
  # those with the same sign by generating the sum.
  # When the sign changes, a new value is added to
  # the cum_changes vector and the cumulative sum
  # of consecutive changes is reset.
  for (delta in changes[-1]) {

    # check if previous (accumulated) and current delta have the same sign
    if (sign(cum_delta) == sign(delta)) {

      # if so, accumulate deltas
      cum_delta <- delta + cum_delta
    } else {
      # if not, save accumulated delta
      cum_changes <- c(cum_changes, cum_delta)

      # reset cum_delta to current delta
      cum_delta <- delta
    }
  }

  # save last cum_delta
  cum_changes <- c(cum_changes, cum_delta)

  # Count changes in direction/sign

  # If there is no threshold, simply look at the number of the accumulated deltas
  if (threshold == 0) {
    n <- length(cum_changes) - 1

  } else {
    # If a threshold is set,
    # only keep changes above threshold
    cum_changes <- cum_changes[abs(cum_changes) > abs(threshold)]

    # Count changes in sign
    # (the diff converts changes in sign into +-1s or 0s for no changes,
    # and their absolute values are added up along the vector)
    n <- sum(abs(diff(cum_changes > 0)))
  }

  return(n)
}





#subsample
subsample = function(data, use = c('trajectories'), n, seed = 1){
  if(is.array(data) | is.data.frame(data) | is.matrix(data)){
    total_cases = dim(data)[1]
    if(n <= total_cases){
      set.seed(seed)
      select = sample(1:total_cases,n)
      set.seed(NULL)
      if(length(dim(data)) == 2){
        data = data[select,]
      }
      if(length(dim(data)) == 3){
        data = data[select,,]
      }
      if(length(dim(data)) == 1 | length(dim(data)) > 3){
        stop('Unexpected number of dimensions')
      }
    }
  }
  if(is.list(data)){
    if(mean(use %in% names(data)) != 1) stop('Objects specified in use do not exist')
    total_cases = dim(data[[use[1]]])[1]
    if(n <= total_cases){
      set.seed(seed)
      select = sample(1:total_cases,n)
      set.seed(NULL)
      for(i in use){
        if(is.array(data[[i]]) | is.data.frame(data[[i]]) | is.matrix(data[[i]])){
          if(length(dim(data[[i]])) == 2){
            data[[i]] = data[[i]][select,]
          }
          if(length(dim(data[[i]])) == 3){
            data[[i]] = data[[i]][select,,]
          }
        } else {
          stop('Objects in use cannot be subsetted')
        }
      }
    }
  }
  return(data)
}




# group

group = function(x,n,type = 'extreme'){
  minx = min(x)
  maxx = max(x)
  x = (x - minx) / (.0000001 + (maxx - minx))
  if(type == 'extreme') x = round((x * n) - .5) / (n - 1)
  if(type == 'mid') x = round((x * n) - .5) / n + (1/(2*n))
  x = x * (maxx - minx)
  x = x + minx
  return(x)
}


# colormixer

colormixer = function(col1,col2,weight,format = 'rgb'){
  
  if(ifelse(is.matrix(col1),nrow(col1),length(col1)) != length(weight) &
     length(col1) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
  }
  if(ifelse(is.matrix(col2),nrow(col2),length(col2)) != length(weight) &
     length(col2) != 1){
    stop('Length of col1 must be either 1 or matching the length of weight')
  }
  if(length(weight) == 1){
    if(ifelse(is.matrix(col1),nrow(col1),length(col1)) !=
       ifelse(is.matrix(col2),nrow(col2),length(col2))){
      stop('If length of weight = 1, number of colors in col1 and col2 must match')
    }
  }
  
  nrows = max(c(ifelse(is.matrix(col1),nrow(col1),length(col1)),
                ifelse(is.matrix(col2),nrow(col2),length(col2)),
                length(weight)))
  
  if(is.character(col1)){
    if(length(col1) == 1){
      col1 = grDevices::col2rgb(col1)
      col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=T)
    } else {
      col1 = t(sapply(col1,grDevices::col2rgb))
    }
  } else{
    col1 = matrix(c(col1),ncol=3,nrow=nrows,byrow=F)
  }
  if(is.character(col2)){
    if(length(col2) == 1){
      col2 = grDevices::col2rgb(col2)
      col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=T)
    } else {
      col2 = t(sapply(col2,grDevices::col2rgb))
    }
  } else{
    col2 = matrix(c(col2),ncol=3,nrow=nrows,byrow=F)
  }
  
  
  col = col1 * (1-weight) + col2 * weight
  
  if(format == 'rgb') return(col)
  if(format == 'hex') return(grDevices::rgb(data.frame(col),maxColorValue = 255))
  if(!format %in% c('rgb','hex')) stop('Choose either "rgb" or "hex" as format')
  
}


# round even
round_even = function(x){
  rx = round(x)
  test = rx %% 2
  ifelse(test == 0,rx,
         ifelse( x < 0,
                 ifelse(x <  rx,floor(x),ceiling(x)),
                 ifelse(x >= rx,ceiling(x),floor(x))))
}


# Cohens d
# Computed cohens d
cohen = function(x,y){
  
  nx = length(x)
  ny = length(y)
  vx = var(x)
  vy = var(y)
  
  nom = mean(x) - mean(y)
  den = sqrt(((nx-1)*vx + (ny-1)*vy)/(nx+ny-2))
  coh = nom / den
  
  v_d = sqrt((nx+ny)/(nx*ny) + (coh*coh) / (2 * (nx + ny)))
  
  rx = vx/nx
  ry = vy/ny
  nu = (rx + ry)**2 / ((rx ** 2 / (rx - 1) + (ry ** 2 / (ry - 1))))
             
  return(c(coh,v_d,nu))
  }


