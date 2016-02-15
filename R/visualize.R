#' Plot trajectory data.
#' 
#' \code{mt_plot} can be used for plotting a number of individual trajectories. 
#' \code{mt_plot_aggregate} can be used for plotting aggregated trajectories. 
#' The color and linetype can be varied depending on a set of condition 
#' variables using the \code{color} and \code{linetype} arguments. If the 
#' \code{x} and \code{y} arguments are varied, this function can also be used 
#' for plotting velocity and acceleration profiles.
#' 
#' \code{mt_plot} internally uses \link{mt_reshape} for reshaping trajectories 
#' into a long format. Next, it creates a ggplot object using the 
#' \link[ggplot2]{ggplot} function of the \code{ggplot2} package. The
#' \link[ggplot2]{aes} mappings are taken from the function arguments for x, y
#' etc.; in addition, the group mapping is set to \link{mt_id}. If
#' \code{return_ggplot=FALSE}, the trajectories are plotted using the
#' \link[ggplot2]{geom_path} function of the \code{ggplot2} package.
#' 
#' \code{mt_plot_aggregate} works similarly, but uses \link{mt_aggregate} for 
#' reshaping and aggregating trajectories prior to plotting.
#' 
#' Please note that this function is intended as a quick and easy solution for 
#' visualizing mouse trajectories. For additional flexibility, we recommend that
#' \link{mt_reshape} or \link{mt_aggregate} be used in conjunction with 
#' \link[ggplot2]{ggplot} to create custom visualizations.
#' 
#' @param data a mousetrap data object created using one of the mt_import 
#'   functions (e.g., \link{mt_import_mousetrap}).
#' @param use a character string specifying which trajectories should be 
#'   plotted. The corresponding trajectories are selected from data using 
#'   \code{data[[use]]}. Usually, this value corresponds to either 
#'   "trajectories", "tn_trajectories" or "av_trajectories", depending on 
#'   whether the raw, time-normalized or averaged trajectories should be 
#'   plotted.
#' @param use2 a character string specifying where the data that contain the 
#'   variables used for determining the \code{color} and \code{linetype} can be 
#'   found (in case these arguments are specified). Defaults to "data" as 
#'   \code{data[["data"]]} usually contains all non mouse-tracking trial data.
#' @param x a character string specifying which dimension in the trajectory 
#'   array should be displayed on the x-axis (defaults to xpos).
#' @param y a character string specifying which dimension in the trajectory 
#'   array should be displayed on the y-axis (defaults to ypos).
#' @param color an optional character string specifying which variable in 
#'   \code{data[[use2]]} should be used for coloring the trajectories.
#' @param linetype an optional character string specifying which variable in 
#'   \code{data[[use2]]} should be used for varying the linetype of the 
#'   trajectories.
#' @param only_ggplot logical. If \code{TRUE}, only the ggplot object is 
#'   returned. If \code{FALSE}(the default), the trajectories are plotted using 
#'   \link[ggplot2]{geom_path}.
#' @param subject_id a character string specifying which column contains the 
#'   subject identifier. Only relevant for \code{mt_plot_aggregate}. If 
#'   specified, aggregation will be performed within subjects first. Note that 
#'   aggregation will be performed separately for each level, including all 
#'   subjects for whom data are available.
#' @param ... additional arguments passed on to \link{mt_reshape} (such as 
#'   \code{subset}).
#'   
#' @seealso \link{mt_plot_add_rect} for adding rectangles representing the
#' response buttons to the plot.
#' 
#' \link{mt_plot_per_trajectory} for individually plotting all trajectories as
#' individual pdf files.
#' 
#' @examples
#' # Load ggplot2
#' library(ggplot2)
#' 
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example,
#'   save_as="tn_trajectories", nsteps=101)
#'   
#' # Plot all time-normalized trajectories
#' # varying the color depending on the condition
#' mt_plot(mt_example, use="tn_trajectories",
#'   x="xpos", y="ypos", color="Condition")
#'         
#' # ... with custom colors
#' mt_plot(mt_example, use="tn_trajectories",
#'   x="xpos", y="ypos", color="Condition") +
#'   scale_color_brewer(type="qual")
#'    
#' # Plot aggregated time-normalized trajectories per condition
#' mt_plot_aggregate(mt_example, use="tn_trajectories",
#'   x="xpos", y="ypos", color="Condition")
#'                   
#' # ... and first aggregate trajectories within subjects
#' mt_plot_aggregate(mt_example, use="tn_trajectories",
#'   x="xpos", y="ypos", color="Condition",
#'   subject_id="subject_nr")
#'         
#' # Plot velocity profiles of subject 1
#' # based on the averaged trajectories
#' # varying the color depending on the condition
#' mt_example <- mt_calculate_derivatives(mt_example)
#' mt_example <- mt_average(mt_example)
#' mt_plot(mt_example, use="av_trajectories",
#'   x="timestamps", y="vel", color="Condition")
#' 
#' @describeIn mt_plot Plot individual trajectory data
#' @export
mt_plot <- function(data,
  use="trajectories", use2="data",
  x="xpos", y="ypos",
  color=NULL, linetype=NULL, 
  only_ggplot=FALSE, ...) {
  
  # Extract plotting options from metadata
  use2_variables <- c(color, linetype)
  if (is.null(use2_variables) == FALSE) {
    for (var in use2_variables) {
      data[[use2]][,var] <- factor(data[[use2]][,var])
    }
  }
  
  # Merge tracking data with metadata
  # and reshape it into long format
  trajectories <- mt_reshape(data=data,
    use=use, use2=use2, use2_variables=use2_variables,
    aggregate=FALSE, ...)
  
  # Build plot
  current_plot <- ggplot2::ggplot(
    trajectories,
    ggplot2::aes_string(
      x=x, y=y,
      group=mt_id,
      color=color,
      linetype=linetype
      )
    )
  
  if (only_ggplot == TRUE) {
    # Return empty plot object
    return(current_plot)
  } else {
    # Add path geom to plot
    return(current_plot+ggplot2::geom_path())
  }
  
}


#' @describeIn mt_plot Plot aggregated trajectory data
#' @export
mt_plot_aggregate <- function(data,
  use="trajectories", use2="data",
  x="xpos", y="ypos", color=NULL, linetype=NULL,
  only_ggplot=FALSE, subject_id=NULL, ...) {

  # Extract plotting options from metadata
  use2_variables <- c(color, linetype)
  if (is.null(use2_variables) == FALSE){
    for (var in use2_variables){
      data[[use2]][,var] <- factor(data[[use2]][,var])
    }
  }
  
  # Merge tracking data with metadata,aggregate it
  # and and reshape it into long format
  trajectories <- mt_aggregate(data=data,
    use=use, use2=use2, use2_variables=use2_variables,
    subject_id=subject_id, ...
  )
  
  # Build plot
  current_plot <- ggplot2::ggplot(
    trajectories,
    ggplot2::aes_string(
      x=x, y=y,
      color=color, linetype=linetype
    )
  )
  
  if (only_ggplot == TRUE) {
    # Return empty plot object
    return(current_plot)
  } else {
    # Add path geom to plot
    return(current_plot + ggplot2::geom_path())
  }
  
}

#' Add rectangles to trajectory plot.
#' 
#' \code{mt_plot_add_rect} adds one or several rectangles to a mousetrap plot. 
#' These buttons usually correspond to the borders of the buttons in the 
#' mouse-tracking experiment. It is specifically designed so that the arguments
#' from the \code{mousetrap_response} plug-in in OpenSesame can be used.
#' 
#' \code{mt_plot_add_rect} internally uses \link[ggplot2]{geom_rect} of the
#' \code{ggplot2} package for plotting.
#' 
#' @param rect a data.frame or matrix with one row per box. For each rectangle,
#'   the x-position (\code{x}), y-position (\code{y}), width (\code{w}), and
#'   height (\code{h}) needs to be provided. If columns are not labeled, the
#'   order \code{x, y, w, h} is assumed.
#' @param color argument passed on to \link[ggplot2]{geom_rect}. Specifies the
#'   color of the border of the rectangles.
#' @param fill argument passed on to \link[ggplot2]{geom_rect}. Specifies the
#'   color of the interior of the rectangles. If \code{NA} (the default),
#'   rectangles are unfilled.
#' @param ... additional arguments passed on to \link[ggplot2]{geom_rect}.
#' 
#' @seealso
#' \link{mt_plot} for plotting trajectory data.
#' 
#' @examples
#' # Load ggplot2
#' library(ggplot2)
#' 
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example,
#'   save_as="tn_trajectories", nsteps=101)
#' 
#' # Create rectangles matrix
#' rectangles <- matrix(
#'   # (The matrix is n x 4, and contains
#'   # all relevant data for every button, 
#'   # (i.e. x, y, width and height values) 
#'   # in separate rows)
#'   c(
#'     -840, 525, 280, -210,
#'     840, 525, -280, -210
#'   ),
#'   ncol=4, byrow=TRUE)
#'
#' # Plot all time-normalized trajectories
#' # varying the color depending on the condition
#' # and add rectangles
#' mt_plot(mt_example, 
#'   use="trajectories",
#'   x="xpos", y="ypos", color="Condition"
#' ) + mt_plot_add_rect(rect=rectangles)   
#'
#' 
#' @export
mt_plot_add_rect <- function(rect,
  color="black", fill=NA, ...) {
  
  # Add colnames to coordinate matrix
  if (is.null(colnames(rect))) {
    colnames(rect) <- c("x", "y", "w", "h")
  }
  
  # Convert coordinate matrix to data frame
  rect <- data.frame(rect)
  # Compute opposite corners
  rect$x2 <- rect$x+rect$w
  rect$y2 <- rect$y+rect$h
  
  # Return rect geom
  return(
    ggplot2::geom_rect(
      data=rect,
      ggplot2::aes_string(
        xmin="x", ymin="y", 
        xmax="x2", ymax="y2",
        x=NULL, y=NULL, 
        color=NULL, linetype=NULL
      ),
      color=color, fill=fill, ...
    )
  )
}

#' Create pdf with separate plots per trajectory.
#' 
#' \code{mt_plot_per_trajectory} creates a PDF file with separate plots per 
#' trajectory. This PDF can be used for inspecting individual trajectories. Note
#' that plotting all trajectories can be time-consuming, especially for raw 
#' trajectories. If the appropriate \code{x} and \code{y} arguments are
#' inserted, this function can also be used for plotting velocity and
#' acceleration profiles.
#' 
#' \code{mt_plot_per_trajectory} creates a pdf using \link{pdf}. Next, it plots 
#' all trajectories individually using \link{mt_plot}. Every plot is labeled 
#' using the \link{mt_id} variable.
#' 
#' @param data a mousetrap data object created using one of the mt_import
#'   functions (e.g., \link{mt_import_mousetrap}).
#' @param file a character string specifying the name of the pdf file. Passed on
#'   to \link{pdf}.
#' @param use a character string specifying which trajectories should be
#'   plotted. The corresponding trajectories are selected from data using
#'   \code{data[[use]]}. Usually, this value corresponds to either
#'   "trajectories", "tn_trajectories" or "av_trajectories", depending on
#'   whether the raw, time-normalized or averaged trajectories should be
#'   plotted.
#' @param x a character string specifying which dimension in the trajectory
#'   array should be displayed on the x-axis (defaults to xpos).
#' @param y a character string specifying which dimension in the trajectory
#'   array should be displayed on the y-axis (defaults to ypos).
#' @param show_progress logical indicating whether function should report its 
#'   progress.
#' @param ... additional arguments passed on to \link{pdf}.
#' 
#' @seealso
#' \link{mt_plot} for plotting trajectory data.
#' 
#' @examples
#' \dontrun{
#' mt_plot_per_trajectory(mt_example,
#'   file="trajectories.pdf",
#'   use="trajectories")
#' }
#' 
#' @export
mt_plot_per_trajectory <- function(data, file,
  use="trajectories", x="xpos", y="ypos",
  show_progress=TRUE, ...) {
  
  # Define axis limits across all plots
  xlim <- range(data[[use]][,x,], na.rm=TRUE)
  xoffset <- .05 * (xlim[2]-xlim[1])
  xlim[1] <- xlim[1] - xoffset
  xlim[2] <- xlim[2] + xoffset
  
  ylim <- range(data[[use]][,y,], na.rm=TRUE)
  yoffset <- .05 * (ylim[2]-ylim[1])
  ylim[1] <- ylim[1] - yoffset
  ylim[2] <- ylim[2] + yoffset
  
  # Create plots
  pdf(file, ...)
  
  # Plot each trajectory individually
  # (as a separate page in the PDF file)
  for (current_id in row.names(data[[use]])) {
    # Build plot
    current_plot <- mt_plot(
      data=data, use=use, x=x, y=y,
      use2=subset(data[["data"]], mt_id==current_id)
    )
    
    # Output plot
    print(
      current_plot +
      ggplot2::ggtitle(current_id) +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim)
    )
    
    # Display progress, if desired
    if (show_progress) {
      message("trajectory ", current_id, " plotted")
    }
  }
  
  # Close device
  dev.off()
}
