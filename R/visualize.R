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
#' etc.; in addition, the group mapping is set to the internal trial identifier
#' (by default called "mt_id").
#' 
#' If \code{return_type == "plot"} (the default), a new ggplot is created and
#' the trajectories are plotted using the \link[ggplot2]{geom_path} function of
#' the \code{ggplot2} package. If \code{return_type == "mapping"}, the ggplot
#' object is returned without layers, which can be used to further customize the
#' plot (see Examples). If \code{return_type == "geoms"}, only the geoms are
#' returned, which allows adding the plotted trajectories to an existing ggplot
#' (e.g., adding aggregate trajectories on top of the individual trajectories,
#' see Examples).
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
#'   functions (see \link{mt_example} for details). Alternatively, a trajectory
#'   array can be provided directly (in this case \code{use} will be ignored).
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
#' @param alpha an optional numeric value between 0 and 1 that can be used to
#'   make the plotted lines (and points) semitransparent.
#' @param size an optional numeric value that can be used to vary the width of
#'   the plotted trajectory lines.
#' @param facet_row an optional character string specifying a variable in 
#'   \code{data[[use2]]} that should be used for (row-wise) faceting.
#' @param facet_col an optional character string specifying a variable in 
#'   \code{data[[use2]]} that should be used for (column-wise) faceting.
#' @param wrap_var an optional character string specifying variable(s) in 
#'   \code{data[[use2]]} that should be used for wrapping.
#' @param wrap_ncol an optional integer specifying the number of columns if
#'   wrapping is used.
#' @param points logical. If \code{TRUE}, points will be added to the plot using
#'   \link[ggplot2]{geom_point}.
#' @param return_type a character string specifying which type of object should
#'   be returned. If \code{"plot"} (the default), a new ggplot is created and
#'   the trajectories are plotted using \link[ggplot2]{geom_path}. If
#'   \code{"mapping"}, only the ggplot object containing the mapping but without
#'   any geoms is returned. If \code{"geoms"}, only the geoms are returned,
#'   which allows adding the plotted trajectories to an existing ggplot.
#' @param mt_id a character string specifying the internal label used for the
#'   trial identifier (passed on to the group aesthetic). Only relevant for
#'   \code{mt_plot}.
#' @param subject_id a character string specifying which column contains the
#'   subject identifier. Only relevant for \code{mt_plot_aggregate}. If
#'   specified, aggregation will be performed within subjects first. Note that
#'   aggregation will be performed separately for each level, including all
#'   subjects for whom data are available.
#' @param only_ggplot Deprecated. Please use \code{return_type} instead.
#' @param ... additional arguments passed on to \link{mt_reshape} (such as
#'   \code{subset}).
#'
#' @seealso \link{mt_plot_add_rect} for adding rectangles representing the 
#'   response buttons to the plot.
#'
#'   \link{mt_plot_riverbed} for plotting the relative frequency of a selected 
#'   variable across time.
#'
#'   \link{mt_plot_per_trajectory} for individually plotting all trajectories as
#'   individual pdf files.
#'
#' @examples
#' ## Plot individual example trajectories
#'
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'
#' # Plot all time-normalized trajectories
#' # varying the color depending on the condition
#' mt_plot(mt_example, use="tn_trajectories",
#'   color="Condition")
#'
#' # ... setting alpha < 1 for semi-transparency
#' mt_plot(mt_example, use="tn_trajectories",
#'   color="Condition", alpha=.2)
#'
#' # ... with custom colors
#' mt_plot(mt_example, use="tn_trajectories",
#'   color="Condition") +
#'   ggplot2::scale_color_brewer(type="qual")
#' 
#' # Create separate plots per Condition
#' mt_plot(mt_example, use="tn_trajectories",
#'   facet_col="Condition")
#'
#' # Create customized plot by setting the return_type option to "mapping"
#' # to setup an empty plot. In a next step, a geom is added.
#' # In this example, only points are plotted.
#' mt_plot(mt_example, use="tn_trajectories",
#'   color="Condition", return_type="mapping") + 
#'   ggplot2::geom_point()
#' 
#' # Plot velocity profiles based on the averaged trajectories
#' # varying the color depending on the condition
#' mt_example <- mt_derivatives(mt_example)
#' mt_example <- mt_average(mt_example, interval_size=100)
#' mt_plot(mt_example, use="av_trajectories",
#'   x="timestamps", y="vel", color="Condition")
#'
#'
#' ## Plot aggregate trajectories for KH2017 data
#'
#' # Time-normalize trajectories
#' KH2017 <- mt_time_normalize(KH2017)
#'
#' # Plot aggregated time-normalized trajectories per condition
#' mt_plot_aggregate(KH2017, use="tn_trajectories",
#'   color="Condition")
#'
#' # ... first aggregating trajectories within subjects
#' mt_plot_aggregate(KH2017, use="tn_trajectories",
#'   color="Condition", subject_id="subject_nr")
#'
#' # ... adding points for each position to the plot
#' mt_plot_aggregate(KH2017, use="tn_trajectories",
#'   color="Condition", points=TRUE)
#'
#' \dontrun{
#'   
#' # Create combined plot of individual and aggregate trajectories
#' # by first plotting the individual trajectories using mt_plot.
#' # In a next step, the aggregate trajectories are added using the
#' # mt_plot_aggregate function with the return_type argument set to "geom".
#' mt_plot(KH2017, use="tn_trajectories", color="Condition", alpha=.05) + 
#'   mt_plot_aggregate(KH2017, use="tn_trajectories",
#'     color="Condition", return_type="geom", size=2)
#' }
#'
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#' 
#' @describeIn mt_plot Plot individual trajectory data
#' @export
mt_plot <- function(data,
  use="trajectories", use2="data",
  x="xpos", y="ypos",
  color=NULL, linetype=NULL,
  alpha = NA, size = 0.5,
  facet_row=NULL, facet_col=NULL,
  wrap_var=NULL, wrap_ncol=NULL,
  points=FALSE,
  return_type="plot",
  mt_id="mt_id",
  only_ggplot=NULL,
  ...) {
  
  if((!is.null(facet_row) | !is.null(facet_col))& !is.null(wrap_var)) {
    stop("wrap_var cannot be used in combination with facet_row and facet_col.")
  }
  
  if(!return_type %in% c("plot", "mapping", "geom")){
    stop("return_type can only be one of the following: plot, mapping, geom")
  }
  
  if(return_type == "geom"){
    if(!is.null(facet_row) | !is.null(facet_col) | !is.null(wrap_var)){
      stop("When return_type is geom, no facet or wrapping can be used")
    }
  }
  
  if (is.null(only_ggplot) == FALSE) {
    warning(
      "The argument only_ggplot is deprecated. ",
      "Please use return_type instead and consult the function documentation for its enhanced functionality.",
      call.=FALSE
    )
    if(only_ggplot==TRUE){
      return_type <- "mapping"
    } else{
      return_type <- "plot"
    }
  }

  # Extract plotting options from metadata
  use2_variables <- c(color, linetype, facet_row, facet_col, wrap_var)

  # Merge tracking data with metadata
  # and reshape it into long format
  trajectories <- mt_reshape(data=data,
    use=use, use2=use2, use2_variables=use2_variables,
    aggregate=FALSE,mt_id=mt_id, ...)
  
  # Ensure that variables are factors
  if (is.null(use2_variables) == FALSE) {
    for (var in use2_variables) {
      trajectories[,var] <- factor(trajectories[,var])
    }
  }
  
  # Setup basic mapping
  current_mapping <-
    ggplot2::aes_string(
      x=x, y=y,
      group=mt_id,
      color=color,
      linetype=linetype
    )

  # Build geom in case only geom should be returned
  if (return_type == "geom"){
    
    current_geom <- ggplot2::geom_path(
      mapping = current_mapping,
      data = trajectories,
      alpha = alpha, size = size,
      inherit.aes = FALSE
      )
    
    # ... and return this geom if no points are required
    if (points == FALSE) {
      return(current_geom)
      
    # ... and return list of geoms if points are required as well
    } else {
      return(
        list(
          current_geom,
          ggplot2::geom_point(
            mapping = current_mapping,
            data = trajectories,
            alpha = alpha,
            inherit.aes = FALSE
          )
        )
      ) 
    }
    
  # Otherwise, setup new ggplot
  } else{
    
    current_plot <- ggplot2::ggplot(
      data = trajectories,
      mapping = current_mapping
    )
    
    # Add facets (optional)
    if(!is.null(facet_row) | !is.null(facet_col)) {
      facet_row <- ifelse(is.null(facet_row),".",facet_row)
      facet_col <- ifelse(is.null(facet_col),".",facet_col)
      facet_formula <- stats::as.formula(paste(facet_row,facet_col,sep="~"))
      current_plot <- current_plot + ggplot2::facet_grid(facet_formula)
    }
    
    # Add wrapping (optional)
    if(is.null(wrap_var)==FALSE) {
      current_plot <- current_plot + ggplot2::facet_wrap(facets=wrap_var, ncol=wrap_ncol)
    }
    
    # Return empty plot object if only mapping should be returned
    if (return_type == "mapping") {
      return(current_plot)
    
    # Otherwise, add geoms to ggplot
    } else {
      
      current_plot <- current_plot +
        ggplot2::geom_path(alpha=alpha, size=size)
      
      if (points) {
        current_plot <- current_plot +
          ggplot2::geom_point(alpha=alpha)
      }
      
      return(current_plot)
      
    }
    
    
  }

}


#' @describeIn mt_plot Plot aggregated trajectory data
#' @export
mt_plot_aggregate <- function(data,
  use="trajectories", use2="data",
  x="xpos", y="ypos", color=NULL, linetype=NULL,
  alpha = NA, size = 0.5,
  facet_row=NULL, facet_col=NULL,
  wrap_var=NULL, wrap_ncol=NULL,
  points=FALSE,
  return_type="plot",
  subject_id=NULL,
  only_ggplot=NULL,
  ...) {
  
  if((!is.null(facet_row) | !is.null(facet_col))& !is.null(wrap_var)) {
    stop("wrap_var cannot be used in combination with facet_row and facet_col.")
  }
  
  if(!return_type %in% c("plot", "mapping", "geom")){
    stop("return_type can only be one of the following: plot, mapping, geom")
  }
  
  if(return_type == "geom"){
    if(!is.null(facet_row) | !is.null(facet_col) | !is.null(wrap_var)){
      stop("When return_type is geom, no facet or wrapping can be used")
    }
  }
  
  if (is.null(only_ggplot) == FALSE) {
    warning(
      "The argument only_ggplot is deprecated. ",
      "Please use return_type instead and consult the function documentation for its enhanced functionality.",
      call.=FALSE
    )
    if(only_ggplot==TRUE){
      return_type <- "mapping"
    } else{
      return_type <- "plot"
    }
  }
  

  # Extract plotting options from metadata
  use2_variables <- c(color, linetype, facet_row, facet_col, wrap_var)
  if (is.null(use2_variables) == FALSE){
    for (var in use2_variables){
      data[[use2]][,var] <- factor(data[[use2]][,var])
    }
  }

  # Merge tracking data with metadata, aggregate it
  # and reshape it into long format
  trajectories <- mt_aggregate(data=data,
    use=use, use2=use2, use2_variables=use2_variables,
    subject_id=subject_id, ...
  )
  
  # Setup basic mapping
  current_mapping <-
    ggplot2::aes_string(
      x=x, y=y,
      color=color, linetype=linetype
    )
  
  # Build geom in case only geom should be returned
  if (return_type == "geom"){
    
    current_geom <- ggplot2::geom_path(
      mapping = current_mapping,
      data = trajectories,
      alpha = alpha, size = size,
      inherit.aes = FALSE
    )
    
    # ... and return this geom if no points are required
    if (points == FALSE) {
      return(current_geom)
      
      # ... and return list of geoms if points are required as well
    } else {
      return(
        list(
          current_geom,
          ggplot2::geom_point(
            mapping = current_mapping,
            data = trajectories,
            alpha = alpha,
            inherit.aes = FALSE
          )
        )
      ) 
    }
    
  # Otherwise, setup new ggplot
  } else{
    
    current_plot <- ggplot2::ggplot(
      data = trajectories,
      mapping = current_mapping
    )
    
    # Add facets (optional)
    if(!is.null(facet_row) | !is.null(facet_col)) {
      facet_row <- ifelse(is.null(facet_row),".",facet_row)
      facet_col <- ifelse(is.null(facet_col),".",facet_col)
      facet_formula <- stats::as.formula(paste(facet_row,facet_col,sep="~"))
      current_plot <- current_plot + ggplot2::facet_grid(facet_formula)
    }
    
    # Add wrapping (optional)
    if(is.null(wrap_var)==FALSE) {
      current_plot <- current_plot + ggplot2::facet_wrap(facets=wrap_var, ncol=wrap_ncol)
    }
    
    
    if (return_type == "mapping") {
      # Return empty plot object
      return(current_plot)
    } else {
      
      # Add path geom to plot
      current_plot <- current_plot +
        ggplot2::geom_path(alpha = alpha, size = size)
      
      # Add points to plot (optional)
      if (points) {
        current_plot <- current_plot +
          ggplot2::geom_point(alpha = alpha)
      }
      
      return(current_plot)
      
    }
    
    
  }


}

#' Add rectangles to trajectory plot.
#'
#' \code{mt_plot_add_rect} adds one or several rectangles to a mousetrap plot.
#' These buttons usually correspond to the borders of the buttons in the
#' mouse-tracking experiment. It is specifically designed so that the arguments
#' from the \code{mousetrap_response} plugin in OpenSesame can be used.
#'
#' \code{mt_plot_add_rect} internally uses \link[ggplot2:geom_tile]{geom_rect} of the
#' \code{ggplot2} package for plotting.
#'
#' @param rect a data.frame or matrix with one row per box. For each rectangle,
#'   the x-position (\code{x}), y-position (\code{y}), width (\code{w}), and
#'   height (\code{h}) needs to be provided. If columns are not labeled, the
#'   order \code{x, y, w, h} is assumed.
#' @param color argument passed on to \link[ggplot2:geom_tile]{geom_rect}. Specifies the
#'   color of the border of the rectangles.
#' @param fill argument passed on to \link[ggplot2:geom_tile]{geom_rect}. Specifies the
#'   color of the interior of the rectangles. If \code{NA} (the default),
#'   rectangles are unfilled.
#' @param ... additional arguments passed on to \link[ggplot2:geom_tile]{geom_rect}.
#'
#' @seealso
#' \link{mt_plot} for plotting trajectory data.
#'
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#' 
#' @examples
#' # Load ggplot2
#' library(ggplot2)
#'
#' # Import, flip, and time-normalize raw trajectories
#' mt_example <- mt_import_mousetrap(mt_example_raw)
#' mt_example <- mt_remap_symmetric(mt_example,remap_xpos="no")
#' mt_example <- mt_time_normalize(mt_example)
#'
#' # Create rectangles matrix
#' rectangles <- matrix(
#'   # (The matrix is n x 4, and contains
#'   # all relevant data for every button,
#'   # (i.e. x, y, width and height values)
#'   # in separate rows)
#'   c(
#'    -840, 525,  350, -170,
#'     840, 525, -350, -170
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
        color=NULL, linetype=NULL
      ),
      inherit.aes = FALSE,
      color=color, fill=fill,
      # ensure that this function does not influence the legend
      show.legend=FALSE,
      ...
    )
  )
}

#' Create PDF with separate plots per trajectory.
#'
#' \code{mt_plot_per_trajectory} creates a PDF file with separate plots per
#' trajectory. This PDF can be used for inspecting individual trajectories. Note
#' that plotting all trajectories can be time-consuming, especially for raw
#' trajectories. If the appropriate \code{x} and \code{y} arguments are
#' inserted, this function can also be used for plotting velocity and
#' acceleration profiles.
#'
#' \code{mt_plot_per_trajectory} creates a PDF using \link[grDevices]{pdf}.
#' Next, it plots all trajectories individually using \link{mt_plot}. Every plot
#' is labeled using the \link{rownames} of the trajectories.
#'
#' @inheritParams mt_plot
#' @param file a character string specifying the name of the PDF file. Passed on
#'   to \link[grDevices]{pdf}.
#' @param xlim optional argument specifying the limits for the x axis (passed on
#'   to \link[ggplot2]{coord_cartesian}). If not specified (the default),
#'   sensible axis limits will be computed.
#' @param ylim optional argument specifying the limits for the y axis (passed on
#'   to \link[ggplot2]{coord_cartesian}). If not specified (the default),
#'   sensible axis limits will be computed.
#' @param axes_exact logical. If \code{TRUE}, axes will be set without offset
#'   exactly at the limits of the x and y axes (which can be specified using
#'   \code{xlim} and \code{ylim}]).
#' @param rect optional argument passed on to \link{mt_plot_add_rect}. If
#'   specified, rectangles (usually representing the response buttons) will be
#'   plotted for each trajectory plot.
#' @param color optional argument passed on to \link{mt_plot_add_rect}. Only
#'   relevant if \code{rect} is specified.
#' @param fill optional argument passed on to \link{mt_plot_add_rect}. Only
#'   relevant if \code{rect} is specified.
#' @param verbose logical indicating whether function should report its
#'   progress.
#' @param ... additional arguments passed on to \link[grDevices]{pdf}.
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
#' @author
#' Pascal J. Kieslich
#' 
#' Felix Henninger
#' 
#' @export
mt_plot_per_trajectory <- function(file,
  data, use="trajectories", x="xpos", y="ypos",
  xlim=NULL, ylim=NULL, axes_exact=FALSE,
  points=FALSE,
  rect=NULL, color="black", fill=NA,
  verbose=FALSE,...) {
  
  # Extract trajectory data
  trajectories <- extract_data(data=data,use=use)
  
  # Define axis limits across all plots (if they have not been defined)
  if(is.null(xlim)){
    xlim <- range(trajectories[,,x], na.rm=TRUE)
    xoffset <- .05 * (xlim[2]-xlim[1])
    xlim[1] <- xlim[1] - xoffset
    xlim[2] <- xlim[2] + xoffset
  }

  if(is.null(ylim)){
    ylim <- range(trajectories[,,y], na.rm=TRUE)
    yoffset <- .05 * (ylim[2]-ylim[1])
    ylim[1] <- ylim[1] - yoffset
    ylim[2] <- ylim[2] + yoffset
  }

  # Create plots
  grDevices::pdf(file, ...)

  # Plot each trajectory individually
  # (as a separate page in the PDF file)
  for (current_id in row.names(trajectories)) {
    # Build plot
    current_plot <- mt_plot(
      data=trajectories[current_id,,,drop=FALSE], x=x, y=y,
      points=points
    )

    # Add rectangles to plot
    # if they are specified
    if (is.null(rect)==FALSE){
      current_plot <- current_plot +
        mt_plot_add_rect(rect=rect, color=color, fill=fill)
    }

    # Remove whitespace for axes (if specified)
    if (axes_exact){
      current_plot <- current_plot +
        ggplot2::scale_x_continuous(expand=c(0, 0)) +
        ggplot2::scale_y_continuous(expand=c(0, 0))
    }

    # Output plot
    print(
      current_plot +
      ggplot2::ggtitle(current_id) +
      ggplot2::coord_cartesian(xlim=xlim, ylim=ylim)
    )

    # Display progress, if desired
    if (verbose) {
      message("trajectory ", current_id, " plotted")
    }
  }

  # Close device
  grDevices::dev.off()
}
