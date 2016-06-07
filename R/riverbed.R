#' Plot density of mouse positions across time steps.
#' 
#' \code{mt_plot_riverbed} creates a plot showing the relative frequency of
#' x-values per time step.
#' 
#' This plot indicates where the majority of trajectories lie, and the degree to
#' which trajectories deviate from the average trajectory. In most data sets,
#' this plot will result in a horizontal line of high density indicating that
#' participants took a certain amount of time before deviating from their
#' starting point.
#' 
#' @param data mousetrap data object containing the data to be plotted.
#' @param use character string specifying the set of trajectories to use in the
#'   plot. The steps of this set will constitute the x axis. Defaults to
#'   'tn_trajectories', which results in time steps being plotted on the x axis.
#' @param y variable in the mousetrap data object to be plotted on the 
#'   output's y dimension. Defaults to 'xpos', the cursor's x coordinate.
#' @param y_range numerical vector containing two values that represent the 
#'   upper and lower ends of the y axis. By default, the range is calculated 
#'   from the data provided.
#' @param y_bins number of bins to distribute along the y axis (defaults to
#'   250).
#' @param x_label label placed on the output's x axis (defaults to 'Time step').
#' @param y_label label placed on the output's y axis (defaults to 'X 
#'   coordinate', as appropriate given the default for \code{y}).
#' 
#' @seealso
#' \link{mt_plot} for plotting trajectory data.
#' 
#' @examples
#' # Load ggplot2
#' library(ggplot2)
#' 
#' # Time-normalize trajectories
#' mt_example <- mt_time_normalize(mt_example)
#'   
#' # Create riverbed plot for all trials
#' mt_plot_riverbed(mt_example)
#' 
#' 
#' # Create separate plots for typical and atypical trials:
#' 
#' # assess range for x-positions across both conditions
#' xpos_range <- range(mt_example$tn_trajectories[,"xpos",])
#' 
#' # create subsets of trials
#' mt_example_atypical <- mt_subset(mt_example,Condition=="Atypical")
#' mt_example_typical <- mt_subset(mt_example,Condition=="Typical")
#' 
#' # create separate riverbed plots
#' mt_plot_riverbed(mt_example_atypical,
#'   use="tn_trajectories", y_range=xpos_range)+
#'   ggtitle("Atypical condition")
#' 
#' mt_plot_riverbed(mt_example_typical,
#'   use="tn_trajectories", y_range=xpos_range)+
#'   ggtitle("Typical condition")
#' 
#' @export
mt_plot_riverbed <- function(data, use='tn_trajectories', 
                             y='xpos', y_range=NULL, y_bins=250, 
                             x_label='Time step', y_label='X coordinate') {
  
  # Extract data from mousetrap data object
  data <- extract_data(data=data, use=use)
  
  # Calculate range of values on y axis,
  # if not specified explicitly
  if (is.null(y_range)) {
    y_range <- range(data[,y,])
  }
  
  # Compute breaks based on the number of bins required
  y_breaks <- seq(y_range[1], y_range[2], length.out=y_bins)
  
  # Compute the center value of each of the resulting bins
  y_bins <- y_breaks[1:(length(y_breaks) - 1)] + diff(y_breaks) / 2
  
  # Extract number of steps on x axis
  steps <- dim(data)[3]
  
  # Create empty data.frame for plot data
  riverbed <- data.frame()
  
  # Iterate across x-axis steps
  for (step in 1:steps) {
    # Extract data for current step
    step_data <- data[,,step]
    
    # Calculate histogram based on this data
    step_hist <- graphics::hist(
      step_data[,y], 
      breaks=y_breaks,
      plot=FALSE
    )
    
    # Append densities and corresponding indices to plot data
    riverbed <- rbind(
      riverbed,
      data.frame(
        value_x=step,
        value_y=y_bins,
        frequency=step_hist$density * diff(y_breaks),
        alpha=step_hist$density > 0
      )
    )
  }
  
  # Create plot output
  output <- ggplot2::ggplot(ggplot2::aes_string(x='value_x', y='value_y', fill='frequency', alpha='alpha'), data=riverbed) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient(name='Frequency', trans='log', labels=scales::percent) +
    ggplot2::scale_alpha_manual(values=c(0, 1), guide='none') +
    ggplot2::xlab(x_label) + ggplot2::ylab(y_label) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill='black'),
      panel.grid.major = ggplot2::element_line(colour='gray30'),
      panel.grid.minor = ggplot2::element_line(colour='gray10')
    )
  
  return(output)
}