#' Plot density of mouse positions across time steps.
#' 
#' \code{mt_plot_riverbed} creates a plot showing the distribution of one
#' trajectory variable (e.g., the x-positions or velocity) per time step.
#' 
#' This function plots the relative frequency of the values of a trajectory 
#' variable separately for each of a series of time steps. This type of plot has
#' been used in previous research to visualize the distribution of x-positions
#' per time step (e.g., Scherbaum et al., 2010).
#' 
#' \code{mt_plot_riverbed} usually is applied to time-normalized trajectory data
#' as all trajectories must contain the same number of values.
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
#' @param facet_row an optional character string specifying a variable in 
#'   \code{data[[facet_data]]} that should be used for (row-wise) faceting. If 
#'   specified, separate riverbed plots for each level of the variable will be 
#'   created.
#' @param facet_col an optional character string specifying a variable in 
#'   \code{data[[facet_data]]} that should be used for (column-wise) faceting.
#'   If specified, separate riverbed plots for each level of the variable will
#'   be created.
#' @param facet_data a character string specifying where the (optional) data
#'   containing the faceting variables can be found.
#'   
#' @references Scherbaum, S., Dshemuchadse, M., Fischer, R., & Goschke, T.
#'   (2010). How decisions evolve: The temporal dynamics of action selection.
#'   \emph{Cognition, 115}(3), 407-416.
#' 
#' @seealso \link{mt_plot} for plotting trajectory data.
#'
#'   \link{mt_time_normalize} for time-normalizing trajectories.
#' 
#' @examples
#' # Time-normalize trajectories
#' KH2017 <- mt_time_normalize(KH2017)
#'   
#' # Create riverbed plot for all trials
#' mt_plot_riverbed(KH2017)
#' 
#' \dontrun{
#' # Create separate plots for typical and atypical trials
#' mt_plot_riverbed(mt_example, facet_col="Condition")
#' 
#' 
#' # Create riverbed plot for all trials with custom x and y axis labels
#' mt_plot_riverbed(mt_example) +
#'   ggplot2::xlab("Time step") + ggplot2::ylab("X coordinate")
#' 
#' # Note that it is also possible to replace the
#' # default scale for fill with a custom scale
#' mt_plot_riverbed(mt_example, facet_col="Condition") +
#'   ggplot2::scale_fill_gradientn(colours=grDevices::heat.colors(9),
#'     name="Frequency", trans="log", labels=scales::percent)
#' }
#' 
#' @author
#' Felix Henninger (\email{mailbox@@felixhenninger.com})
#' 
#' Pascal J. Kieslich (\email{kieslich@@psychologie.uni-mannheim.de})
#' 
#' @export
mt_plot_riverbed <- function(data, use='tn_trajectories', 
                             y='xpos', y_range=NULL, y_bins=250,
                             facet_row=NULL, facet_col=NULL, facet_data='data') {
  
  # Extract data from mousetrap data object
  trajectories <- extract_data(data=data, use=use)
  
  # Calculate range of values on y axis,
  # if not specified explicitly
  if (is.null(y_range)) {
    y_range <- range(trajectories[,,y])
  }
  
  # Compute breaks based on the number of bins required
  y_breaks <- seq(y_range[1], y_range[2], length.out=y_bins)
  
  # Compute the center value of each of the resulting bins
  y_bins <- y_breaks[1:(length(y_breaks) - 1)] + diff(y_breaks) / 2
  
  # Extract number of steps on x axis
  steps <- dim(trajectories)[2]
  
  # Prepare optional facet variables
  if (!is.null(facet_row)){
    facet_row_values <- data[[facet_data]][,facet_row]
    names(facet_row_values) <- rownames(data[[facet_data]])
    facet_row_values <- facet_row_values[dimnames(trajectories)[[1]]]
  } else {
    facet_row_values <- rep('constant',each=dim(trajectories)[1])
    names(facet_row_values) <- dimnames(trajectories)[[1]]
  }
  
  if (!is.null(facet_col)){
    facet_col_values <- data[[facet_data]][,facet_col]
    names(facet_col_values) <- rownames(data[[facet_data]])
    facet_col_values <- facet_col_values[dimnames(trajectories)[[1]]]
  } else {
    facet_col_values <- rep('constant',each=dim(trajectories)[1])
    names(facet_col_values) <- dimnames(trajectories)[[1]]
  }
  
  # Create empty data.frame for plot data
  riverbed <- data.frame()
  
  
  # Iterate across facet_row levels
  for (facet_row_value in unique(facet_row_values)) {
    
    # Iterate across facet_col levels
    for (facet_col_value in unique(facet_col_values)) {
      
      # Iterate across x-axis steps
      for (step in 1:steps) {
        
        # Extract data for current step
        step_data <- trajectories[facet_row_value==facet_row_values & facet_col_values==facet_col_value,step,]
        
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
            alpha=step_hist$density > 0,
            facet_row=facet_row_value,
            facet_col=facet_col_value
          )
        )
      }
    }
  }
  
  # Preserve factor levels if facet variables are factors
  if(!is.null(facet_row)){
    if(is.factor(data[[facet_data]][,facet_row])){
      riverbed[,'facet_row'] <- factor(riverbed[,'facet_row'],levels=levels(data[[facet_data]][,facet_row]))
    }
  }
  if(!is.null(facet_col)){
    if(is.factor(data[[facet_data]][,facet_col])){
      riverbed[,'facet_col'] <- factor(riverbed[,'facet_col'],levels=levels(data[[facet_data]][,facet_col]))
    }
  }
  
  # Remove zero frequencies (coded in alpha)
  riverbed <- riverbed[riverbed[,"alpha"]==TRUE,]
  
  # Create plot output
  output <- ggplot2::ggplot(ggplot2::aes_string(x='value_x', y='value_y', 
      fill='frequency'), data=riverbed) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(9, "YlOrRd")),
      name='Frequency', trans='log', labels=scales::percent) +
    ggplot2::scale_alpha_manual(values=c(0, 1), guide='none') +
    ggplot2::xlab('Steps') + ggplot2::ylab(y) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill='black'),
      panel.grid.major = ggplot2::element_line(colour='gray30'),
      panel.grid.minor = ggplot2::element_line(colour='gray10')
    )
  
  # Return output with / without facets depending on the specified variables
  if(is.null(facet_row) & is.null(facet_col)) {
    return(output)
  } else {
    facet_row <- ifelse(is.null(facet_row),'.','facet_row')
    facet_col <- ifelse(is.null(facet_col),'.','facet_col')
    facet_formula <- stats::as.formula(paste(facet_row,facet_col,sep='~'))
    return(output + ggplot2::facet_grid(facet_formula))
  }
  
}