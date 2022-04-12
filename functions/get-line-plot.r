# Wrapper for ggplot function (geom_line)
get_line_plot <- function(df, x, y, line_color = "#f8766d", ...) {
  # Get optional parameters
  options <- list(...)
  
  # Build plot
  if(length(options$group) > 0) {
    p <- ggplot(
      df,
      aes(x = !! sym(x), y = !! sym(y), group = !! sym(options$group))
    ) + 
      scale_y_continuous(label = comma) +
      geom_line(aes(color = !! sym(options$group)))
  } else {
    p <- ggplot(
      df,
      aes(x = !! sym(x), y = !! sym(y))
    ) + 
      scale_y_continuous(label = comma) +
      geom_line(color = line_color)
  }
  
  # Add a panel plot
  if(! is.null(options$panel_column) & ! is.null(options$panel_row)) {
    facet_grid_applied <- TRUE
    p <- p + 
      facet_grid(
        cols = vars(!! sym(options$panel_column)),
        rows = vars(!! sym(options$panel_row))
      )
  } else if(! is.null(options$panel_column)) {
    facet_grid_applied <- TRUE
    p <- p + facet_wrap(vars(!! sym(options$panel_column)))
  } else if(! is.null(options$panel_row)) {
    facet_grid_applied <- TRUE
    p <- p + facet_wrap(vars(!! sym(options$panel_row)))
  } else {
    facet_grid_applied <- FALSE
  }
  
  # Add a plot title if it exists
  if(length(options$plot_title) > 0) {
    p <- p + ggtitle(options$plot_title)
  }
  
  # Add an x variable label if it exists
  if(length(options$x_variable_label) > 0) {
    p <- p + xlab(options$x_variable_label)
  }
  
  # Add a y variable label if it exists
  if(length(options$y_variable_label) > 0) {
    p <- p + ylab(options$y_variable_label)
  }
  
  # Add theme options
  if(facet_grid_applied) {
    p <- p +
      theme(
        legend.title = element_blank(),
        plot.margin = margin(t = 36, r = 0, b = 36, l = 36),
        panel.spacing.y = unit(2, "lines")
      )
  } else {
    p <- p +
      theme_minimal() +
      theme(
        legend.title = element_blank(),
        plot.margin = margin(t = 36, r = 0, b = 36, l = 36)
      )
  }
  
  # Return plot object and plot stats
  return(
    list(
      plot = p,
      rows = get_plot_rows(p)
    )
  )
}