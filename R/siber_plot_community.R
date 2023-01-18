#' Plotting the raw data 
#' 
#' @description
#' This function create a SIBER a SIBER object and plot the raw data 
#' 
#' @param x list of SIBER ojects
#'
#' @return plot the ellipse for each community 
#' 
#' @export
#'
#' @examples
#' siber_plot_community ()
#' siber_plot_community (siber_data)

siber_plot_community <- function (data) {
  
  # List of community (depth)
  community <- c("25", "370", "555", "715", "1000", "1010", "1335")
  
  #Standard siber options
  community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
  group.ellipses.args  <- list(n = 100, lty = 1, lwd = 2)
  group.hull.args      <- list(lty = 2, col = "grey20")
  
  for (i in community) {
    # Assign the stable isotope data for the current community to a variable
    data_test <- subset(data, community == i)
    # convert in siber object
    siber_results <- SIBER::createSiberObject (data_test)
    # Plot the results for the current community
    SIBER::plotSiberObject (siber_results,
                            ax.pad = 0.1, 
                            hulls = T, community.hulls.args, 
                            ellipses = T, group.ellipses.args,
                            group.hulls = F, group.hull.args,
                            bty = "L",
                            iso.order = c(1, 2),
                            xlab = expression({delta}^13*C~('\u2030')),
                            ylab = expression({delta}^15*N~('\u2030')))
    # Add a title to the plot with the current community (depth) name
    title (main = i)
  }
}

