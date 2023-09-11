#' Plot variable importance
#'
#' @param x `data.frame`, output from enmpa::var_importance().
#' @param xlab a label for the x axis.
#' @param ylab a label for the y axis.
#' @param main main title for the plot.
#' @param extra_info `logical`, adds extra details to the plot when there is
#' more information from more than one model.
#' @param ... additional arguments passed to graphics::barplot or
#' graphics::boxplot.
#'
#' @export
#'
#' @importFrom graphics barplot boxplot text
#'
#' @examples
#' # Single model
#' # vi_1 <- var_importance(preds$fitted_models$Model_ID_1)
#' # plot_importance(x = vi_1)
#'
#' # Multiple models
#' # vi_2 <- var_importance(preds$fitted_models)
#' # plot_importance(x = vi_2)
#'

plot_importance <- function(x,
                            xlab = NULL,
                            ylab = "Relative contribution",
                            main = "Variable importance",
                            extra_info = TRUE, ...) {

  # Check if single or multiple models
  if ("Models" %in% colnames(x)) {

    # Create a list of arguments to pass to boxplot
    boxplot_args <- list(formula = contribution ~ predictor,
                         data = x,
                         main = main,
                         xlab= xlab,
                         ylab = ylab,
                         ylim = c(0, max(x$contribution) + 0.2), ...)

    # plot a boxplot using do.call()
    do.call(boxplot, boxplot_args)


    if (extra_info){

      #Calculate means and counts per group
      means  <- tapply(x$contribution, x$predictor, mean)
      counts <- tapply(x$contribution, x$predictor, length)

      # Common y coordinate for text labels in the upper part of the plot
      text_y <- max(x$contribution) + 0.15

      # Add means and counts to the plot (same level)
      for (i in 1:length(unique(x$predictor))) {
        text(i, text_y, paste("Mean =", round(means[i], 2)), pos = 3)
        text(i, text_y , paste("N =", counts[i]), pos = 1)
      }
    }

  } else {

    # Create a list of arguments to pass to barplot
    barplot_args <- list(height = x$contribution,
                         names.arg = x$predictor,
                         main = main,
                         xlab = xlab,
                         ylab = ylab, ...)

    # plot a barplot using do.call()
    do.call(barplot, barplot_args)

  }
}












