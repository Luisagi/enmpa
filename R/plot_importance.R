#' Plot variable importance
#'
#' @description
#' Visualization of the results obtained with the function
#' \code{\link{var_importance}}.
#'
#' @usage
#' plot_importance(x, xlab = NULL, ylab = "Relative contribution",
#'                 main = "Variable importance", extra_info = TRUE, ...)
#'
#'
#' @param x data.frame output from \code{\link{var_importance}}.
#' @param xlab (character) a label for the x axis.
#' @param ylab (character) a label for the y axis.
#' @param main (character) main title for the plot.
#' @param extra_info (logical) when results are from more than one model, it
#' adds information about the number of models using each predictor and the mean
#' contribution found.
#' @param ... additional arguments passed to \code{\link[graphics]{barplot}} or
#' \code{\link[graphics]{boxplot}}.
#'
#' @return
#' A plot
#'
#' @export
#'
#' @importFrom graphics barplot boxplot text
#'
#'
#' @examples
#'
#' # Load species occurrences and environmental data.
#' data("enm_data", package = "enmpa")
#'
#' # Custom formulas
#' forms <- c("Sp ~ bio_1 + I(bio_1^2) + I(bio_12^2)",
#'            "Sp ~ bio_12 + I(bio_1^2) + I(bio_12^2)")
#'
#' # Fitting models
#' fits <- fit_glms(forms, data = enm_data)
#'
#' # Variable importance for single models
#' vi_1 <- var_importance(fits$ModelID_1)
#' plot_importance(x = vi_1)
#'
#' vi_2 <- var_importance(fits$ModelID_2)
#' plot_importance(x = vi_2)
#'
#' # Variable importance for multiple models
#' vi_c <- var_importance(fits)
#' plot_importance(x = vi_c)


plot_importance <- function(x, xlab = NULL, ylab = "Relative contribution",
                            main = "Variable importance", extra_info = TRUE,
                            ...) {

  if (missing(x)) {
    stop("Argument 'x' must be defined.")
  }

  # Check if single or multiple models
  if ("Models" %in% colnames(x)) {

    # sort predictors by importance
    sort_p <- with(x, reorder(predictor, contribution, median,
                               decreasing = TRUE))

    # Create a list of arguments to pass to boxplot
    boxplot_args <- list(formula = x$contribution ~ sort_p,
                         main = main, xlab= xlab, ylab = ylab,
                         ylim = c(0, max(x$contribution) + 0.2), ...)

    # plot a boxplot using do.call()
    do.call(boxplot, boxplot_args)


    if (extra_info) {
      #Calculate means and counts per group
      means  <- tapply(x$contribution, x$predictor, mean)[levels(sort_p)]
      counts <- tapply(x$contribution, x$predictor, length)[levels(sort_p)]

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
    barplot_args <- list(height = x$contribution, names.arg = x$predictor,
                         main = main, xlab = xlab, ylab = ylab, ...)

    # plot a barplot using do.call()
    do.call(barplot, barplot_args)
  }
}

