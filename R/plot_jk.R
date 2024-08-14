#' Jackkniffe plot for variable contribution
#'
#' @description
#' The jackknife figure shows the impact of each variable on the full model,
#' providing detailed information about the function and significance of each
#' variable. Light blue indicates the impact on the model if the variable is not
#' included, while dark blue indicates the independent contribution of the
#' variable to the model.
#'
#' @usage
#' plot_jk(x, metric = "ROC_AUC", legend = TRUE,
#'         colors = c("cyan", "blue", "red"),
#'         xlab = NULL, main = NULL)
#'
#' @param x list output from \code{\link{jackknife}}.
#' @param metric (character) model metric to plot. Default = "ROC_AUC".
#' @param legend (logical) whether to add legend. Default = TRUE.
#' @param colors (character) vector of colors.
#' Default = c("cyan", "blue", "red").
#' @param xlab (character) a label for the x axis.
#' @param main (character) main title for the plot.
#'
#' @export
#' @importFrom graphics barplot par abline legend
#'

plot_jk <- function(x, metric = "ROC_AUC", legend = TRUE,
                    colors = c("cyan", "blue", "red"),
                    xlab = NULL, main = NULL){

  if (missing(x)) {
    stop("Arguments 'x' must be defined.")
  }

  if (!is.character(colors) && is.vector(colors) && length(colors) == 3){
    colors = c("cyan", "blue", "red")
  }

  # get matrix
  m <- as.matrix(cbind(x$Without[metric], x$With_only[metric]))
  colnames(m) <- c("without", "with_only")

  fm <- x[["Full_model_stats"]][[metric]]

  if(is.null(xlab)){xlab = metric}
  if(is.null(main)){main = paste("Jackknife of", metric)}

  if (metric %in% c("ROC_AUC", "TSS")){
    # xlim = c(0,1)}
    xlim = c(0, max(c(max(m), fm)) + 0.1)
  } else {
    xlim = c(0, max(c(max(m), fm)) + 0.10*max(c(max(m), fm)))
  }

  # Adjust margins to accommodate variable names and legend
  original_par <- par(no.readonly = TRUE)  # Save the original par settings

  if (legend){
    par(mar = c(5, 8, 4, 8) + 0.1)
  } else {
    par(mar = c(5, 8, 4, 6) + 0.1)
  }

  barplot(t(m),
          beside = TRUE,
          horiz = TRUE,
          xlim = xlim,
          las=1,
          col =  colors[1:2],
          xlab = xlab,
          cex.names = 1,
          main = main)
  abline(v = fm, col = colors[3], lty = "dotted", lwd = 2)

  if (legend){
    legend("topright",
           inset=c(-0.285, -0.075), xpd = TRUE, horiz = FALSE, bty = "n",
           cex = 0.95,
           legend=c("Without variable", "With only variable", "Full model"),
           fill = colors)
  }
  # Restore the original par settings
  par(original_par)
}
