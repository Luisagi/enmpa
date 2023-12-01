#' Plot Niche Signal results
#'
#' @description
#' Plots to interpret results from niche_signal tests
#' (Cobos & Peterson (2022) <doi:10.17161/bi.v17i.15985>).
#'
#' @usage
#' plot_niche_signal(niche_signal_list, statistic = "mean",
#'                   variables = NULL, ellipses = FALSE, level = 0.99,
#'                   breaks = "Sturges", main = "", xlab = NULL, ylab = NULL,
#'                   h_col = "lightgray", h_cex = 0.8, lty = 2, lwd = 1,
#'                   l_col = c("blue", "black"), e_col = c("black", "red"),
#'                   pch = 19, pt_cex = c(1.3, 0.8), pt_col = c("black", "red"),
#'                   ...)
#'
#' @param niche_signal_list list of results from niche_signal.
#' @param statistic (character) name of the statistic for which results will be
#' explored when results come for univariate analysis. Default = "mean". Options
#' are: "mean", "median", "SD", and "range".
#' @param variables (character) name of variables to used in plots when
#' results come from analysis using the `permanova` method. The default, NULL,
#' uses the first two variables.
#' @param ellipses (logical) whether to use ellipses to represent all and
#' positive data when results come from PERMANOVA. The default, FALSE, plots
#' points instead.
#' @param level (numeric) value from 0 to 1 representing the limit of
#' the ellipse to be plotted. Default = 0.99.
#' @param main (character) title for plot. Default = "".
#' @param xlab (character) x axis label. Default = NULL. For results from
#' PERMANOVA, appropriate variable names are used.
#' @param ylab (character) y axis label. Default = NULL. For univariate
#' results, the default turn into "Frequency". For results from PERMANOVA,
#' appropriate variable names are used.
#' @param breaks breaks in the histogram as in \code{\link[graphics]{hist}}.
#' Default = "Sturges".
#' @param h_col a color to be used to fill the bars of histograms.
#' Default = "lightgray".
#' @param h_cex (numeric) value by which plotting text and symbols should be
#' magnified relative to the default in histograms. Default = 0.8.
#' @param lty (numeric) line type. See options in \code{\link[graphics]{par}}.
#' Default = 2. A vector of length = 2 can be used, in order, all and positive.
#' @param lwd (numeric) line width. See options in \code{\link[graphics]{par}}.
#' Default = 1. A vector of length = 2 can be used, in order, all and positive.
#' @param l_col line color for observed value of positives and confidence
#' intervals. Default = c("blue", "black").
#' @param e_col color of ellipse lines for all and positive data.
#' Default = c("black", "red").
#' @param pch point type. See options in \code{\link[graphics]{points}}.
#' Default = 19. A vector of length = 2 can be used, in order, all and positive.
#' @param pt_cex (numeric) value by which points will be magnified. Values
#' for all and positive points are recommended. Default = c(1.3, 0.8).
#' @param pt_col color for points. Values for all and positive points are
#' recommended. Default = c("black", "red").
#' @param ... other plotting arguments to be used.
#'
#' @return
#' A plot.
#'
#' @export
#'
#' @importFrom ellipse ellipse
#' @importFrom stats cov anova ecdf quantile
#' @importFrom graphics lines box hist points
#'
#' @rdname plot_niche_signal
#'
#' @examples
#' # Load species occurrences and environmental data.
#' data("enm_data", package = "enmpa")
#' head(enm_data)
#'
#' # Detection of niche signal using an univariate non-parametric test
#' sn_bio1 <- niche_signal(data = enm_data,
#'                         variables = "bio_1",
#'                         condition = "Sp",
#'                         method = "univariate")
#'
#' plot_niche_signal(sn_bio1, variables = "bio_1")
#'
#' sn_bio12 <- niche_signal(data = enm_data,
#'                          variables = "bio_12",
#'                          condition = "Sp",
#'                          method = "univariate")
#'
#' plot_niche_signal(sn_bio12, variables = "bio_12")


plot_niche_signal <- function(niche_signal_list, statistic = "mean",
                              variables = NULL, ellipses = FALSE,
                              level = 0.99, breaks = "Sturges", main = "",
                              xlab = NULL, ylab = NULL, h_col = "lightgray",
                              h_cex = 0.8, lty = 2, lwd = 1,
                              l_col = c("blue", "black"),
                              e_col = c("black", "red"),
                              pch = 19, pt_cex = c(1.3, 0.8),
                              pt_col = c("black", "red"), ...) {

  # method using to produce niche_signal_list
  method <- niche_signal_list$summary$method

  if (!is.null(method)) {
    # plotting results from general analysis
    if (method == "univariate") {
      rlist <- list(
        data = niche_signal_list$data,
        summary = niche_signal_list$summary,
        analysis_results = niche_signal_list$univariate_results
      )

      plot_niche_signal_univariate(rlist, statistic, breaks, main, xlab,
                                   ylab, h_col, h_cex, lty, lwd, l_col, ...)

    } else {
      rlist <- list(
        modified_data = niche_signal_list$modified_data,
        summary = niche_signal_list$summary,
        permanova_results = niche_signal_list$permanova_results
      )

      plot_niche_signal_permanova(rlist, variables,
                                  ellipses, level, main, xlab, ylab, e_col,
                                  lty, lwd, pch, pt_cex, pt_col, ...)
    }
  } else {
    # plotting results from univariate analysis
    if (!is.null(niche_signal_list$analysis_results)) {
      plot_niche_signal_univariate(niche_signal_list, statistic,
                                   breaks, main, xlab, ylab, h_col,
                                   h_cex, lty, lwd, l_col, ...)
    } else {
      # plotting results from permanova results
      if (!is.null(niche_signal_list$permanova_results)) {
        plot_niche_signal_permanova(niche_signal_list, variables,
                                    ellipses, level, main, xlab, ylab, e_col,
                                    lty, lwd, pch, pt_cex, pt_col, ...)
      }
    }
  }
}



#' @rdname plot_niche_signal
#' @param niche_signal_univariate_list list of results from niche_signal_univariate.
#' @export
#' @usage
#' plot_niche_signal_univariate(niche_signal_univariate_list, statistic = "mean",
#'                              breaks = "Sturges", main = "", xlab = NULL,
#'                              ylab = "Frequency", h_col = "lightgray",
#'                              h_cex = 0.8, lty = 2, lwd = 1,
#'                              l_col = c("blue", "black"), ...)

plot_niche_signal_univariate <- function(niche_signal_univariate_list, statistic = "mean",
                                         breaks = "Sturges", main = "", xlab = NULL,
                                         ylab = "Frequency", h_col = "lightgray",
                                         h_cex = 0.8, lty = 2, lwd = 1,
                                         l_col = c("blue", "black"), ...) {
  # test
  if (missing(niche_signal_univariate_list)) {
    stop("Argument 'niche_signal_univariate_list' is missing")
  }

  # other graphical settings
  if (length(l_col) == 1) {
    l_col <- rep(l_col, 2)
  } else {
    l_col <- l_col[1:2]
  }

  if (length(lty) == 1) {
    lty <- rep(lty, 2)
  } else {
    lty <- lty[1:2]
  }

  if (length(lwd) == 1) {
    lwd <- rep(lwd, 2)
  } else {
    lwd <- lwd[1:2]
  }

  # preparing data
  if (statistic == "mean") {
    all <- niche_signal_univariate_list$analysis_results$null_results$mean
    pos <- niche_signal_univariate_list$analysis_results$positive_niche["mean"]
    ci <- quantile(niche_signal_univariate_list$analysis_results$null_results$mean,
                   c(0.025, 0.975))
  }

  if (statistic == "median") {
    all <- niche_signal_univariate_list$analysis_results$null_results$median
    pos <- niche_signal_univariate_list$analysis_results$positive_niche["median"]
    ci <- quantile(niche_signal_univariate_list$analysis_results$null_results$median,
                   c(0.025, 0.975))
  }

  if (statistic == "SD") {
    all <- niche_signal_univariate_list$analysis_results$null_results$SD
    pos <- niche_signal_univariate_list$analysis_results$positive_niche["SD"]
    ci <- quantile(niche_signal_univariate_list$analysis_results$null_results$SD,
                   c(0.025, 0.975))
  }

  if (statistic == "range") {
    all <- niche_signal_univariate_list$analysis_results$null_results$range
    pos <- niche_signal_univariate_list$analysis_results$positive_niche["range"]
    ci <- quantile(niche_signal_univariate_list$analysis_results$null_results$range,
                   c(0.025, 0.975))
  }

  var <- niche_signal_univariate_list$summary$variable

  # other things for plots
  ylab <- ifelse(is.null(ylab), "Frequency", ylab)
  xlab <- ifelse(is.null(xlab), paste0(var, " (", statistic, ")"), xlab)
  xrang <- range(c(all, pos))

  # plot
  hist(all, breaks = breaks, xlim = xrang, main = main, xlab = xlab,
       ylab = ylab, col = h_col, cex = h_cex, ...)
  abline(v = pos, col = l_col[1], lty = lty[1], lwd = lty[1])
  abline(v = ci, col = l_col[2], lty = lty[2], lwd = lty[2])
  box(bty = "l")
}



#' @rdname plot_niche_signal
#' @param niche_signal_permanova_list list of results from niche_signal_permanova.
#' @export
#' @usage
#' plot_niche_signal_permanova(niche_signal_permanova_list, variables = NULL,
#'                            ellipses = FALSE, level = 0.99, main = "",
#'                            xlab = NULL, ylab = NULL,
#'                            e_col = c("black", "red"), lty = 2, lwd = 1,
#'                            pch = 19, pt_cex = c(1.3, 0.8),
#'                            pt_col = c("black", "red"), ...)

plot_niche_signal_permanova <- function(niche_signal_permanova_list, variables = NULL,
                                        ellipses = FALSE, level = 0.99,
                                        main = "", xlab = NULL, ylab = NULL,
                                        e_col = c("black", "red"), lty = 2, lwd = 1,
                                        pch = 19, pt_cex = c(1.3, 0.8),
                                        pt_col = c("black", "red"), ...) {

  # test
  if (missing(niche_signal_permanova_list)) {
    stop("Argument 'niche_signal_permanova_list' is missing")
  }

  # selecting variables to use
  if (is.null(variables)) {
    variables <- niche_signal_permanova_list$summary$variables[1:2]
  } else {
    variables <- variables[1:2]
  }

  # relevant data to plot
  condition <- niche_signal_permanova_list$summary$condition
  condition <- niche_signal_permanova_list$modified_data[, condition]

  all <- condition == 0
  posi <- condition == 1

  # type of points
  if (ellipses == FALSE) {
    if (length(pch) == 1) {
      pch <- c(pch, pch)
    } else {
      pch <- pch[1:2]
    }

    if (length(pt_col) == 1) {
      pt_col <- c(pt_col, pt_col)
    } else {
      pt_col <- pt_col[1:2]
    }

    # labels
    xlab <- ifelse(is.null(xlab), variables[1], xlab)
    ylab <- ifelse(is.null(ylab), variables[2], ylab)

    # plot
    allp <- niche_signal_permanova_list$modified_data[all, variables]
    plot(allp, main = main, xlab = xlab, ylab = ylab, pch = pch[1],
         cex = pt_cex[1], col = pt_col[1], ...)

    allp <- niche_signal_permanova_list$modified_data[posi, variables]
    points(allp,
           pch = pch[2], cex = pt_cex[2], col = pt_col[2])

  } else {
    # type of lines
    if (length(e_col) == 1) {
      e_col <- rep(e_col, 2)
    } else {
      e_col <- e_col[1:2]
    }

    if (length(lty) == 1) {
      lty <- rep(lty, 2)
    } else {
      lty <- lty[1:2]
    }

    if (length(lwd) == 1) {
      lwd <- rep(lwd, 2)
    } else {
      lwd <- lwd[1:2]
    }

    # all ellipse
    hell <- ellipse::ellipse(
      x = cov(niche_signal_permanova_list$modified_data[all, variables]),
      centre = colMeans(niche_signal_permanova_list$modified_data[all, variables]),
      level = level
    )

    # positive ellipse
    pell <- ellipse::ellipse(
      x = cov(niche_signal_permanova_list$modified_data[posi, variables]),
      centre = colMeans(niche_signal_permanova_list$modified_data[posi, variables]),
      level = level
    )

    # plot limits
    rans <- apply(rbind(hell, pell), 2, range)

    # plot
    plot(rans, type = "n", main = main, xlab = xlab, ylab = ylab, ...)
    lines(hell, col = e_col[1], lwd = lwd[1], lty = lty[1])
    lines(pell, col = e_col[2], lwd = lwd[2], lty = lty[2])
  }
}
