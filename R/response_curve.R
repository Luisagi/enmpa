#' Variable response curves for GLMs
#'
#' @description
#' A view of variable responses in models. Responses based on single or multiple
#' models can be provided.
#'
#' @usage
#' response_curve(fitted, variable, n = 100, new_data = NULL, extrapolate = TRUE,
#'                xlab = NULL, ylab = "Probability", col = "red", ...)
#
#' @param fitted an object of class `glm` or a list of GLMs obtained using the
#' functions \code{\link{fit_selected}} or \code{\link{fit_glms}}.
#' @param variable (character) name of the variables to be plotted.
#' @param n (numeric) an integer guiding the number of breaks. Default = 100
#' @param new_data a `SpatRaster`, data.frame, or  matrix of variables
#' representing the range of variable values in an area of interest.
#' Default = NULL.
#' @param extrapolate (logical) whether to allow extrapolation to study the
#' behavior of the response outside the calibration limits. Ignored if
#' `new_data` is defined. Default = TRUE.
#' @param xlab (character) a label for the x axis. The default, NULL, uses the
#' name defined in `variable`.
#' @param ylab (character) a label for the y axis. Default = "Probability".
#' @param col (character) color for lines. Default = "red".
#' @param ... additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @details
#' The function calculates these probabilities by focusing on a single
#' environmental variable while keeping all other variables constant at their
#' mean values.
#'
#' When responses for multiple models are to be plotted, the mean and confidence
#' intervals for the set of responses are calculated using a GAM.
#'
#' @return
#' A plot with the response curve for a `variable`.
#'
#' @export
#'
#' @importFrom stats predict coef
#' @importFrom graphics abline polygon
#' @importFrom terra minmax
#' @importFrom mgcv gam
#'
#' @examples
#' # Load a fitted selected model
#' data(sel_fit, package = "enmpa")
#'
#' # Response curve for single models
#' response_curve(sel_fit$ModelID_7, variable = "bio_1")
#'
#' # Response curve when model(s) are in a list (only one model in this one)
#' response_curve(sel_fit, variable = "bio_12")

response_curve <- function(fitted, variable, n = 100, new_data = NULL,
                           extrapolate = TRUE, xlab = NULL,
                           ylab = "Probability", col = "red", ...) {

  # initial tests
  if (missing(fitted) | missing(variable)) {
    stop("Argument 'fitted' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  # preparing data
  fitted$selected <- NULL
  if (is.null(xlab)) {xlab <- variable}

  # Response curve for all selected models
  if (check_if_glm_list(fitted)){
    response_curve_cons(fitted, variable, n = n, new_data = new_data,
                        extrapolate = extrapolate, xlab = xlab, ylab = ylab,
                        col = col, ...)
  } else {
    # Response curve of an individual model
    response_curve_ind(fitted, variable, n = n, new_data = new_data,
                       extrapolate = extrapolate, xlab = xlab, ylab = ylab,
                       col = col, ...)
  }

}
