#' Response curve
#'
#' @description
#' The function offers a comprehensive view of the response of a variable,
#' either based on a single model or multiple models. It plots the probabilities
#' of species presence across a wide range of values for a specific environmental
#' variable.
#'
#' The function calculates these probabilities by focusing on a single
#' environmental gradient while keeping all other gradients constant at their
#' mean values (Elith et al., 2005). This approach allows for the estimation of
#' response curves, enabling a deeper understanding of how species occurrence
#' fluctuates in response to the environmental conditions captured by the
#' variable.
#'
#' @usage
#' response_curve(model, variable, n = 100, new_data = NULL, extrapolate = FALSE,
#'                xlab = NULL, ylab = "Probability", col = "red", ...)
#
#' @param model an object of class `glm` or a list of them which inherit
#' from the class `lm`.
#' @param variable (character) name of the variables to be plotted.
#' @param n (numeric) an integer guiding the number of breaks. Default n = 100
#' @param new_data a `SpatRaster`, data.frame or  matrix of variables
#' representing the range of values for the complete extent in an area of
#' interest. Default = NULL.
#' @param extrapolate (logical) whether to allow extrapolation to study the
#' behavior of the response outside the calibration limits. Ignored if `new_data`
#' is defined.
#' @param xlab (character) a label for the x axis.
#' @param ylab (character) a label for the y axis.
#' @param col (character) color for lines.
#' @param ... additional arguments passed to \code{\link[graphics]{plot}}.
#'
#' @examples
#' # Load species occurrences and environmental data.
#' enm_data <- read.csv(system.file("extdata", "pa_data.csv", package = "enmpa"))
#' head(enm_data)
#'
#' # Custom formulas
#' forms <- c("Sp ~ bio_1 + I(bio_1^2) + I(bio_12^2)",
#'            "Sp ~ bio_12 + I(bio_1^2) + I(bio_12^2)")
#'
#' # Fitting models
#' fits <- fit_glm(forms, data = enm_data )
#'
#' # Response curve for single models
#' response_curve(fits$Model_ID_1, variable = "bio_1",  extrapolate = TRUE)
#' response_curve(fits$Model_ID_1, variable = "bio_12", extrapolate = TRUE)
#'
#' # Consensus response curve for both models
#' response_curve(fits, variable = "bio_1",  extrapolate = TRUE)
#' response_curve(fits, variable = "bio_12", extrapolate = TRUE)
#'
#'
#' @export
#'
#' @importFrom stats predict coef
#' @importFrom graphics abline polygon
#' @importFrom terra minmax
#' @importFrom mgcv gam

response_curve <- function(model, variable, n = 100, new_data = NULL,
                           extrapolate = FALSE, xlab = NULL,
                           ylab = "Probability", col = "red", ...) {

  # initial tests
  if (missing(model) | missing(variable)) {
    stop("Argument 'model' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  if (is.null(xlab)){xlab <- variable}

  # Response curve for all selected models
  if (check_if_glm_list(model)){

    response_curve_cons(model, variable, n = n, new_data = new_data,
                        extrapolate = extrapolate, xlab = xlab, ylab = ylab,
                        col = col, ...)
  }
  # Response curve of an individual model
  else {
    response_curve_ind(model, variable, n = n, new_data = new_data,
                       extrapolate = extrapolate, xlab = xlab, ylab = ylab,
                       col = col, ...)
  }

}














