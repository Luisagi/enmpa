#' Response curve
#'
#' @description
#' The function evaluates the response of the variable and its limits.
#'
#' @usage
#' response_curve(model, variable, n = 100, new_data = NULL,
#' new_range = NULL, rescale = FALSE)
#
#' @param model an object of class "glm" of a "list" of them which inherit
#' from the class "lm".
#' @param variable `character vector`, name or names of the variables to be plotted.
#' @param n `numeric`, an integer guiding the number of breaks. Default n = 100
#' @param new_data a `SpatRaster`, data.frame or  matrix of variables
#' representing the range of values for the complete extent of the study area.
#' Default = NULL
#' @param new_range `numeric vector`, a numerical vector with the lower and
#' upper limits of the variable. Default = NULL
#' @param rescale probability 0-1. Default = FALSE.
#'
#' @export
#'
#' @importFrom stats predict coef
#' @importFrom graphics abline polygon
#' @importFrom terra minmax
#' @importFrom mgcv gam


response_curve <- function(model, variable, n = 100, new_data = NULL,
                           new_range = NULL, rescale = FALSE) {

  # initial tests
  if (missing(model) | missing(variable)) {
    stop("Argument 'model' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  if (check_if_glm_list(model)){

    response_curve_cons(model, variable, n = 100, new_data = new_data,
                        new_range = new_range, rescale = rescale)
  }
  else {
    response_curve_ind(model, variable, n = 100, new_data = new_data,
                        new_range = new_range, rescale = rescale)
  }
}














