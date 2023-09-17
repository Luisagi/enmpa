#' Response curve
#'
#' @description
#' The function plot the response curve to  evaluates the variable of interest
#' and its limits.
#'
#' @usage
#' response_curve(model, variable, n = 100, new_data = NULL, new_range = NULL,
#'                rescale = FALSE)
#
#' @param model an object of class `glm` or a list of them which inherit
#' from the class `lm`.
#' @param variable (character) vector, name or names of the variables to be
#' plotted.
#' @param n (numeric) an integer guiding the number of breaks. Default n = 100
#' @param new_data a `SpatRaster`, data.frame or  matrix of variables
#' representing the range of values for the complete extent of the study area.
#' Default = NULL.
#' @param new_range (numeric) a numerical vector with the lower and upper limits
#' of the variable. Default = NULL.
#' @param rescale (logical) whether to scale the y-axis to the probability
#' range (0-1). Default = FALSE.
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


  for (i in variable){

    # Response curve for all selected models
    if (check_if_glm_list(model)){

      response_curve_cons(model, i, n = n, new_data = new_data,
                          new_range = new_range, rescale = rescale)
      Sys.sleep(1)
    }
    # Response curve of an individual model
    else {
      response_curve_ind(model, i, n = n, new_data = new_data,
                          new_range = new_range, rescale = rescale)
      Sys.sleep(1)

    }
  }
}














