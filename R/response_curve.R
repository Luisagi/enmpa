#' Variable response curves for GLMs
#'
#' @description
#' A view of variable responses in models. Responses based on single or multiple
#' models can be provided.
#'
#' @usage
#' response_curve(fitted, variable, data = NULL, modelID = NULL,  n = 100,
#'                new_data = NULL,  extrapolate = TRUE, show_lines = TRUE,
#'                xlab = NULL, ylab = "Probability", col = "red", ...)
#
#' @param fitted an object of class `glm`, a list of GLMs obtained using the
#' function \code{\link{fit_glms}},  or an object `enmpa_fitted_models` from
#' \code{\link{fit_selected}} .
#' @param variable (character) name of the variables to be plotted.
#' @param data data.frame or matrix of data used in the model calibration step.
#' Default = NULL.
#' @param modelID (character) vector of ModelID(s) to be considered when the
#' fitted models is an `enmpa_fitted_object`. By default all models are included.
#' Default = NULL.
#' @param n (numeric) an integer guiding the number of breaks. Default = 100
#' @param new_data a `SpatRaster`, data.frame, or  matrix of variables
#' representing the range of variable values in an area of interest.
#' Default = NULL. It must be defined in case the model entered does not
#' explicitly include a data component.
#' @param extrapolate (logical) whether to allow extrapolation to study the
#' behavior of the response outside the calibration limits. Ignored if
#' `new_data` is defined. Default = TRUE.
#' @param show_lines (logical) whether to show variable responses of distinct
#' models as different lines. Default = TRUE. If
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
#' When responses for multiple models are to be plotted, and `show_lines` =
#' FALSE, the mean and confidence intervals for the set of responses are
#' calculated using a GAM.
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
#' response_curve(sel_fit, modelID = "ModelID_7", variable = "bio_1")
#'
#' # Response curve when model(s) are in a list (only one model in this one)
#' response_curve(sel_fit, variable = "bio_12")

response_curve <- function(fitted, variable, data = NULL, modelID = NULL, n = 100,
                           new_data = NULL, extrapolate = TRUE, show_lines = TRUE,
                           xlab = NULL, ylab = "Probability", col = "red", ...) {

  # initial tests
  if (missing(fitted) | missing(variable)) {
    stop("Argument 'fitted' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  if (!is.null(data)) {
    if (!class(data)[1] %in% c("matrix", "data.frame")) {
      stop("'data' must be of class 'matrix', 'data.frame'")
    }
  }


  if (is.null(xlab)) {xlab <- variable}

  #  individual GLMs____________________________________________________________
  if (class(fitted)[1] == "glm") {
    response_curve_ind(fitted, variable, data = data, n = n,
                       new_data = new_data, extrapolate = extrapolate,
                       xlab = xlab, ylab = ylab, col = col, ...)
  }

  #  List of GLMs_______________________________________________________________
  if (check_if_glm_list(fitted)){

    # check availability
    if (is.null(data) && is.null(fitted[1][[1]]$data)){
      stop("Calibration data must be defined.")
    } else {
      if (!is.null(fitted[1][[1]]$data)){
        data  <- fitted[1][[1]]$data
      }
    }

    if (is.null(modelID)){

      response_curve_cons(fitted, variable, data = data, n = n, new_data = new_data,
                          extrapolate = extrapolate, show_lines = show_lines,
                          xlab = xlab, ylab = ylab, col = col, ...)
    } else {

      if (!modelID %in% names(fitted)){
        stop(paste0("The 'ModelID' is not correct, check the following:\n[",
                    paste(names(fitted), collapse = ", ")),
             "]"
        )
      }

      if (length(modelID) > 1){
        response_curve_cons(fitted[modelID], variable, data = data, n = n,
                            new_data = new_data, extrapolate = extrapolate,
                            show_lines = show_lines, xlab = xlab, ylab = ylab,
                            col = col, ...)
      } else {
        response_curve_ind(fitted[modelID][[1]], variable, data = data, n = n,
                           new_data = new_data, extrapolate = extrapolate,
                           xlab = xlab, ylab = ylab, col = col, ...)
      }
    }
  }

  #   # if data argument is not empty
  #   if (!is.null(data) && length(fitted) > 1 ){
  #     response_curve_cons(fitted, variable, data = data, n = n,
  #                         new_data = new_data, extrapolate = extrapolate,
  #                         xlab = xlab, ylab = ylab, col = col, ...)
  #   }
  #
  #   if (!is.null(data) && length(fitted) == 1 ){
  #     response_curve_ind(fitted[1][[1]], variable, data = data, n = n,
  #                         new_data = new_data, extrapolate = extrapolate,
  #                         xlab = xlab, ylab = ylab, col = col, ...)
  #   }
  #
  #   # if data argument is empty
  #   if (is.null(fitted[1][[1]]$data)){
  #     stop("Calibration data must be defined.")
  #
  #   } else {
  #     data <- fitted[1][[1]]$data
  #
  #     if (length(fitted) > 1){
  #       response_curve_cons(fitted, variable, data = data, n = n,
  #                           new_data = new_data, extrapolate = extrapolate,
  #                           xlab = xlab, ylab = ylab, col = col, ...)
  #     } else {
  #       response_curve_ind(fitted[1][[1]], variable, data = data, n = n,
  #                          new_data = new_data, extrapolate = extrapolate,
  #                          xlab = xlab, ylab = ylab, col = col, ...)
  #     }
  #   }
  # }

  #  using de 'enmpa fitted object'_____________________________________________
  if (class(fitted)[1] == "enmpa_fitted_models") {
    data  <- fitted$data
    list_glms <-  fitted$glms_fitted

    if (is.null(modelID)){

      response_curve_cons(list_glms, variable, data = data, n = n, new_data = new_data,
                          extrapolate = extrapolate, show_lines = show_lines,
                          xlab = xlab, ylab = ylab, col = col, ...)
    } else {

      if (!modelID %in% names(list_glms)){
        stop(paste0("The ModelID is not correct, check the following:\n[",
                    paste(names(list_glms), collapse = ", ")),
             "]"
        )
      }

      if (length(modelID) > 1){
        response_curve_cons(list_glms[modelID], variable, data = data, n = n,
                            new_data = new_data,extrapolate = extrapolate,
                            show_lines = show_lines, xlab = xlab, ylab = ylab,
                            col = col, ...)
      } else {
        response_curve_ind(list_glms[modelID][[1]], variable, data = data, n = n,
                           new_data = new_data, extrapolate = extrapolate,
                           xlab = xlab, ylab = ylab, col = col, ...)
      }
    }
  }
}
