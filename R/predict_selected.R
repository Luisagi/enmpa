#' Predictions for the models selected after calibration
#'
#' @description
#' Wrapper function that facilitates the prediction of those models
#' selected as the most robust. In addition, it allows the calculation of
#' consensus models, when more than one model are selected.
#'
#' @usage
#' predict_selected(fitted, newdata, clamping = FALSE, var_to_clamp = NULL,
#'                  type = "response", consensus = TRUE)
#'
#' @param fitted a list of GLMs obtained using the
#' functions \code{\link{fit_selected}} or \code{\link{fit_glms}}.
#' @param newdata a `SpatRaster`, data.frame, or matrix with the new data on
#' which to predict.
#' @param clamping (logical) this option controls extrapolation when making
#' predictions for environmental conditions beyond the calibration data.
#' Default = FALSE.
#' @param var_to_clamp (character) a vector containing the names of the variables
#' that will undergo clamping. By default, if no specific names are provided,
#' the value is set to NULL, which indicates that clamping will be applied to
#' all variables. Ignore if clamping = FALSE.
#' @param type (character) the type of prediction required. For a default
#' binomial model the default predictions are of log-odds (probabilities on
#' logit scale). The default, "response", returns predicted probabilities.
#' @param consensus (logical) valid if `newdata` is a `SpatRaster`, whether to
#' produce consensus results obtained by combining the predictions from the
#' collection of selected models. By default consensuses are calculated using
#' the mean, median, a weighted average using the AIC weights, and variance.
#' Default = TRUE.
#'
#' @return
#' A list with predictions of selected models on the `newdata` and fitted
#' selected model(s). Consensus predictions are added if multiple selected
#' models exits and if `newdata` is a `SpatRaster` object.
#'
#' @export
#'
#' @importFrom terra rast
#' @importFrom stats var median
#'
#' @examples
#' # Load a fitted selected model
#' data(sel_fit, package = "enmpa")
#'
#' # Load raster layers to be projected
#' env_vars <- terra::rast(system.file("extdata", "vars.tif", package = "enmpa"))
#'
#' # Predictions (only one selected mode, no consensus required)
#' preds <- predict_selected(sel_fit, newdata = env_vars, consensus = FALSE)
#'
#' # Plot prediction
#' terra::plot(preds$predictions)

predict_selected <- function(fitted, newdata, clamping = FALSE,
                             var_to_clamp = NULL, type = "response",
                             consensus = TRUE) {

  if (missing(fitted)) {
    stop("Arguments 'fitted' must be defined.")
  }
  if (missing(newdata)) {
    stop("Arguments 'newdata' must be defined.")
  }

  # separate parts
  selected <- fitted$selected
  fitted$selected <- NULL

  # Obtain the predicted values (p) for each selected model
  p <- lapply(fitted, function(y) {
    predict_glm(y, newdata, clamping = clamping, var_to_clamp = var_to_clamp,
                type = type)
  })

  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
  }

  names(p) <- names(fitted)

  # Consensus obtained by combining the forecasts from
  # the selected models.
  if (consensus  && length(fitted) > 1 &&
      class(newdata)[1] == "SpatRaster") {
    cons_p <- consensus_p(predictions = p, weights = selected$AIC_weight)
    out <- list(predictions = p, consensus = cons_p)
    return(out)

  } else {
    out <- list(predictions = p)
    return(out)
  }
}

