#' Predictions for the models selected after calibration
#'
#' @description
#' Wrapper function that facilitates the prediction of those models
#' selected as the most robust. In addition, it allows the calculation of
#' consensus models, when more than one model are selected.
#'
#' @usage
#' predict_selected(glm_calibration, newdata, clamping = FALSE,
#'                  type = "response", consensus = TRUE)
#'
#' @param glm_calibration a list resulting from \code{\link{calibration_glm}}.
#' Predictions are from models in the slot "selected".
#' @param newdata a `SpatRaster`, data.frame, or matrix with the new data on
#' which to predict.
#' @param clamping (logical) this option controls extrapolation when making
#' predictions for environmental conditions beyond the calibration data.
#' Default = FALSE.
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
#' A list with predictions of selected models on the `newdata`. Consensus
#' predictions are added if defined and if `newdata` is a `SpatRaster` object.
#'
#' @export
#'
#' @importFrom terra rast
#' @importFrom stats var median
#'
#' @examples
#' # Load two fitted models
#' data(cal_res, package = "enmpa")
#'
#' # Load raster layers to be projected
#' env_vars <- terra::rast(system.file("extdata", "vars.tif", package = "enmpa"))
#'
#' # Predictions (only one selected mode, no consensus required)
#' preds <- predict_selected(cal_res, newdata = env_vars, consensus = FALSE)
#'
#' # Plot prediction
#' terra::plot(preds$predictions)

predict_selected <- function(glm_calibration, newdata, clamping = FALSE,
                             type = "response", consensus = TRUE,
                             consensus_weights = NULL) {

  if (missing(glm_calibration)) {
    stop("Arguments 'glm_calibration' must be defined.")
  }
  if (missing(newdata)) {
    stop("Arguments 'newdata' must be defined.")
  }

  # fitting selected models
  x <- fit_selected(glm_calibration)

  # Obtain the predicted values (p) for each selected model
  p <- lapply(x, function(y) {
    predict_glm(y, newdata, clamping = clamping, type = type)
  })

  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
  }

  names(p) <- names(x)

  # Consensus obtained by combining the forecasts from
  # the selected models.
  if (consensus  && length(x) > 1 && class(newdata)[1] == "SpatRaster") {
    cons_p <- consensus_p(predictions = p,
                          weights = glm_calibration$selected$AIC_weight)
    out <- list(predictions = p, consensus = cons_p)
    return(out)

  } else {
    out <- list(predictions = p)
    return(out)
  }
}

