#' Predictions for the models selected after calibration
#'
#' @description
#' Wrapper function that facilitates the prediction of those models
#' selected as the most robust. In addition, it allows the calculation of
#' consensus models, when more than one model are selected.
#'
#' @usage
#' predict_selected(fitted, newdata, extrapolation_type = "E",
#'                  restricted_vars = NULL, type = "response", consensus = TRUE)
#'
#' @param fitted an enmpa-class`fitted models` object obtained using the
#' functions \code{\link{fit_selected}}.
#' @param newdata a `SpatRaster`, data.frame, or matrix with the new data on
#' which to predict.
#' @param extrapolation_type (character) to indicate extrapolation type of model.
#' Models can be transferred with three options: free extrapolation ('E'),
#' extrapolation with clamping ('EC'), and no extrapolation ('NE').
#' Default = 'E'.
#' @param restricted_vars (character) a vector containing the names of the
#' variables that will undergo clamping or no extrapolation. For clamping,
#' these variables are set to minimum and maximum values established for the
#' max and min values within calibration values. For no extrapolation, the
#' variables outside calibration limits became NA. If no specific names are
#' provided, the value is set to NULL by default, indicating that clamping (EC)
#' or no extrapolation (NE) will be applied to all variables. Ignore if
#' extrapolation_type = 'E'.
#' @param type (character) the type of prediction required. For a default
#' binomial model the default predictions are of log-odds (probabilities on
#' logit scale). The default, "response", returns predicted probabilities.
#' @param consensus (logical) valid if `newdata` is a `SpatRaster`, whether to
#' produce consensus results obtained by combining the predictions from the
#' collection of selected models. By default consensuses are calculated using
#' the mean, median, variance, and weighted average using the AIC weights.
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

predict_selected <- function(fitted, newdata, extrapolation_type = "E",
                             restricted_vars = NULL, type = "response",
                             consensus = TRUE) {

  if (missing(fitted)) {
    stop("Arguments 'fitted' must be defined.")
  }
  if (missing(newdata)) {
    stop("Arguments 'newdata' must be defined.")
  }

  # Obtain the predicted values (p) for each selected model
  p <- lapply(fitted$glms_fitted, function(y) {
    predict_glm(y, newdata, data = fitted$data,
                extrapolation_type = extrapolation_type,
                restricted_vars = restricted_vars, type = type)
  })

  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
  }

  names(p) <- names(fitted$glms_fitted)

  # Consensus obtained by combining the forecasts from
  # the selected models.
  if (consensus  && length(fitted$glms_fitted) > 1 &&
      class(newdata)[1] == "SpatRaster") {

    # Adjust AIC weights to sum 1
    aicw_dif <- 1 - sum(fitted$selected$AIC_weight)
    fitted$selected$AIC_weight[1] <- fitted$selected$AIC_weight[1] + aicw_dif

    cons_p <- consensus_p(predictions = p, weights = fitted$selected$AIC_weight)
    out <- list(predictions = p, consensus = cons_p)
    return(out)

  } else {
    out <- list(predictions = p)
    return(out)
  }
}

