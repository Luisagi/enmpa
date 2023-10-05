#' Predict the selected models
#'
#' @description
#' Wrapper function that facilitates the prediction of those models
#' selected as the most robust. In addition, it allows the calculation of
#' consensus models, when more than one model are selected.
#'
#' @param x a list of fitting models obtained by \code{\link{fit_glm}}.
#' @param newdata a `SpatRaster`, data.frame or matrix with the new data to
#' project the predictions.
#' @param clamping (logical) this option mitigates the risk of extreme
#' extrapolations when making model predictions for environmental conditions
#' beyond the calibrated data range. It employs the marginal values within the
#' calibration area to predict outcomes for more extreme conditions in transfer
#' areas. Default = FALSE.
#' @param type (character) the type of prediction required. For a default
#' binomial model the default predictions are of log-odds (probabilities on
#' logit scale) and type = "response" gives the predicted probabilities.
#' @param consensus (logical) whether to produce three consensus forecasts
#' (or “ensemble”) obtained by combining the forecasts from the collection the
#' selected models. By default consensuses are calculated from the mean and
#' median. To calculated weighted mean user must provide a weighting metric.
#' It is recommended the wAIC. Additionally, an consensus map of variance
#' is also calculated to measure the amount of variability or disagreement among
#' individuals consensus. Default = TRUE.
#' @param consensus_weights (numeric) vector with the metric to calculate a
#' weighted average consensus model. It is recommended the wAIC.
#'
#' @return
#' An individual model predictions `SpatRaster` object,
#' and a consensus predictions `SpatRaster` object.
#'
#' @examples
#' # Load two fitted models
#' load(system.file("extdata", "glm_fitted.RData", package = "enmpa"))
#'
#' # Load raster layers to be projected
#' env_vars <- terra::rast(system.file("extdata", "vars.tif", package = "enmpa"))
#' terra::plot(env_vars)
#'
#' # Predictions
#' wAICs <- c(0.6248182, 0.3751818) # Akaike weights
#' preds <- predict_selected(x = fits, newdata = env_vars, consensus = TRUE,
#'                           consensus_weights = wAICs)
#'
#' # Predictions of the two selected models in the area of interest
#' terra::plot(preds$predictions)
#'
#' # Predictions of the consensus models using averaging approaches
#' terra::plot(preds$consensus,  mar = c(0, 0, 0, 5))
#'
#' @export
#'
#' @importFrom terra rast
#' @importFrom stats var median

predict_selected <- function(x, newdata, clamping = FALSE,
                             type = "response", consensus = TRUE,
                             consensus_weights = NULL){


  # Obtain the predicted values (p) for each selected model
  p <- lapply( x, function(y){
    predict_glm(y, newdata, clamping = clamping, type = type)
    })


  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
  }

  # Consensus (or “ensemble”) obtained by combining the forecasts from
  # the selected models.

  if (consensus  && length(x) > 1 && class(newdata)[1] == "SpatRaster"){

    cons_p <- consensus_p(predictions = p, weights = consensus_weights)
    out <- list(predictions = p, consensus = cons_p)
    return(out)

  } else {

    out <- list(predictions = p)
    return(out)
  }
}

