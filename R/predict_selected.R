
#' Predict the selected models
#'
#' @param x the object returned by the function "calibration_glm()".
#' @param newdata a `SpatRaster`, data.frame or matrix with the new data to
#' project the predictions.
#' @param clamping `logical`, this option mitigates the risk of extreme
#' extrapolations when making model predictions for environmental conditions
#' beyond the calibrated data range. It employs the marginal values within the
#' calibration area to predict outcomes for more extreme conditions in transfer
#' areas. Default = FALSE.
#' @param type the type of prediction required. For a default binomial model
#' the default predictions are of log-odds (probabilities on logit scale)
#' and type = "response" gives the predicted probabilities.
#' @param consensus `logical`, whether to produce three consensus forecasts
#' (or “ensemble”) obtained by combining the forecasts from the collection the
#' selected models. Consensuses are calculated from the mean, median and weighted
#' mean using the wAIC as the weighting metric. An consensus map of variance
#' is also calculated to measure the amount of variability or disagreement among
#' individuals consensus. Default = TRUE.
#'
#' @return
#' A list that includes a second list of fitting models, an individual model
#' predictions `SpatRaster` object, and a consensus predictions `SpatRaster`
#' object.
#'
#' @export
#'
#' @importFrom stats glm var median
#' @importFrom terra rast

predict_selected <- function(x, newdata, clamping = FALSE,
                             type = "response", consensus = TRUE){

  fs <- x$selected$Formulas

  # model fitting
  fits <- lapply(fs, function(y){
    glm(formula = as.formula(y), family = binomial(link = "logit"),
        data = x$data,
        weights = x$weights)
  })

  names(fits) <- paste0("Model_ID_", rownames(x$selected))

  # Obtain the predicted values (p) for each selected model
  p <- lapply( fits, function(y){
    predict_glm(y, newdata, clamping = clamping, type = type)
    })


  # Name models
  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
    names(p) <- paste0("Model_ID_", row.names(x$selected))

  } else{
    names(p) <- paste0("Model_ID_", row.names(x$selected))
  }

  # Consensus forecasts (or “ensemble”) obtained by combining the forecasts from
  #  the collection of selected models.
  if (consensus){
    cons_p <- consensus_p(predictions = p, weights = x$selected$AIC_weight)

    out <- list(fitted_models = fits, predictions = p, consensus = cons_p)
    return(out)

  } else {

    out <- list(fitted_models = fits, predictions = p)
    return(out)
  }
}

