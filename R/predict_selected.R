
#' Predict the selected models
#'
#' @param x the object returned by the function "calibration_glm()".
#' @param newdata a `SpatRaster`, data.frame or matrix with the new data to
#' project the predictions.
#' @param clamping Clamp values to a minimum and maximum value, that are
#' establish for the max and min values of the calibration limits.
#' @param type the type of prediction required. For a default binomial model
#' the default predictions are of log-odds (probabilities on logit scale)
#' and type = "response" gives the predicted probabilities.
#'
#' @return  a `SpatRaster` object or a list of vector with the probabilities.
#' @export
#'

predict_selected <- function(x, newdata, clamping = FALSE, type = "response"){


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

  out <- list(fitted_models = fits, predictions = p)
  return(out)

}

