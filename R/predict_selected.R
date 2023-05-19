
#' Predict the selected models
#'
#' @param x the object returned by the function "calibration_glm()".
#' @param newdata a data.frame or matrix with the new data to project the
#' predictions.
#' @param clamping Clamp values to a minimum and maximum value, that are
#' establish for the max and min values of the calibration limits.
#' @param type the type of prediction required. For a default binomial model
#' the default predictions are of log-odds (probabilities on logit scale)
#' and type = "response" gives the predicted probabilities.
#'
#' @return
#' @export
#'
#'
#

predict_selected <- function(x, newdata, clamping = TRUE, type = "response"){


  fs <- x$best_models_selection$Formulas

  # model fitting
  fits <- lapply(fs, function(y){
    glm(formula = as.formula(y), family = binomial,
        data = x$data,
        weights = x$weights)
  })

  # Obtain the predicted values (p) for each selected model
  p <- lapply( fits, function(y){
    enmpa::predict_glm(y, newdata, clamping = clamping, type = type)
    })


  # Name models
  if (class(newdata)[1] == "SpatRaster") {
    p <- terra::rast(p)
    names(p) <- paste0("Model_ID_", row.names(x$best_models_selection))

  } else{
    names(p) <- paste0("Model_ID_", row.names(x$best_models_selection))
  }

  return(p)

}
