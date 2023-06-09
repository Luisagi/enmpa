#' Model Prediction
#'
#' @description
#'  Obtains predictions from a fitted generalized linear model object. It also
#'  allows the clamping option to avoid extrapolation in areas outside the
#'  calibration area.
#'
#' @param model a GLM fitted object
#' @param newdata a data.frame or matrix with the new data to project the
#' predictions.
#' @param clamping Clamp values to a minimum and maximum value, that are
#' establish for the max and min values of the calibration limits.
#' @param type the type of prediction required. For a default binomial model
#' the default predictions are of log-odds (probabilities on logit scale)
#' and type = "response" gives the predicted probabilities.
#'
#' @return a SpatRaster object or a vector o probabilities.
#'
#' @export
#' @importFrom terra as.data.frame predict setValues
#' @importFrom stats coef predict.glm
#'

predict_glm <- function(model, newdata, clamping = TRUE,
                        type = "response") {

  # initial test
  if(missing(model) | missing(newdata)){
    stop("'model' or 'newdata' must be defined.")
  }

  if (!is.null(newdata)) {
    if (!class(newdata)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'newdata' must be of class 'matrix', 'data.frame' or 'SpatRaster'")
    }
  }

  ## clamping to the calibration limits

  if (clamping) {

    if (class(newdata)[1] == "SpatRaster") {
      newdata_clam <- terra::as.data.frame(newdata, na.rm = FALSE)
      } else {
        newdata_clam <- newdata
        }

    # It gets only the variable names used in the fitted model
    modelp <- sapply(colnames(model$data), grepl, names(coef(model)[-1]))
    vnames <- colSums(modelp) > 0

    # min and max limit of the calibration data
    cal_data <- model$data[, vnames]
    varmin <- apply(cal_data, 2, FUN = min)
    varmax <- apply(cal_data, 2, FUN = max)

    for (v in intersect(names(varmax), names(newdata_clam))) {
      newdata_clam[,v] <- pmin(pmax(newdata_clam[,v], varmin[v]), varmax[v])
    }


    if (class(newdata)[1] == "SpatRaster") {
    newdata <- setValues(newdata, newdata_clam)
    } else{
    newdata <- newdata_clam
    }
  }

  ### Prediction

  if (class(newdata)[1] == "SpatRaster") {
    out <- terra::predict(newdata, model,  type = "response")
  } else{
    out <- predict.glm(model, newdata, type = "response")
  }

  return(out)
}
