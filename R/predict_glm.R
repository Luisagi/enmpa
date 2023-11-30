#' Extension of glm predict to generate predictions of different types
#'
#' @description
#' Obtains predictions from a fitted generalized linear model objects. It also
#' allows the clamping option to restrict extrapolation in areas outside the
#' calibration area.
#'
#' @param model a `glm` object.
#' @param newdata a data.frame or matrix with the new data to project the
#' predictions.
#' @param clamping (logical) whether to clamp values to a minimum and maximum
#' value, that are established for the max and min values within calibration
#' values. Default = FALSE.
#' @param var_to_clamp (character) a vector containing the names of the variables
#' that will undergo clamping. By default, if no specific names are provided,
#' the value is set to NULL, which indicates that clamping will be applied to
#' all variables. Ignore if clamping = FALSE.
#' @param type (character) the type of prediction required. For a default
#' binomial model the default predictions are of log-odds (probabilities on
#' logit scale). The default, "response", returns predicted probabilities.
#'
#' @return
#' A `SpatRaster` object or a vector with predictions.
#'
#' @export
#' @importFrom terra as.data.frame predict setValues
#' @importFrom stats coef predict.glm
#'
#' @examples
#' # Load fitted model
#' data("sel_fit", package = "enmpa")
#'
#' # Load raster layers to be projected
#' env_vars <- terra::rast(system.file("extdata", "vars.tif", package = "enmpa"))
#'
#' # Prediction
#' pred <- predict_glm(sel_fit$ModelID_7, newdata = env_vars)
#' terra::plot(pred)

predict_glm <- function(model, newdata, clamping = FALSE, var_to_clamp = NULL,
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

  # clamping to the calibration limits
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

    if (!is.null(var_to_clamp)){

      if (!is.vector(var_to_clamp) | any(!var_to_clamp %in% names(newdata_clam))){
        stop("'var_to_clamp' must provide a vector with the variables names to be clamped")

      } else {
        # Perform clamping in custom variables
        for (v in var_to_clamp) {
          newdata_clam[,v] <- pmin(pmax(newdata_clam[,v], varmin[v]), varmax[v])
        }
      }

    } else {

      # Perform clamping in all variables
      for (v in intersect(names(varmax), names(newdata_clam))) {
        newdata_clam[,v] <- pmin(pmax(newdata_clam[,v], varmin[v]), varmax[v])
      }
    }

    if (class(newdata)[1] == "SpatRaster") {
      newdata <- setValues(newdata, newdata_clam)
    } else{
      newdata <- newdata_clam
    }
  }

  # Prediction
  if (class(newdata)[1] == "SpatRaster") {
    out <- terra::predict(newdata, model,  type = "response")
  } else {
    out <- predict.glm(model, newdata, type = "response")
  }

  return(out)
}


