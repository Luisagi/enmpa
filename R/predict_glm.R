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
#' @param data data.frame or matrix of data used in the model calibration step.
#' Default = NULL.
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
#' pred <- predict_glm(sel_fit$glms_fitted$ModelID_7, newdata = env_vars,
#'                     data = sel_fit$data)
#' terra::plot(pred)


predict_glm <- function(model, newdata, data = NULL, extrapolation_type = "E",
                        restricted_vars = NULL, type = "response") {

  # initial test
  if(missing(model) | missing(newdata)){
    stop("'model' or 'newdata' must be defined.")
  }

  if (!is.null(newdata)) {
    if (!class(newdata)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'newdata' must be of class 'matrix', 'data.frame' or 'SpatRaster'")
    }
  }

  if (!extrapolation_type %in% c("E", "NE", "EC")){
    stop("'extrapolation_type' must be of class 'E','NE', or 'EC'")
  }

  # check calibration data availability
  if (is.null(data) && is.null(model$data)){
    stop("Calibration data must be defined.")
  } else {
    if (!is.null(model$data)){
      data  <- model$data
    }
  }

  ## Extrapolation type:

  # E = "Free extrapolation"____________________________________________________
  if (extrapolation_type == "E"){
    # Prediction
    if (class(newdata)[1] == "SpatRaster") {
      out <- terra::predict(newdata, model,  type = type)
    } else {
      out <- predict.glm(model, newdata, type = type)
    }
  }

  # NE = "No extrapolation"_____________________________________________________
  if (extrapolation_type == "NE") {

    if (class(newdata)[1] == "SpatRaster") {
      newdata_aux <- terra::as.data.frame(newdata, na.rm = FALSE)
    } else {
      newdata_aux <- newdata
    }

    # It gets only the variable names used in the fitted model
    modelp <- sapply(colnames(data), grepl, names(coef(model)[-1]))
    vnames <- colSums(modelp) > 0

    # min and max limit of the calibration data
    cal_data <- data[, vnames]
    varmin <- apply(cal_data, 2, FUN = min)
    varmax <- apply(cal_data, 2, FUN = max)

    if (!is.null(restricted_vars)){

      if (!is.vector(restricted_vars) | any(!restricted_vars %in% names(newdata_aux))){
        stop("'restricted_vars' must provide a vector with the variables names to be restricted.")
      } else {
        # no extrapolation for custom variables
        nep <- sapply(restricted_vars, function(v){
          which(newdata_aux[, v] < varmin[v] | newdata_aux[, v] > varmax[v])
        })
        nep <- unique(unname(do.call("c", nep)))
      }
    } else {
      # Clamping for all variables
      nep <- sapply(intersect(names(varmax), names(newdata_aux)), function(v){
        which(newdata_aux[, v] < varmin[v] | newdata_aux[, v] > varmax[v])
      })
      nep <- unique(unname(do.call("c", nep)))
    }

    # Prediction with NE
    if (class(newdata)[1] == "SpatRaster") {
      out <- terra::predict(newdata, model,  type = type)
      out[nep] <- 0
    } else {
      out <- predict.glm(model, newdata, type = type)
      out[nep] <- 0
    }
  }

  # EC = "Extrapolation with clamping"_________________________________________
  if (extrapolation_type == "EC") {
    if (class(newdata)[1] == "SpatRaster") {
      newdata_aux <- terra::as.data.frame(newdata, na.rm = FALSE)
    } else {
      newdata_aux <- newdata
    }

    # It gets only the variable names used in the fitted model
    modelp <- sapply(colnames(data), grepl, names(coef(model)[-1]))
    vnames <- colSums(modelp) > 0

    # min and max limit of the calibration data
    cal_data <- data[, vnames]
    varmin <- apply(cal_data, 2, FUN = min)
    varmax <- apply(cal_data, 2, FUN = max)

    if (!is.null(restricted_vars)){

      if (!is.vector(restricted_vars) | any(!restricted_vars %in% names(newdata_aux))){
        stop("'restricted_vars' must provide a vector with the variables names to be restricted.")
      } else {
        # Clamping for  custom variables
        for (v in restricted_vars) {
          newdata_aux[,v] <- pmin(pmax(newdata_aux[,v], varmin[v]), varmax[v])
        }
      }
    } else {
      # Clamping for all variables
      for (v in intersect(names(varmax), names(newdata_aux))) {
        newdata_aux[,v] <- pmin(pmax(newdata_aux[,v], varmin[v]), varmax[v])
      }
    }
    if (class(newdata)[1] == "SpatRaster") {
      newdata <- setValues(newdata, newdata_aux)
    } else{
      newdata <- newdata_aux
    }
    # Prediction
    if (class(newdata)[1] == "SpatRaster") {
      out <- terra::predict(newdata, model,  type = type)
    } else {
      out <- predict.glm(model, newdata, type = type)
    }
  }
  return(out)
}
