#' Evaluate final models using an independent dataset.
#'
#'@description
#' The function enables a final evaluation of model predictions using a dataset
#' that is independent of the one used for model calibration.
#'
#' @param indep_data `data.frame`.
#' @param prediction `SpatRaster` object.
#' @param crs `character`, any Coordinate Reference System (CRS)
#' that terra::vect() function accepts.
#' @param occ `character`, column name with the occurrence data.
#' @param xy `character` vector, field names associated with the geometry data,
#' representing the x and y coordinates.
#'
#' @return `data.frame`
#'
#' @export
#'
#' @importFrom terra same.crs vect



indep_evaluation <- function(indep_data, prediction, crs , occ = "sp",
                             xy = c("lon", "lat")){

  # initial test
  if (missing(indep_data) | missing(prediction) | missing(crs)) {
    stop("All requiere arguments must be defined.")
  }

  if (!class(indep_data)[1] %in% c("matrix", "data.frame")) {
    stop("'newdata' must be of class 'matrix', 'data.frame'.")
    }

  if (!class(prediction)[1] %in% c("SpatRaster")) {
    stop("'prediction' must be of class 'SpatRaster'.")
  }

  # CRS checking
  if (!terra::same.crs(prediction, crs)){
    stop("Different Coordinate Reference Systems (CRS).")
  }


  # transform to a Spatvector object and extract predicted values
  idata <- terra::vect(indep_data, geom = xy, crs  = crs)

  obs <- data.frame(idata)[, occ]
  prd <- terra::extract(prediction, idata, ID = FALSE)


  # Check if the occurrence data contains only 1
  if (all(obs == 1)) {
    message("The occurrence data contains only presences.")

    stop("")

    #
    # Code for omission error rate and partial ROC (Marlon).
    #

  } else if (all(obs %in% c(0,1))) {
    message("The occurrence data contains both presences and absences.")

    metrics <- optimize_metrics(actual = obs,
                                predicted = prd,
                                n_threshold = 1000)
    return(metrics$optimized)


  } else {
    stop("The occurrence data must contains '1' and/or '0' to be evaluated.\n")
  }
}

