#' Evaluate final models using an independent dataset.
#'
#'@description
#' The function enables a final evaluation of model predictions using a dataset
#' that is independent of the one used for model calibration. The function is
#' designed to detect and calculate different evaluation metrics depending on
#' the type of occurrence data available: presences-only or presences-absences.
#'
#' @param data data.frame or matrix with the occurrence data and spatial
#' coordinates.
#' @param prediction `SpatRaster` object of a model prediction.
#' @param threshold (numeric) the lowest predicted probability value for an
#' occurrence point. This value must be defined for presences-only data.
#' Default = NULL.
#' @param crs (character) any Coordinate Reference System (CRS)
#' that \code{\link[terra]{vect}} function from `terra` accepts.
#' @param occ (character) column name of the `data` containing the
#' occurrence data.
#' @param xy  (character) vector with the field names associated with the
#' geometry data, representing the x-y coordinates. Default = c("lon", "lat").
#'
#' @return data.frame or list containing evaluation results.
#'
#' @examples
#' # Independent test data based on coordinates (lon/lat WGS 84) from  presence
#' # and absences records
#' test <- read.csv(system.file("extdata", "test_data.csv", package = "enmpa"))
#' head(test)
#'
#' # Loading a model prediction
#' pred <- terra::rast(system.file("extdata", "proj_out_wmean.tif", package = "enmpa"))
#' terra::plot(pred)
#'
#' # Evaluation using presence-absence data
#' independent_eval(data = test, prediction = pred, occ = "Sp",
#'                  crs = "EPSG:4326", xy = c("lon", "lat"))
#'
#' # Evaluation using presence-only data
#' test_p_only <- test[test$Sp == 1, ]
#' th_maxTSS   <- 0.1274123            # threshold based on the maxTSS
#'
#' independent_eval(data = test_p_only, prediction = pred, threshold = th_maxTSS,
#'                  occ = "Sp", crs = "EPSG:4326", xy = c("lon", "lat"))
#'
#' @export
#'
#' @importFrom terra same.crs vect



independent_eval <- function(data, prediction, threshold = NULL,
                             crs , occ = "sp", xy = c("lon", "lat")){

  # initial test
  if (missing(data) | missing(prediction) | missing(crs)) {
    stop("All requiere arguments must be defined.")
  }

  if (!class(data)[1] %in% c("matrix", "data.frame")) {
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
  idata <- terra::vect(data, geom = xy, crs  = crs)

  obs <- data.frame(idata)[, occ]
  prd <- terra::extract(prediction, idata, ID = FALSE)[[1]]


  # Check if the occurrence data contains only 1
  if (all(obs == 1)) {
    message("The occurrence data contains only presences.")

    if (is.null(threshold)) {
      message("A threshold value must be defined by the user.")

    } else if (!(threshold >= 0 & threshold <= 1)) {
      message("Threshold value must be in the range 0-1.")

    } else {

      # Omission error (false negative rate)
      prd_bin <- ifelse(prd >= threshold, 1, 0)

      fn <- sum(obs == 1 & prd_bin == 0) # false positives
      tp <- sum(obs == 1)                # total real positives
      oe <- fn / tp                      # Omission error

      # pROC
      proc <- proc_enm(test_prediction = prd, prediction = prediction)


      oe_out <- data.frame(omission_error = oe,
                           threshold = threshold,
                           Mean_AUC_ratio_at_5 = proc$pROC_summary[[1]],
                           pval_pROC = proc$pROC_summary[[2]])

      return(oe_out)

    }

    # Check if the occurrence data contains 0 and 1.
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
