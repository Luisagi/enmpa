#' Evaluate final models using independent data
#'
#' @description
#' Final evaluation steps for model predictions using an independent dataset
#' (not used in model calibration).
#'
#' @usage
#' independent_eval1(prediction, threshold, test_prediction = NULL,
#'                   lon_lat = NULL)
#'
#' @param prediction (numeric) vector or `SpatRaster` object. If numeric,
#' predicted values in independent data (for \code{\link{independent_eval01}}),
#' or the entire area of prediction (for \code{\link{independent_eval1}}).
#' If `SpatRaster` prediction over the area of interest.
#' @param threshold (numeric) the lowest predicted probability value for an
#' occurrence point. This value must be defined for presences-only data.
#' Default = NULL.
#' @param test_prediction (numeric) vector of predictions for independent data.
#' Default = NULL.
#' @param lon_lat matrix or data.frame of coordinates (longitude and latitude,
#' in that order) of independent data. Points must be located within the valid
#' area of `prediction`. For \code{\link{independent_eval01}} they
#' must correspond with values in observation.
#'
#' @return
#' A data.frame or list containing evaluation results.
#'
#' @export
#'
#' @rdname independent_eval
#'
#' @importFrom terra extract
#'
#' @examples
#' # Independent test data based on coordinates (lon/lat WGS 84) from presence
#' # and absences records
#' data("test", package = "enmpa")
#' head(test)
#'
#' # Loading a model prediction
#' pred <- terra::rast(system.file("extdata", "proj_out_wmean.tif",
#'                                 package = "enmpa"))
#' terra::plot(pred)
#'
#' # Evaluation using presence-absence data
#' independent_eval01(prediction = pred, observation = test$Sp,
#'                    lon_lat = test[, 2:3])
#'
#' # Evaluation using presence-only data
#' test_p_only <- test[test$Sp == 1, ]
#' th_maxTSS   <- 0.1274123            # threshold based on the maxTSS
#'
#' independent_eval1(prediction = pred, threshold = th_maxTSS,
#'                   lon_lat = test_p_only[, 2:3])

independent_eval1 <- function(prediction, threshold, test_prediction = NULL,
                              lon_lat = NULL) {

  # initial test
  if (missing(prediction) | missing(threshold)) {
    stop("Arguments 'prediction' and 'threshold' must be defined.")
  }
  if (threshold <= 0 | threshold >= 1) {
    stop("Threshold value must be within the range 0-1.")
  }
  if (class(prediction)[1] == "SpatRaster" & is.null(lon_lat)) {
    stop("Argument 'lonlat' is required if 'prediction' is a 'SpatRaster'.")
  }
  if (class(prediction)[1] == "numeric" & is.null(test_prediction)) {
    stop("Argument 'test_prediction' is required if 'prediction' is a 'numeric' vector.")
  }

  if (class(prediction)[1] == "SpatRaster") {
    if (!class(lon_lat)[1] %in% c("matrix", "data.frame")) {
      stop("'lonlat' must be a 'matrix' or a 'data.frame'.")
    }
    # transform to a Spatvector object and extract predicted values
    prd <- terra::extract(prediction, as.matrix(lon_lat))[, 1]
  } else {
    prd <- test_prediction
  }

  # Omission error (false negative rate)
  oe <- 1 - sum(prd >= threshold) / length(prd)

  # pROC
  proc <- proc_enm(test_prediction = prd, prediction = prediction,
                   threshold = round(threshold * 100, 2))

  # results
  oe_out <- data.frame(omission_error = oe,
                       threshold = threshold,
                       Mean_AUC_ratio = proc$pROC_summary[[1]],
                       pval_pROC = proc$pROC_summary[[2]], row.names = NULL)

  return(oe_out)
}

#'
#' @rdname independent_eval
#' @param observation (numeric) vector of observed (known) values of presence
#' or absence to test against `prediction` (if numeric) or values of prediction
#' in `lon_lat`.
#' @export
#'

independent_eval01 <- function(prediction, observation, lon_lat = NULL) {

  # initial test
  if (missing(prediction) | missing(observation)) {
    stop("Arguments 'prediction' and 'observation' must be defined.")
  }
  if (!all(observation %in% c(0, 1))) {
    stop("'observation' contain non-valid values.")
  }
  if (class(prediction)[1] == "SpatRaster" & is.null(lon_lat)) {
    stop("Argument 'lonlat' is required if 'prediction' is a 'SpatRaster'.")
  }

  if (class(prediction)[1] == "SpatRaster") {
    if (!class(lon_lat)[1] %in% c("matrix", "data.frame")) {
      stop("'lonlat' must be a 'matrix' or a 'data.frame'.")
    }
    # transform to a Spatvector object and extract predicted values
    prediction <- terra::extract(prediction, as.matrix(lon_lat))[, 1]
  }

  # evaluation
  metrics <- optimize_metrics(actual = observation, predicted = prediction,
                              n_threshold = 1000)

  return(metrics$optimized)
}
