#' Constructor of S3 objects of class enmpa_calibration
#'
#' @name enmpa_calibration
#' @aliases enmpa_calibration new_enmpa_calibration
#'
#' @param selected date.frame with information about selected models.
#' @param summary data.frame a summary of statistics for all models.
#' @param calibration_results data.frame with results obtained from
#' cross-validation for all models.
#' @param data data.frame or matrix with the input data used for calibration.
#' @param partitioned_data a list of partition indices.
#' @param weights (numeric) a vector with the weights for observations.
#' Default = NULL.
#'
#' @return
#' An S3 object of class \code{enmpa_calibration}.
#'
#' @export
#'
#' @usage
#' new_enmpa_calibration(selected, summary, calibration_results, data,
#'                       partitioned_data, weights = NULL)

new_enmpa_calibration <- function(selected, summary, calibration_results, data,
                              partitioned_data, weights = NULL) {
  structure(
    list(
      selected = selected,
      summary = summary,
      calibration_results = calibration_results,
      data = data,
      partitioned_data = partitioned_data,
      weights = weights
    ),
    class = "enmpa_calibration"
  )
}

#' Constructor of S3 objects of class enmpa_fitted_models
#'
#' @name enmpa_fitted_models
#' @aliases enmpa_fitted_models new_enmpa_fitted_models
#' @param glms_fitted a list of fitted GLMs.
#' @param selected date.frame with information about selected models.
#' @param data data.frame or matrix with the input data used for calibration.
#' @param weights (numeric) a vector with the weights for observations.
#' Default = NULL.
#'
#' @return
#' An S3 object of class \code{enmpa_fitted_models}.
#'
#' @export
#'
#' @usage
#' new_enmpa_fitted_models(glms_fitted, selected, data, weights = NULL)
#'

new_enmpa_fitted_models <- function(glms_fitted, selected, data, weights = NULL) {
  structure(
    list(
      glms_fitted = glms_fitted,
      selected = selected,
      data = data,
      weights = weights
    ),
    class = "enmpa_fitted_models"
  )
}


