#' Print a short version of elements in 'calibration' and 'fitted models' objects
#'
#' @name print
#' @aliases print,enmpa_calibration-method print,enmpa_calibration-method
#' @aliases print,enmpa_fitted_models-method print,enmpa_fitted_models-method
#' @param x object of enmpa_fitted_models or enmpa_calibration
#' @param ... additional arguments affecting the summary produced. Ignored in
#' these functions.
#'
#' @export
#' @rdname print

print.enmpa_calibration <- function(x, ...) {
  cat("enmpa-class `enmpa_calibration`:\n")
  cat(paste0(
    "$selected             : Selected models (N = ", nrow(x$selected), ")\n"))
  cat(
    "$summary              : A summary of statistics for all models. \n")
  cat(
    "$calibration_results  : Results obtained from cross-validation for all models. \n")
  cat(
    "$data                 : Data used for calibration. \n")
  cat(paste0(
    "$partitioned_data     : k-fold indexes (k = ", length(x$partitioned_data), ")\n"))
  cat(paste0(
    "$weights              : Use of weights (", !is.null(x$weights), ")\n"))
  invisible(x)

}

#' @export
#' @rdname print

print.enmpa_fitted_models <- function(x, ...) {
  cat("enmpa-class `fitted models`:\n")
  cat(paste0(
    "$glms_fitted    : List of GLMs fitted (N = ", length(x$glms_fitted), ")\n"
  ))
  cat(paste0(
    "$selected       : Selected models (N = ", nrow(x$selected), ")\n"
  ))
  cat(
    "$data           : Data used for calibration. \n"
  )
  cat(paste0(
    "$weights        : Use of weights (", !is.null(x$weights), ")\n"
  ))
  invisible(x)
}


#' Summary of 'calibration' and 'fitted models'
#' @name summary
#' @aliases summary,enmpa_calibration-method summary,enmpa_calibration-method
#' @aliases summary,enmpa_fitted_models-method summary,enmpa_fitted_models-method
#' @param object of class enmpa_calibration or enmpa_fitted_models
#' @param ... additional arguments affecting the summary produced. Ignored in
#' these functions.
#' @export
#' @return
#' A printed summary.
#' @rdname summary

summary.enmpa_calibration <- function(object, ...) {
  cat("\n                     Summary of enmpa_calibration                  \n")
  cat("-------------------------------------------------------------------\n\n")
  cat(paste0(
    "Number of selected models: ", nrow(object$selected), "\n"))
  cat(paste0(
    "Number of partitions: (k = ", length(object$partitioned_data), ")\n"))
  cat(paste0(
    "Weights used: ", ifelse(is.null(object$weights), "No", "Yes"), "\n"))
  cat(
    "Summary of selected models (threshold criteria = ",
    object$selected$Threshold_criteria[1],
    "):\n")
  print(object$selected[,c("ModelID", "ROC_AUC_mean", "Accuracy_mean",
                           "Specificity_mean", "Sensitivity_mean" ,"TSS_mean",
                           "AIC", "Delta_AIC")
                        ]
        )
  invisible(object)
}

#' @export
#' @rdname summary

summary.enmpa_fitted_models <- function(object, ...) {
  cat("\n                     Summary of enmpa_fitted_models                \n")
  cat("-------------------------------------------------------------------\n\n")
  cat(paste0("Number of GLMs fitted: ", length(object$glms_fitted), "\n"))
  cat(paste0("Weights used: ", ifelse(is.null(object$weights), "No", "Yes"), "\n"))
  cat("Formulas of fitted models:\n")

  for (i in (names(object$glms_fitted))){
    y <- object$selected[object$selected$ModelID == i, "Formulas"]
    cat(paste0(i,":\t",y, "\n"))
  }
  invisible(object)
}






