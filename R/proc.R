#' Partial ROC calculation
#'
#' @description proc applies partial ROC tests to model predictions.
#'
#' @usage
#' proc_enm(test_prediction, prediction, threshold = 5, sample_percentage = 50,
#'          iterations = 500)
#'
#' @param prediction `SpatRaster` or numeric vector of model predictions to be
#' evaluated.
#' @param test_prediction (numeric) vector of model predictions for testing
#' data.
#' @param threshold (numeric) value from 0 to 100 to represent the percentage of
#' potential error (E) that the data could have due to any source of uncertainty.
#' Default = 5.
#' @param iterations (numeric) number of bootstrap iterations to be performed;
#' default = 500.
#' @param sample_percentage (numeric) percentage of testing data to be used in each
#' bootstrapped process for calculating the partial ROC. Default = 50.
#'
#' @return A list with the summary of the results and a data.frame containing
#' the AUC values and AUC ratios calculated for all iterations.
#'
#' @details Partial ROC is calculated following Peterson et al. (2008)
#' <doi:10.1016/j.ecolmodel.2007.11.008>.
#'
#' @importFrom terra as.data.frame minmax
#' @importFrom stats na.omit
#'
#' @useDynLib enmpa, .registration = TRUE
#'
#' @export
#'
#' @examples
#' # Loading a model prediction
#' pred <- terra::rast(system.file("extdata", "proj_out_wmean.tif",
#'                                 package = "enmpa"))
#'
#' # Simulated data
#' test <- runif(100, min = 0.3, max = 0.8)
#'
#' # partial ROC calculation
#' pr <- proc_enm(test, pred, threshold = 5, sample_percentage = 50,
#'                iterations = 500)

proc_enm <- function(test_prediction, prediction, threshold = 5,
                     sample_percentage = 50, iterations = 500) {

  # detecting potential errors, other potential problems tested in code
  if (missing(prediction)) {
    stop("Argument 'prediction' is necessary to perform the analysis.")
  }
  if (missing(test_prediction)) {
    stop("Argument 'test_prediction' is necessary to perform the analysis.")
  }
  c_pred <- class(prediction)[1]
  if (!c_pred %in% c("SpatRaster", "numeric")) {
    stop("'prediction' must be of class 'SpatRaster' or 'numeric'.")
  }
  c_tdat <- class(test_prediction)[1]
  if (c_tdat != "numeric") {
    stop("'test_prediction' must be of class 'numeric'.")
  }

  # preparing data
  ## user data
  if (c_pred == "SpatRaster") {
    mmx <- c(terra::minmax(prediction))
    prediction <- unlist(terra::as.data.frame(prediction))
  } else {
    mmx <- range(prediction, na.rm = TRUE)
    prediction <- stats::na.omit(prediction)
  }

  test_prediction <- stats::na.omit(test_prediction)

  # analysis
  if(mmx[1] == mmx[2]) {
    warning("\nprediction has no variability, pROC will return NA.\n")

    # returning NA if model is too bad
    p_roc <- rep(NA, 2)
    names(p_roc) <- c(paste0("Mean_AUC_ratio_at_", threshold, "%"), "pval_pROC")

    auc_ratios <- rep(NA, 3)
    names(auc_ratios) <- c("Model_partial_AUC", "Random_curve_partial_AUC",
                           "AUC_ratio")

    p_roc_res <- list(pROC_summary = p_roc, pROC_results = auc_ratios)

  } else {
    # preparing inputs for proc
    nprediction <- length(prediction)
    prediction <- c(prediction, test_prediction)
    prediction <- as.numeric(cut(prediction, 500))
    test_prediction <- prediction[(nprediction + 1):length(prediction)]
    prediction <- prediction[1:nprediction]

    ## counting pixels in distinct classes
    classpixels <- as.data.frame(table(prediction), stringsAsFactors = FALSE)
    colnames(classpixels) <- c("value", "count")
    classpixels$value <- as.numeric(classpixels$value)

    classpixels <- classpixels[order(classpixels$value, classpixels$count,
                                      decreasing = TRUE), ]
    csum <- cumsum(classpixels$count)
    classpixels <- data.frame(classpixels,
                               totpixperclass = csum,
                               percentpixels = csum / sum(classpixels$count))
    classpixels <- classpixels[order(classpixels$value), ]

    ## proc parameters
    error_sens <- 1 - (threshold / 100)
    n_data <- length(test_prediction)
    n_samp <- ceiling((sample_percentage / 100) * n_data)

    ## matrix with values according to needs
    big_classpixels <- matrix(rep(classpixels$value, each = n_samp),
                              ncol = length(classpixels$value))

    # P ROC calculation
    partial_AUC <- lapply(1:iterations, function(x) {
      calc_aucDF(big_classpixels, classpixels$percentpixels, test_prediction,
                 n_data, n_samp, error_sens)
    })

    partial_AUC <- do.call(rbind, partial_AUC)

    # filtering
    nona_valproc <- partial_AUC$auc_ratio[!is.na(partial_AUC$auc_ratio)]
    mauc <- mean(nona_valproc)
    proc <- sum(nona_valproc <= 1) / length(nona_valproc)

    p_roc <- c(mauc, proc)
    names(p_roc) <- c(paste0("Mean_AUC_ratio_at_", threshold, "%"), "pval_pROC")

    auc_ratios <- partial_AUC
    names(auc_ratios) <- c("Model_partial_AUC", "Random_curve_partial_AUC",
                           "AUC_ratio")

    p_roc_res <- list(pROC_summary = p_roc, pROC_results = auc_ratios)
  }

  return(p_roc_res)
}



# helper to calculate AUC for partial ROC
calc_aucDF <- function(big_classpixels, fractional_area, test_prediction,
                       n_data, n_samp, error_sens) {

  rowsID <- sample(x = n_data, size = n_samp, replace = TRUE)
  omssion_matrix <- big_classpixels > test_prediction[rowsID]
  sensibility <- 1 - colSums(omssion_matrix) / n_samp

  xyTable <- data.frame(fractional_area, sensibility)
  xyTable <- xyTable[-which(xyTable$sensibility <= error_sens), ]
  xyTable <- xyTable[order(xyTable$fractional_area, decreasing = FALSE), ]

  auc_pmodel <- trap_roc(xyTable$fractional_area, xyTable$sensibility)
  auc_prand <- trap_roc(xyTable$fractional_area, xyTable$fractional_area)
  auc_table <- data.frame(auc_pmodel, auc_prand,
                          auc_ratio = auc_pmodel / auc_prand )
  return(auc_table)
}
