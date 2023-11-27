#' Find threshold values to produce three optimal metrics
#'
#' @description
#' The metrics true skill statistic (TSS), sensitivity, specificity are explored
#' by comparing actual vs predicted values to find threshold values that produce
#' sensitivity = specificity, maximum TSS, and a sensitivity value of 0.9.
#'
#' @usage
#' optimize_metrics(actual, predicted, n_threshold = 100)
#'
#' @param actual (numeric) vector of actual values (0, 1) to be compared to
#' `predicted` values after applying a threshold.
#' @param predicted (numeric) vector of predicted probability values to be
#' thresholded and compared to `actual`.
#' @param n_threshold (numeric) number of threshold values to be used.
#' Default = 100.
#'
#' @return
#' A list containing a data.frame with the resulting metrics for all threshold
#' values tested, and a second data.frame with the results for the threshold
#' values that produce sensitivity = specificity (ESS), maximum TSS (maxTSS),
#' and a sensitivity value of 0.9 (SEN90).
#'
#' @export
#'
#' @examples
#' # example data
#' act <- c(rep(1, 20), rep(0, 80))
#' pred <- c(runif(20, min = 0.4, max = 0.7), runif(80, min = 0, max = 0.5))
#'
#' # run example
#' om <- optimize_metrics(actual = act, predicted = pred)
#' om$optimized

optimize_metrics <- function(actual, predicted, n_threshold = 100) {

  if (missing(actual) | missing(predicted)) {
    stop("Arguments 'actual' and 'predicted' must be defined.")
  }

  # Calculate TPR and FPR for different threshold values
  thresholds <- seq(min(predicted), max(predicted), length.out = n_threshold)

  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))
  spe <- numeric(length(thresholds))
  tss <- numeric(length(thresholds))
  acc <- numeric(length(thresholds))

  for (i in 1:length(thresholds)) {
    tp <- sum(actual & (predicted >= thresholds[i]))
    fn <- sum(actual & (predicted < thresholds[i]))
    tn <- sum(!actual & (predicted < thresholds[i]))
    fp <- sum(!actual & (predicted >= thresholds[i]))

    tpr[i] <- tp / (tp + fn)
    fpr[i] <- fp / (fp + tn)
    spe[i] <- tn / (tn + fp)
    acc[i] <- (tn + tp) / (tp + tn + fp + fn)
    tss[i] <- spe[i] + tpr[i] - 1
  }

  # final stats
  roc_auc <- abs(sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2))

  metrics <- data.frame(Threshold = thresholds, ROC_AUC = roc_auc,
                        False_positive_rate = fpr, Accuracy = acc,
                        Sensitivity = tpr, Specificity = spe, TSS = tss)

  t09 <- tpr == 0.9
  if (sum(t09) == 0) {
    t09 <- tpr == min(tpr[tpr >= 0.9])
  }

  optimized <- rbind(metrics[which.min(abs(tpr - spe)), ][1, ],
                     metrics[which.max(tss), ][1, ], metrics[t09, ][1, ])

  criteria <- c("ESS", "maxTSS", "SEN90")

  optimized <- data.frame(Threshold_criteria = criteria, optimized)

  return(list(optimized = optimized, metrics = metrics))
}
