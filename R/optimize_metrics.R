# example data
# actual <- c(rep(1, 2000), rep(0, 8000))
# predicted <- c(runif(2000, min = 0.4, max = 0.7),
#                runif(8000, min = 0, max = 0.5))
# n_thr <- 100


optimize_metrics <- function(actual, predicted, n_threshold = 1000) {

  if (missing(actual) | missing(predicted)) {
    stop("Arguments 'actual' and 'predicted' must be defined")
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

  optimized <- rbind(metrics[which.min(abs(tpr - spe)), ][1, ],
                     metrics[which.max(tss), ][1, ],
                     metrics[which.min(abs(tpr - 0.9)), ][1, ])

  return(list(metrics = metrics, optimized = optimized))
}
