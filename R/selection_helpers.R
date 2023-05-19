evaluation_stats <- function(evaluation_results) {
  toagg <- colnames(evaluation_results)[5:ncol(evaluation_results)]

  xy <- lapply(toagg, function(x) {
    do.call(
      data.frame,
      aggregate(
        as.formula(paste(x, "~ Formulas + Threshold_criteria")),
        data = evaluation_results,
        FUN = function(y) c(mean = round(mean(y), 4), sd = round(sd(y), 4))
      )
    )
  })

  # put summary together
  stats <- do.call(data.frame, lapply(xy, function(y) {y[, 3:4]}))
  colnames(stats) <- unlist(lapply(xy, function(y) {colnames(y[, 3:4])}))

  # remove sd for AIC and paramenters
  stats <- cbind(xy[[1]][, 1:2] , stats[, -c(14, 16)])
  colnames(stats)[c(15, 16)] <- c("Parameters", "AIC")
  colnames(stats) <- gsub(".", "_", colnames(stats), fixed = TRUE)

  # delta and weight of AIC for the aggregated data
  stats$Delta_AIC <- stats$AIC - min(stats$AIC, na.rm = TRUE)
  stats$AIC_weight <- exp(-0.5 * stats$Delta_AIC)
  stats$AIC_weight <- stats$AIC_weight / sum(stats$AIC_weight, na.rm = TRUE)

  # sort by formula
  stats <- stats[order(stats$Formulas), ]

  rownames(stats) <- 1:nrow(stats)

  return(stats)
}



model_selection <- function(evaluation_stats, criterion = "TSS",
                            tolerance = 0.01) {
  # selection
  sel <- evaluation_stats[evaluation_stats$ROC_AUC_mean > 0.5, ]

  if (nrow(sel) == 0) {
    warning("No candidate model met the 'AUC > 0.5' criterion.",
            "\nModels with maximum AUC values will be used.")

    sel <- evaluation_stats[evaluation_stats$ROC_AUC_mean ==
                              max(evaluation_stats$ROC_AUC_mean), ]
  }

  # intermediate selection based on TSS or Accuracy (filter)
  if (criterion == "TSS") {
    sel <- evaluation_stats[evaluation_stats$Threshold_criteria == "maxTSS", ]

    sel <- sel[sel$TSS_mean >= 0.4, ]

    if (nrow(sel) == 0) {
      warning("No candidate model met the 'TSS >= 0.4' criterion.",
              "\nModels with 'TSS values >= (maximum TSS - ",  tolerance,
              ")' will be used.")

      sel <- sel[sel$TSS_mean >= (max(sel$TSS_mean) - tolerance), ]
    }
  } else {
    sel <- evaluation_stats[evaluation_stats$Threshold_criteria == "ESS", ]

    sel <- sel[sel$Accuracy >= (max(sel$Accuracy) - tolerance), ]
  }

  # delta AIC for filtered models
  sel$Delta_AIC <- sel$AIC - min(sel$AIC, na.rm = TRUE)
  sel <- sel[sel$Delta_AIC <= 2, ]

  # weight of AIC selected models
  sel$AIC_weight <- exp(-0.5 * sel$Delta_AIC)
  sel$AIC_weight <- sel$AIC_weight / sum(sel$AIC_weight, na.rm = TRUE)

  rownames(sel) <- 1:nrow(sel)

  return(sel)
}
