evaluation_stats <- function(evaluation_results) {
  toagg <- colnames(evaluation_results)[4:ncol(evaluation_results)]

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
  stats <- cbind(xy[[1]][, 1:2] , stats[, -c(16, 18)])
  colnames(stats)[c(17, 18)] <- c("Parameters", "AIC")
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
  if (missing(evaluation_stats) ) {
    stop("Argument 'evaluation_stats' must be defined.")
  }
  if (!criterion %in% c("TSS", "ESS")) {
    stop("'criterion' must be 'TSS' or 'ESS'.")
  }
  if (!is.numeric(tolerance)) {
    stop("Argument 'tolerance' must be of class numeric.")
  }

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
    sel1 <- sel[sel$Threshold_criteria == "maxTSS", ]

    sel1 <- sel1[sel1$TSS_mean >= 0.4, ]

    if (nrow(sel) == 0) {
      warning("No candidate model met the 'TSS >= 0.4' criterion.",
              "\nModels with 'TSS values >= (maximum TSS - ",  tolerance,
              ")' will be used.")

      sel1 <- sel[sel$TSS_mean >= (max(sel$TSS_mean) - tolerance), ]
    }
  } else {
    sel1 <- sel[sel$Threshold_criteria == "ESS", ]

    sel1 <- sel1[sel1$Accuracy >= (max(sel1$Accuracy) - tolerance), ]
  }

  tryCatch({
    # delta AIC for filtered models
    sel1$Delta_AIC <- sel1$AIC - min(sel1$AIC, na.rm = TRUE)
    sel1 <- sel1[sel1$Delta_AIC <= 2, ]

    # weight of AIC selected models
    sel1$AIC_weight <- exp(-0.5 * sel1$Delta_AIC)
    sel1$AIC_weight <- sel1$AIC_weight / sum(sel1$AIC_weight, na.rm = TRUE)

    rownames(sel1) <- 1:nrow(sel1)

  }, error = function(e) {

    # error message
    message_error <- paste0("No model passed selection criteria,",
                            "try increasing 'tolerance'.",
                            "\nCurrent 'tolerance' = ", tolerance)
    message(message_error)

    # Default value for no candidate model met the
    sel1 <- NULL
  })


  # # delta AIC for filtered models
  # sel$Delta_AIC <- sel$AIC - min(sel$AIC, na.rm = TRUE)
  # sel <- sel[sel$Delta_AIC <= 2, ]
  #
  # # weight of AIC selected models
  # sel$AIC_weight <- exp(-0.5 * sel$Delta_AIC)
  # sel$AIC_weight <- sel$AIC_weight / sum(sel$AIC_weight, na.rm = TRUE)
  #
  # try(rownames(sel) <- 1:nrow(sel))


  return(sel1)
}



contribution_formulas <- function(data, dependent, independent, weights = NULL,
                                  type = "lqp", range = c(0.0001, 0.2),
                                  by = 0.0001) {

  f <- get_formulas(dependent = dependent, independent = independent,
                    type = type, all_combinations = FALSE)

  allfit <- glm(as.formula(f), data = data, family = binomial(link = "logit"),
                weights = weights)

  av <- anova(allfit, test = "Chisq")
  preds <- rownames(av)
  devs <- av$Deviance[-1]

  sequ <- seq(range[1], range[2], by = by)

  norm_dev <- seq(0, max(devs), length.out = 100)

  forms <- lapply(sequ, function(x) {
    thres <- quantile(norm_dev, x)

    pick <- devs > thres

    pred <- preds[-1][pick]

    paste(dependent, "~", paste(pred, collapse = " + "))
  })

  forms <- unique(unlist(forms))
  forms <- forms[length(forms):1]

  return(forms)
}


# jackknife_selection <- function(dependent, independent, data, weights,
#                                 type = "lpq", aic_limit = 2){
#
#   ff <- get_formulas(dependent, independent, type = type, all_combinations = F)
#
#   aux <- gsub(paste0(dependent," ~ "), "", ff)
#   features <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))
#
#   comb <- utils::combn(features, length(features) - 1)
#
#   withon <- sapply(1:length(features), function(x) {
#     get_formulas(dependent, rev(features)[x], type = "l", all_combinations = F)
#   })
#
#   without <- sapply(1:length(features), function(x) {
#     get_formulas(dependent, comb[, x], type = "l", all_combinations = F)
#   })
#
#
#   metrics <- c("ROC_AUC", "TSS", "AIC")
#
#   full <- model_validation(ff, data = sdmdata, weights = myweights)[2, metrics]
#
#   withon <- lapply(withon, function(x) {
#     model_validation(x, data = sdmdata, weights = myweights)[2, metrics]
#   })
#
#   without <- lapply(without, function(x) {
#     model_validation(x, data = sdmdata, weights = myweights)[2, metrics]
#   })
#
#   withon <- do.call(rbind, withon)
#   without <- do.call(rbind, without)
#
#   rownames(withon) <- features
#   rownames(without) <- features
#
#   aucs <- rbind(withon$ROC_AUC, without$ROC_AUC)
#   tsss <- rbind(withon$TSS, without$TSS)
#   aics <- rbind(withon$AIC, without$AIC)
#
#   colnames(aucs) <- features
#   rownames(aucs) <- c("with_only", "without")
#
#   colnames(tsss) <- features
#   rownames(tsss) <- c("with_only", "without")
#
#   colnames(aics) <- features
#   rownames(aics) <- c("with_only", "without")
#
#   auc_nos <- aucs[2, ] > 0.5
#   auc_nos <- names(auc_nos)[auc_nos < 0]
#
#   tss_dif <- full$TSS - tsss[2, ]
#   tss_dif <- names(tss_dif)[tss_dif < 0]
#
#   aic_dif <- full$AIC - aics[2, ]
#   aic_dif <- names(aic_dif)[aic_dif > aic_limit]
#
#   to_reduce <- union(union(auc_nos, tss_dif), aic_dif)
#   to_reduce1 <- intersect(union(auc_nos, tss_dif), aic_dif)
#
#   to_keep <- features[!features %in% to_reduce]
#   to_keep1 <- features[!features %in% to_reduce1]
#
#   return(list(
#     full_model = full,
#     without = without,
#     with_only = withon,
#     detrimental_auc = auc_nos,
#     detrimental_tss = tss_dif,
#     detrimental_aic = aic_dif,
#     detrimental_union = to_reduce,
#     detrimental_intersection = to_reduce1,
#     to_keep_union = to_keep,
#     to_keep_intersection = to_keep1))
#
# }



