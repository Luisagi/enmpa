#' Selection of best candidate models considering various criteria
#'
#' @description
#' Applies a series of criteria to select best candidate models.
#'
#' @usage
#' model_selection(evaluation_stats, criterion = "TSS", exclude_bimodal = FALSE,
#'                 tolerance = 0.01)
#'
#' @param evaluation_stats data.frame with the statistics of model evaluation
#' results. These results are the output of the function
#' \code{\link{evaluation_stats}}.
#' @param criterion (character) metric used as the predictive criterion for
#' model selection.
#' @param exclude_bimodal (logical) whether to exclude models in which binomial
#' variable response curves were detected.
#' @param tolerance (numeric)
#'
#' @return
#' A data.frame with one or more selected models.
#'
#' @export
#'
#' @examples
#' # data
#' data("cal_res", package = "enmpa")
#' eval_stats <- cal_res$summary[, -1]
#'
#' # selecting best model
#' selected_mod <- model_selection(eval_stats, exclude_bimodal = TRUE)

model_selection <- function(evaluation_stats, criterion = "TSS",
                            exclude_bimodal = FALSE, tolerance = 0.01) {
  if (missing(evaluation_stats)) {
    stop("Argument 'evaluation_stats' must be defined.")
  }
  if (!criterion %in% c("TSS", "ESS")) {
    stop("'criterion' must be 'TSS' or 'ESS'.")
  }
  if (!is.numeric(tolerance)) {
    stop("Argument 'tolerance' must be of class numeric.")
  }

  # selection based on significance
  if (exclude_bimodal) {
    sel <- evaluation_stats[evaluation_stats$Concave_responses == "", ]

    if (nrow(sel) == 0) {
      message("All models have at least one predictor with concave responses.",
              "\nReturning selected_models = NULL.")
      return(NULL)
    }

    sel <- sel[sel$ROC_AUC_mean > 0.5, ]

  } else {
    sel <- evaluation_stats[evaluation_stats$ROC_AUC_mean > 0.5, ]
  }

  if (nrow(sel) == 0) {
    warning("No candidate model met the 'AUC > 0.5' criterion.",
            "\nModels with maximum AUC values will be used.")
    sel <- evaluation_stats[evaluation_stats$ROC_AUC_mean ==
                              max(evaluation_stats$ROC_AUC_mean), ]
  }

  # intermediate selection based on TSS or Accuracy (filter)
  if (criterion == "TSS") {
    sel <- sel[sel$Threshold_criteria == "maxTSS", ]
    sel1 <- sel[sel$TSS_mean >= 0.4, ]
    if (nrow(sel1) == 0) {
      warning("No candidate model met the 'TSS >= 0.4' criterion.",
              "\nModels with 'TSS values >= (maximum TSS - ",
              tolerance, ")' will be used.")
      sel1 <- sel[sel$TSS_mean >= (max(sel$TSS_mean) -
                                     tolerance), ]
    }
  } else {
    sel <- sel[sel$Threshold_criteria == "ESS", ]
    sel1 <- sel[sel$Accuracy >= (max(sel$Accuracy) - tolerance),
    ]
  }

  tryCatch({
    # delta AIC for filtered models
    sel1$Delta_AIC <- sel1$AIC - min(sel1$AIC, na.rm = TRUE)
    sel1 <- sel1[sel1$Delta_AIC <= 2, ]

    # weight of AIC selected models
    sel1$AIC_weight <- exp(-0.5 * sel1$Delta_AIC)
    sel1$AIC_weight <- sel1$AIC_weight/sum(sel1$AIC_weight,
                                           na.rm = TRUE)
    rownames(sel1) <- 1:nrow(sel1)
  }, error = function(e) {
    message_error <- paste0("No model passed selection criteria,",
                            "try increasing 'tolerance'.", "\nCurrent 'tolerance' = ",
                            tolerance)
    message(message_error)
    sel1 <- NULL
  })

  return(sel1)
}
