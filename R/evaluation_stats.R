#' Summary of evaluation statistics for candidate models
#'
#' @description
#' Calculate median and standard deviation of evaluation results for all
#' candidate models considering cross-validation kfolds.
#'
#' @usage
#' evaluation_stats(evaluation_results, bimodal_toexclude = FALSE)
#'
#' @param evaluation_results data.frame model evaluation results. These results
#' are the output of the function \code{\link{model_validation}}.
#' @param bimodal_toexclude (logical) whether models in which binomial variable
#' response curves were detected will be excluded during selection processes.
#'
#' @return
#' A data.frame with the mean and standard deviation for all metrics considering
#' cross-validation kfolds.
#'
#' @export
#'
#' @examples
#' # data
#' data("cal_res", package = "enmpa")
#' all_res <- cal_res$calibration_results[, -1]
#'
#' # statistics for all evaluation results
#' evaluation_stats(all_res, bimodal_toexclude = TRUE)

evaluation_stats <- function(evaluation_results, bimodal_toexclude = FALSE) {

  if (missing(evaluation_results)) {
    stop("Argumet 'evaluation_results' must be defined.")
  }

  cv_kfolds <- max(evaluation_results$Kfold)

  if (bimodal_toexclude) {
    toagg <- colnames(evaluation_results)[4:(ncol(evaluation_results) - 1)]
    bim <- evaluation_results[seq(1, nrow(evaluation_results), (cv_kfolds * 3)),
                              "Concave_responses"]

    agg_formula <- "~ Formulas + Threshold_criteria + Concave_responses"
    msd <- 4:5

  } else {
    toagg <- colnames(evaluation_results)[4:ncol(evaluation_results)]

    agg_formula <- "~ Formulas + Threshold_criteria"
    msd <- 3:4
  }

  # summary stats
  xy <- lapply(toagg, function(x) {
    do.call(
      data.frame,
      aggregate(
        as.formula(paste(x, agg_formula)),
        data = evaluation_results,
        FUN = function(y) c(mean = round(mean(y), 4), sd = round(sd(y), 4))
      )
    )
  })

  # put summary together
  stats <- do.call(data.frame, lapply(xy, function(y) {y[, msd]}))
  colnames(stats) <- unlist(lapply(xy, function(y) {colnames(y[, msd])}))

  # remove sd for Devaince, AIC and parameters
  stats <- cbind(xy[[1]][, 1:2] , stats[, -c(16, 18, 20)])
  colnames(stats)[c(17, 18, 19)] <- c("Parameters", "Deviance", "AIC")
  colnames(stats) <- gsub(".", "_", colnames(stats), fixed = TRUE)


  if (bimodal_toexclude) {
    stats$Concave_responses <- xy[[1]][, 3]
  }

  # sort data by formula (more simple to complex)
  ii <- order(factor(stats$Formulas, levels = unique(evaluation_results$Formulas)))
  stats <- stats[ii,]
  rownames(stats) <- 1:nrow(stats)

  # delta and weight of AIC for the aggregated data
  AICs <- stats[!duplicated(stats$Formulas), "AIC"] # per individual model
  Delta_AIC <- AICs - min(AICs, na.rm = TRUE)
  AIC_weight <- exp(-0.5 * Delta_AIC)
  AIC_weight <- AIC_weight / sum(AIC_weight, na.rm = TRUE)

  stats$Delta_AIC <- rep(Delta_AIC, each = length(unique(stats$Threshold_criteria)))
  stats$AIC_weight <- rep(AIC_weight, each = length(unique(stats$Threshold_criteria)))

  if (bimodal_toexclude) {stats <- data.frame(stats[, c(1:19, 21, 22, 20)])}

  return(stats)
}
