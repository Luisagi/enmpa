#' enmpa: Ecological Niche Modeling using Presence-Absence Data
#'
#' @description
#' `enmpa` contains a set of tools to perform detailed Ecological Niche Modeling
#' using presence-absence data.
#'
#' @details
#' It includes algorithms for data partitioning, model fitting, calibration,
#' evaluation, selection, and prediction. Other functions help to explore model
#' features as such variable response curves and variable importance.
#'
#' @section Main functions in `enmpa`:
#' \code{\link{calibration_glm}}, \code{\link{evaluation_stats}},
#' \code{\link{fit_glms}}, \code{\link{fit_selected}},
#' \code{\link{get_formulas}}, \code{\link{independent_eval1}},
#' \code{\link{kfold_partition}}, \code{\link{model_selection}}
#' \code{\link{model_validation}}, \code{\link{niche_signal}},
#' \code{\link{optimize_metrics}}, \code{\link{predict_glm}},
#' \code{\link{predict_selected}}, \code{\link{response_curve}},
#' \code{\link{resp2var}}, \code{\link{var_importance}},
#' \code{\link{jackknife}}, \code{\link{plot_jk}}
#'
#' @name enmpa

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
"_PACKAGE"
