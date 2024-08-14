#' Jackkniffe test for variable contribution
#'
#' @description
#' The Jackknife function providing a detailed reflection of the  impact of each
#' variable on the overall model, considering four difference measures:
#' ROC-AUC, TSS, AICc, and Deviance.
#'
#' @usage
#' jackknife(data, dependent, independent, user_formula = NULL, cv = 3,
#'           response_type = "l", weights = NULL)
#'
#' @param dependent (character) name of dependent variable.
#' @param independent (character) vector of name(s) of independent variable(s).
#' @param user_formula (character) custom formula to test. Default = NULL.
#' @param data data.frame or matrix of data to be used in model calibration.
#' Columns represent dependent and independent variables.
#' @param cv (numeric) number of folds to use for k-fold
#' cross-validation exercises. Default = 3.
#' @param response_type (character) a character string that must contain "l",
#' "p", "q" or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param weights (numeric) a vector with the weights for observations.
#'
#' @return list including model performance metrics (ROC-AUC, TSS, AICc, and
#' deviance) for the complete model, model performance when excluding a specific
#' predictor, and the independent contribution of that predictor to the model.
#'
#' @examples
#'
#' # Load data
#' data("enm_data", package = "enmpa")
#'
#' jk <- jackknife(data = enm_data,
#'                 dependent = "Sp",
#'                 independent = c("bio_1", "bio_12"),
#'                 user_formula = NULL,
#'                 cv = 3, response_type = "lpq")
#' jk
#'
#' # plot JK's results
#' plot_jk(jk, metric = "TSS")
#' plot_jk(jk, metric = "ROC_AUC")
#' plot_jk(jk, metric = "AIC")
#' plot_jk(jk, metric = "Residual_deviance")
#'
#' @export
#'
#' @importFrom utils combn
#'

jackknife <- function(data, dependent, independent, user_formula = NULL, cv = 3,
                      response_type = "l", weights = NULL){

  # initial tests
  if (missing(data) | missing(dependent) | missing(dependent)) {
    stop("Arguments 'data', 'dependent', and 'independent' must be defined.")
  }

  if (is.null(user_formula)) {
    # get full model formula using get_formulas
    ff <- get_formulas(dependent, independent, type = response_type,
                       mode = "complex")
  } else {
    # use user formula
    ff <- user_formula
  }

  aux <- gsub(paste0(dependent," ~ "), "", ff)
  features <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))

  comb <- utils::combn(features, length(features) - 1)

  withon <- sapply(1:length(features), function(x) {
    get_formulas(dependent, rev(features)[x], type = "l", mode = "complex")
  })

  without <- sapply(1:length(features), function(x) {
    get_formulas(dependent, comb[, x], type = "l", mode = "complex")
  })

  metrics <- c("ROC_AUC", "TSS", "AIC", "Residual_deviance")

  full <- model_validation(ff, data = data, weights = weights)[2, metrics]
  rownames(full) <- NULL

  withon_r <- lapply(withon, function(x) {
    model_validation(x, data = data, weights = weights)[2, metrics]
  })

  without_r <- lapply(without, function(x) {
    model_validation(x, data = data, weights = weights)[2, metrics]
  })

  withon_r <- do.call(rbind, withon_r)
  without_r <- do.call(rbind, without_r)

  rownames(withon_r) <- features
  rownames(without_r) <- features


  return(list(Full_model_stats = full,
              Formula = ff,
              Without = without_r,
              With_only = withon_r)
  )
}
