#' Model validation options
#'
#' @description
#' Model evaluation using entire set of data and a k-fold cross validation
#' approach. Models are assessed based on discrimination power (ROC-AUC),
#' classification ability (accuracy, sensitivity, specificity, TSS, etc.),
#' and the balance between fitting and complexity (AIC).
#'
#' @usage
#' model_validation(formula, data, family = binomial(link = "logit"),
#'                  weights = NULL, cv = FALSE, partition_index = NULL,
#'                  k = NULL, dependent = NULL, n_threshold = 100,
#'                  keep_coefficients = FALSE, seed = 1)
#'
#' @param formula (character) `expression` to be used as a glm `formula`.
#' @param data data.frame with dependent and independent variables.
#' @param family a `family` object for models used by functions such as `glm`.
#' Default = binomial(link = "logit").
#' @param weights (numeric) vector with weights for observations. Default = NULL.
#' @param cv (logical) whether to use a k-fold cross validation for evaluation.
#' Default = FALSE.
#' @param partition_index list of indices for cross validation in k-fold.
#' Obtained with the function \code{\link{kfold_partition}}. Default = NULL.
#' @param k (numeric) number of folds for a new k-fold index preparation.
#' Ignored if `partition_index` is defined or if `cv` = FALSE. Default = NULL.
#' @param dependent (character) name of dependent variable. Ignore if
#' `cv` = FALSE. Default = NULL.
#' @param n_threshold (numeric) number of threshold values to be used for ROC.
#' Default = 100.
#' @param keep_coefficients (logical) whether to keep model coefficients.
#' Default = FALSE.
#' @param seed (numeric) a seed number. Default = 1.
#'
#' @return
#' A data.frame with results from evaluation.
#'
#' @export
#'
#' @importFrom stats as.formula binomial glm predict.glm
#'
#' @examples
#' # Load species occurrences and environmental data.
#' data("enm_data", package = "enmpa")
#' head(enm_data)
#'
#' # Custom formula
#' form <- c("Sp ~ bio_1 + I(bio_1^2) + I(bio_12^2)")
#'
#' # Model evaluation using the entire set of records
#' model_validation(form, data = enm_data)
#'
#' # Model evaluation using a k-fold cross-validation (k = 3)
#' model_validation(form, data = enm_data, cv = TRUE, k = 3, dependent = "Sp")

model_validation <- function(formula, data, family = binomial(link = "logit"),
                             weights = NULL, cv = FALSE, partition_index = NULL,
                             k = NULL, dependent = NULL, n_threshold = 100,
                             keep_coefficients = FALSE, seed = 1) {

  # initial test
  if (missing(formula) | missing(data)) {
    stop("Argument 'formula' or 'data' must be defined.")
  }
  if (is.logical(cv)) {
    if (cv) {
      if (is.null(partition_index) & (is.null(k) | is.null(dependent))) {
        stop(" If 'partition_index' = NULL, then 'k' and 'dependent' must be defined if 'cv' = TRUE.")
      }
    }
  } else {
    stop("'cv' must be logical.")
  }
  f <- as.formula(formula)

  gfit <- suppressWarnings(
    stats::glm(formula = f, family = family, data = data, weights = weights)
    )

  AIC <- gfit$aic
  Residual_deviance <- gfit$deviance
  nparameters <- length(gfit$coefficients) - 1

  if (cv) {
    ## Cross-validation
    if (is.null(partition_index) & !is.null(k) & !is.null(dependent)) {
      partition_index <- kfold_partition(data, dependent = dependent, k = k,
                                         seed = seed)
    }

    out <- data.frame()

    for (x in 1:length(partition_index)) {

      # Define the train and test data
      data_test <- data[partition_index[[x]], ]
      data_train <- data[-partition_index[[x]], ]

      # extract the corresponding weight values for the k-fold
      if (!is.null(weights)) {
        weights_p <- weights[-partition_index[[x]]]
      } else {
        weights_p <- NULL
      }

      # Fit using training data
      kfit <- suppressWarnings(
        stats::glm(formula = f, family = family, data = data_train,
                   weights = weights_p)
      )

      # Evaluation using Test Dependent data
      pred_k <- stats::predict.glm(kfit, data_test, type = "response")
      eval_k <- optimize_metrics(actual = data_test[, all.vars(f)[1]],
                                 predicted = pred_k,
                                 n_threshold = n_threshold)$optimized

      res <- data.frame(Formulas = formula,
                        Kfold = x,
                        eval_k,
                        Parameters = nparameters,
                        Residual_deviance = Residual_deviance,
                        AIC = AIC)
      out <- rbind(out, res)
    }

  } else {
    pred_global <- stats::predict.glm(gfit, data, type = "response")
    eval_global <- optimize_metrics(actual = data[, all.vars(f)[1]],
                                    predicted = pred_global,
                                    n_threshold = n_threshold)$optimized
    out <- data.frame(Formulas = formula,
                      eval_global,
                      Parameters = nparameters,
                      Residual_deviance = Residual_deviance,
                      AIC = AIC)
  }

  if (keep_coefficients) {
    out <- data.frame(out,
                      Concave_responses = detect_concave(gfit$coefficients))
  }

  return(out)
}

