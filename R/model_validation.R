#' Model validation options
#'
#' @description
#' Model evaluation using entire set of data or a cross-validation approach.
#'
#' @usage
#' model_validation(formula, data, family = binomial(link = "logit"),
#'                  weights = NULL, cv = FALSE, partition_index = NULL,
#'                  k = NULL, n_threshold = 100, keep_coefficients = FALSE,
#'                  seed = 1)
#'
#' @param formula `character`, “expressions” to return a "formula" object class.
#' @param data `data.frame`, data set to create model
#' @param family, a family object for models used by functions such as `glm`.
#' @param weights `numeric` vector with weights for observations.
#' @param cv `logical`, whether to use a cross-validation apprach
#' @param partition_index `list` of indices for cross-validation in k-fold.
#' @param k `numeric`, number of folds for a new k-fold index preparation. Ignored if
#' `partition_index` is defined.
#' @param n_threshold `numeric`, number of threshold values to be used.
#' Default = 100.
#' @param keep_coefficients `logical`, whether to keep model coefficients.
#' Default = FALSE.
#' @param seed `numeric`, seed.
#'
#' @return
#' data.frame
#'
#' @export
#'
#' @importFrom stats as.formula binomial glm predict.glm
#'

model_validation <- function(formula, data, family = binomial(link = "logit"),
                             weights = NULL, cv = FALSE, partition_index = NULL,
                             k = NULL, n_threshold = 100,
                             keep_coefficients = FALSE, seed = 1) {

  # initial test
  if (missing(formula) | missing(data)) {
    stop("Argument 'formula' or 'data' must be defined.")
  }
  if (is.logical(cv)) {
    if (cv) {
      if (is.null(partition_index) & is.null(k)) {
        stop("'partition_index' or 'k' must be defined if 'cv' = TRUE.")
      }
    }
  } else {
    stop("'cv' must be logical.")
  }
  f <- as.formula(formula)

  gfit <- suppressWarnings(
    glm(formula = f, family = family, data = data, weights = weights)
    )

  AIC <- gfit$aic
  nparameters <- length(gfit$coefficients) - 1

  if (cv) {
    ## Cross-validation
    if (is.null(partition_index) & !is.null(k)) {
      partition_index <- kfold_partition(data, k = k, seed = seed)
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
        glm(formula = f, family = family, data = data_train,
                  weights = weights_p)
        )

      # Evaluation using Test Dependent data
      pred_k <- predict.glm(kfit, data_test[, -1], type = "response")
      eval_k <- optimize_metrics(actual = data_test[, 1], predicted = pred_k,
                                 n_threshold = n_threshold)$optimized

      res <- data.frame(Formulas = formula, Kfold = x,
                        eval_k, Parameters = nparameters, AIC = AIC)
      out <- rbind(out, res)
    }

  } else {
    pred_global <- predict.glm(gfit, data[, -1], type = "response")
    eval_global <- optimize_metrics(actual = data[, 1],
                                    predicted = pred_global,
                                    n_threshold = n_threshold)$optimized
    out <- data.frame(Formulas = formula, eval_global, Parameters = nparameters,
                      AIC = AIC)
  }

  if (keep_coefficients) {
    out <- data.frame(out,
                      Concave_responses = detect_concave(gfit$coefficients))
  }

  return(out)
}
