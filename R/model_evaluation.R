
model_validation <- function(formula, data, family = binomial(link = "logit"),
                             weights = NULL, cv = FALSE, partition_index = NULL,
                             k = NULL, seed = 1) {

  # initial tests
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


  # Fit GLM model using the whole dataset and calculate a global AIC
  f <- as.formula(formula)
  gfit <- glm(formula = f, family = family, data = data, weights = weights)

  # Extract AIC and the number of parameters
  AIC <- gfit$aic
  nparameters <- length(gfit$coefficients) - 1

  if (cv) {
    ## Cross-validation
    if (is.null(partition_index) & !is.null(k)) {
      partition_index <- kfold_partition(data, k = k, seed = seed)
    }

    out <- lapply(1:length(partition_index), function(x) {
      # Define the train and test data
      data_test  <- data[partition_index[[x]], ]
      data_train <- data[-partition_index[[x]], ]

      # Fit using training data
      kfit <- glm(formula = f, family = family, data = data_train,
                  weights = weights)

      # Evaluation using Test Dependent data
      pred_k <- predict.glm(kfit, data_test[,-1], type = "response")
      eval_k <- optimize_metrics(actual = data_test[, 1],
                                 predicted = pred_k)$optimized

      data.frame(Formulas = formula, Kfold = x, eval_k,
                 Parameters = nparameters, AIC = AIC)

    })

    out <- do.call(rbind, out)

  } else {
    pred_global <- predict.glm(gfit, data[,-1], type = "response")
    eval_global <- optimize_metrics(actual = data[, 1],
                                    predicted = pred_global)$optimized

    out <- data.frame(Formulas = formula, eval_global,
                      Parameters = nparameters, AIC = AIC)
  }

  return(out)
}
