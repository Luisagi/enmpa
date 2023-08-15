calibration_glmb <- function (data, dependent, independent, weights = NULL,
                              response_type = "l", all_combinations = TRUE,
                              minvar = 1, maxvar = NULL, user_formulas = NULL,
                              cv_kfolds = 5, seed = 1, n_threshold = 100,
                              selection_criterion = "TSS", exclude_bimodal = FALSE,
                              tolerance = 0.01, parallel = FALSE, n_cores = NULL,
                              verbose = TRUE) {
  if (missing(data) | missing(dependent) | missing(dependent)) {
    stop("Arguments 'data', 'dependent', and 'independent' must be defined.")
  }
  if (is.null(user_formulas) & missing(dependent)) {
    stop("Argument 'respuesta' must be defined if 'user_formulas' in NULL.")
  }

  k <- cv_kfolds
  data_partition <- kfold_partition(data, k = k, seed = seed)
  if (is.null(user_formulas)) {
    message("\nEstimating formulas combinations for evaluation.")
    user_formulas <- get_formulas(dependent = dependent,
                                  independent = independent, type = response_type,
                                  minvar = minvar, maxvar = maxvar,
                                  all_combinations = all_combinations)
  }
  else {
    message("\nUsing user-defined formulas.")
  }
  if (verbose == TRUE) {
    message("Evaluating a total of ", length(user_formulas),
            " models.\n")
  }
  iterations <- length(user_formulas)
  if (parallel == FALSE) {
    if (verbose == TRUE) {
      message("Running in Sequential.")
    }
    start <- Sys.time()
    if (verbose == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = iterations,
                                  style = 3)
    }
    glm_res <- data.frame()
    for (i in 1:iterations) {
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, i)
      }
      res <- model_validationb(formula = user_formulas[i],
                               data = data, weights = weights, cv = TRUE,
                               partition_index = data_partition,
                               n_threshold = n_threshold,
                               keep_coefficents = exclude_bimodal)
      glm_res <- rbind(glm_res, res)
    }
    time.seq <- Sys.time() - start
    if (verbose == TRUE) {
      message("\nRunning time: ", time.seq)
    }
  }
  else {
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }
    if (verbose == TRUE) {
      message(paste0("Running in Parallel using ", n_cores,
                     " threads."))
    }
    start <- Sys.time()
    if (verbose == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = iterations,
                                  style = 3)
      progress <- function(n) {
        utils::setTxtProgressBar(pb, n)
      }
      opts <- list(progress = progress)
    }
    else {
      opts <- NULL
    }
    cl <- snow::makeSOCKcluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    glm_res <- foreach::foreach(i = 1:iterations, .combine = "rbind",
                                .inorder = FALSE, .options.snow = opts) %dopar%
      {
        res <- model_validationb(formula = user_formulas[i],
                                 data = data, weights = weights, cv = TRUE,
                                 partition_index = data_partition,
                                 n_threshold = n_threshold,
                                 keep_coefficents = exclude_bimodal)
        return(res)
      }
    snow::stopCluster(cl)
    time.seq <- Sys.time() - start
    if (verbose == TRUE) {
      message("\nRunning time: ", time.seq)
    }
  }
  if (verbose == TRUE) {
    message("\nPreparing results...\n")
  }
  stats <- evaluation_statsb(evaluation_results = glm_res,
                             bimodal_toexclude = exclude_bimodal,
                             cv_kfolds = cv_kfolds)
  sel <- model_selectionb(evaluation_stats = stats, criterion = selection_criterion,
                          exclude_bimodal = exclude_bimodal, tolerance = 0.01)
  output <- list(selected = sel, summary = stats, calibration_results = glm_res,
                 data = data, weights = weights)
  return(output)
}






############
model_validationb <- function (formula, data, family = binomial(link = "logit"),
                               weights = NULL, cv = FALSE, partition_index = NULL,
                               k = NULL, n_threshold = 100,
                               keep_coefficents = FALSE, seed = 1) {
  if (missing(formula) | missing(data)) {
    stop("Argument 'formula' or 'data' must be defined.")
  }
  if (is.logical(cv)) {
    if (cv) {
      if (is.null(partition_index) & is.null(k)) {
        stop("'partition_index' or 'k' must be defined if 'cv' = TRUE.")
      }
    }
  }
  else {
    stop("'cv' must be logical.")
  }
  f <- as.formula(formula)
  gfit <- glm(formula = f, family = family, data = data, weights = weights)
  AIC <- gfit$aic
  nparameters <- length(gfit$coefficients) - 1
  if (cv) {
    if (is.null(partition_index) & !is.null(k)) {
      partition_index <- kfold_partition(data, k = k,
                                         seed = seed)
    }
    out <- data.frame()
    for (x in 1:length(partition_index)) {
      data_test <- data[partition_index[[x]], ]
      data_train <- data[-partition_index[[x]], ]
      if (!is.null(weights)) {
        weights_p <- weights[-partition_index[[x]]]
      }
      else {
        weights_p <- NULL
      }
      kfit <- glm(formula = f, family = family, data = data_train,
                  weights = weights_p)
      pred_k <- predict.glm(kfit, data_test[, -1], type = "response")
      eval_k <- optimize_metrics(actual = data_test[, 1], predicted = pred_k,
                                 n_threshold = n_threshold)$optimized
      res <- data.frame(Formulas = formula, Kfold = x,
                        eval_k, Parameters = nparameters, AIC = AIC)
      out <- rbind(out, res)
    }
  }
  else {
    pred_global <- predict.glm(gfit, data[, -1], type = "response")
    eval_global <- optimize_metrics(actual = data[, 1],
                                    predicted = pred_global,
                                    n_threshold = n_threshold)$optimized
    out <- data.frame(Formulas = formula, eval_global, Parameters = nparameters,
                      AIC = AIC)
  }

  if (keep_coefficents) {
    out <- data.frame(out,
                      Concave_responses = detect_concave(gfit$coefficients))
  }

  return(out)
}




############
detect_concave <- function(glm_coefficents) {
  if (missing(glm_coefficents)) {
    stop("Argumet 'glm_coefficents' must be defined.")
  }
  coef <- glm_coefficents
  varname <- names(coef)

  ccoef <- grep("\\^2", varname, value = TRUE)

  if (length(ccoef) == 0) {
    return("")
  } else {
    cccoef <- ccoef[coef[ccoef] >= 0]

    return(paste(cccoef, collapse = ", "))
  }
}







############
evaluation_statsb <- function(evaluation_results, bimodal_toexclude = FALSE,
                              cv_kfolds = NULL) {
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

  # remove sd for AIC and paramenters
  stats <- cbind(xy[[1]][, 1:2] , stats[, -c(16, 18)])
  colnames(stats)[c(17, 18)] <- c("Parameters", "AIC")
  colnames(stats) <- gsub(".", "_", colnames(stats), fixed = TRUE)

  # delta and weight of AIC for the aggregated data
  stats$Delta_AIC <- stats$AIC - min(stats$AIC, na.rm = TRUE)
  stats$AIC_weight <- exp(-0.5 * stats$Delta_AIC)
  stats$AIC_weight <- stats$AIC_weight / sum(stats$AIC_weight, na.rm = TRUE)

  if (bimodal_toexclude) {
    stats$Concave_responses <- xy[[1]][, 3]
  }

  # sort by formula
  stats <- stats[order(stats$Formulas), ]

  rownames(stats) <- 1:nrow(stats)

  return(stats)
}













################

model_selectionb <- function (evaluation_stats, criterion = "TSS",
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
  }
  else {
    sel <- sel[sel$Threshold_criteria == "ESS", ]
    sel1 <- sel[sel$Accuracy >= (max(sel$Accuracy) - tolerance),
    ]
  }

  tryCatch({
    sel1$Delta_AIC <- sel1$AIC - min(sel1$AIC, na.rm = TRUE)
    sel1 <- sel1[sel1$Delta_AIC <= 2, ]
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
