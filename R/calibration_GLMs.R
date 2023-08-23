#' GLM calibration with presence-absence data
#'
#' @description
#' Model calibration and selection using presence-absence data and GLMs.
#'
#' @usage
#' calibration_glm(data, dependent, independent, weights = NULL,
#'                 response_type = "l", all_combinations = TRUE,
#'                 minvar = 1, maxvar = NULL, user_formulas = NULL,
#'                 cv_kfolds = 5, seed = 1,n_threshold = 100,
#'                 selection_criterion = "TSS", exclude_bimodal = FALSE,
#'                 tolerance = 0.01, parallel = FALSE, n_cores = NULL,
#'                 verbose = TRUE)
#'
#' @param data data.frame or matrix of independent variables.
#' @param dependent `character`, name of dependent variable.
#' @param independent `character`, vector of name(s) of independent variable(s).
#' @param weights a vector with the weights for observations.
#' @param response_type `character`, a character string that must contain "l",
#' "p", "q" or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param all_combinations `logical`, whether to produce all combinations of
#' formulas according to `response_type`, default = TRUE. FALSE returns only
#' the most complex formula defined in `response_type`.
#' @param minvar `numeric` minimum number of features.
#' @param maxvar `numeric` maximum number of features.
#' @param user_formulas a vector of character with the set of formulas to test.
#' Default = NULL.
#' @param cv_kfolds `numeric`, number of folds to use for k-fold
#' cross-validation exercises. Default = 5.
#' @param seed a seed for k-fold partitioning.
#' @param n_threshold `logical`, number of thresholds to use to produce
#' evaluation metrics. Default = 100,
#' @param selection_criterion `character`, criterion used to select best models,
#' options are "TSS" and "ESS". Default = "TSS".
#' @param exclude_bimodal logical, whether to exclude from selected models those
#' with one or more variable presenting concave responses. Default = FALSE.
#' @param tolerance `numeric`, value to modify the metric used for model filtering
#' for model selection if no models meet initial the consideration. Default = 0.01
#' @param parallel `logical`, whether to run on parallel or sequential.
#' Default = FALSE.
#' @param n_cores `numeric`, number of cores to use. Default = number of free
#' processors - 1.
#' @param verbose `logical`, whether to print messages and show progress bar.
#' Default = TRUE
#'
#' @return
#' A list containing: selected models, a summary of statistics for all models,
#' results obtained in cross-validation for all models, original data used, and
#' weights.
#'
#' @details
#' Model evaluation is done considering the ability to predict presences and
#' absences. Model selection consists of three steps: 1) a first filter to keep
#' the models with ROC AUC >= 0.5 (statistically significant models), 2) a
#' second filter to maintain only models that meet the `selection_criterion`
#' ("TSS": TSS >= 0.4; or "ESS": maximum Accuracy - `tolerance`), and 3) from
#' those, pick the ones with delta AIC <= 2.
#'
#' @export
#'
#' @importFrom utils txtProgressBar
#' @importFrom stats aggregate sd
#' @importFrom snow makeSOCKcluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel clusterExport
#' @importFrom foreach foreach %dopar%

calibration_glm <- function(data, dependent, independent, weights = NULL,
                            response_type = "l", all_combinations = TRUE,
                            minvar=1, maxvar = NULL,  user_formulas = NULL,
                            cv_kfolds = 5, seed = 1, n_threshold = 100,
                            selection_criterion = "TSS", exclude_bimodal = FALSE,
                            tolerance = 0.01, parallel = FALSE, n_cores = NULL,
                            verbose = TRUE) {

  # initial tests
  if (missing(data) | missing(dependent) | missing(dependent)) {
    stop("Arguments 'data', 'dependent', and 'independent' must be defined.")
  }

  if (is.null(user_formulas) & missing(dependent)) {
    stop("Argument 'respuesta' must be defined if 'user_formulas' in NULL.")
  }


  ## 1. Data partitioning: k-Fold Cross-Validation
  k <- cv_kfolds
  data_partition <- kfold_partition(data, k = k, seed = seed)

  ## 2. Formula combination

  if (is.null(user_formulas)) {
    message("\nEstimating formulas combinations for evaluation.")

    user_formulas <- get_formulas(dependent = dependent, independent = independent,
                                  type = response_type, minvar=minvar, maxvar = maxvar,
                                  all_combinations = all_combinations)

  } else {
    message("\nUsing user-defined formulas.")
  }

  if (verbose == TRUE) {
    message("Evaluating a total of ", length(user_formulas), " models.\n")
  }


  # 3. MAIN FUNCTION ___________________________________________________________
  # number of models to test
  iterations <- length(user_formulas)

  if (parallel == FALSE) {
    ## SEQUENTIAL RUNNING
    if (verbose == TRUE) {
      message("Running in Sequential.")
    }

    # time start
    start <- Sys.time()

    ## progress bar
    if (verbose == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = iterations, style = 3)
    }

    glm_res <- data.frame() # empty df to store results for each iteration.

    for (i in 1:iterations) {
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, i)
      }

      res <- model_validation(formula = user_formulas[i],
                              data = data, weights = weights, cv = TRUE,
                              partition_index = data_partition,
                              n_threshold = n_threshold,
                              keep_coefficients = exclude_bimodal)

      # Concatenate by rows each loop iteration
      glm_res <- rbind(glm_res, res)
    }

    # Final time
    time.seq <- Sys.time() - start
    if (verbose == TRUE) {
      message("\nRunning time: ", time.seq)
    }

  } else {
    ## PARALLEL RUNNING
    if (is.null(n_cores)) {
      n_cores <- parallel::detectCores() - 1
    }

    if (verbose == TRUE) {
      message(paste0("Running in Parallel using ", n_cores, " threads."))
    }

    # time start
    start <- Sys.time()

    ## progress bar
    if (verbose == TRUE) {
      pb <- utils::txtProgressBar(min = 1, max = iterations, style = 3)
      progress <- function(n) {
        utils::setTxtProgressBar(pb, n)
      }
      opts <- list(progress = progress)
    } else {
      opts <- NULL
    }


    ## Make cluster
    cl <- snow::makeSOCKcluster(n_cores)
    # export local function
    #parallel::clusterExport(cl, c("optimize_metrics", "model_validation"))
    doSNOW::registerDoSNOW(cl)

    glm_res <- foreach::foreach(
      i = 1:iterations, .combine = "rbind", .inorder = FALSE,
      .options.snow = opts
    ) %dopar% {
      res <- model_validation(formula = user_formulas[i],
                              data = data, weights = weights, cv = TRUE,
                              partition_index = data_partition,
                              n_threshold = n_threshold,
                              keep_coefficients = exclude_bimodal)
      return(res)
    }

    # Closing cluster
    snow::stopCluster(cl)

    # Final time
    time.seq <- Sys.time() - start
    if (verbose == TRUE) {
      message("\nRunning time: ", time.seq)
    }
  }

  # 4. data output rearrangement
  if (verbose == TRUE) {
    message("\nPreparing results...\n")   ### Final steps
  }

  # Summary stats: mean and SD calculation for each model based on folds
  stats <- evaluation_stats(evaluation_results = glm_res,
                            bimodal_toexclude = exclude_bimodal,
                            cv_kfolds = cv_kfolds)

  # selected models
  sel <- model_selection(evaluation_stats = stats,
                         criterion = selection_criterion,
                         exclude_bimodal = exclude_bimodal,
                         tolerance = 0.01)

  # Final output
  output <- list(selected = sel, summary = stats, calibration_results = glm_res,
                 data = data, weights = weights)

  return(output)
}



