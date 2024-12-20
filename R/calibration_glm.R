#' GLM calibration with presence-absence data
#'
#' @description
#' Creates candidate models based on distinct parameter settings, evaluates
#' models, and selects the ones that perform the best.
#'
#' @usage
#' calibration_glm(data, dependent, independent, weights = NULL,
#'                 response_type = "l", formula_mode = "moderate",
#'                 minvar = 1, maxvar = NULL, user_formulas = NULL,
#'                 cv_kfolds = 5, partition_index = NULL, seed = 1,
#'                 n_threshold = 100, selection_criterion = "TSS",
#'                 exclude_bimodal = FALSE, tolerance = 0.01,
#'                 out_dir = NULL, parallel = FALSE,
#'                 n_cores = NULL, verbose = TRUE)
#'
#' @param data data.frame or matrix of data to be used in model calibration.
#' Columns represent dependent and independent variables.
#' @param dependent (character) name of dependent variable.
#' @param independent (character) vector of name(s) of independent variable(s).
#' @param weights (numeric) a vector with the weights for observations.
#' @param response_type (character) a character string that must contain "l",
#' "p", "q" or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param formula_mode (character) a character string to indicate the strategy to
#' create the formulas for candidate models. Options are: "light", "moderate",
#' "intensive", or "complex". Default = "moderate". "complex" returns only the
#' most complex formula defined in `response_type`.
#' @param minvar (numeric) minimum number of independent variables in formulas.
#' @param maxvar (numeric) maximum number of independent variables in formulas.
#' @param user_formulas (character) vector  with formula(s) to test.
#' Default = NULL.
#' @param partition_index list of indices for cross-validation in k-fold. The
#' default, NULL, uses the function \code{\link{kfold_partition}}.
#' @param cv_kfolds (numeric) number of folds to use for k-fold
#' cross-validation exercises. Default = 5. Ignored if `partition_index`
#' is defined.
#' @param seed (numeric) a seed for k-fold partitioning.
#' @param n_threshold (logical) number of threshold values to produce
#' evaluation metrics. Default = 100.
#' @param selection_criterion (character) criterion used to select best models,
#' options are "TSS" and "ESS". Default = "TSS".
#' @param exclude_bimodal (logical) whether to filter out models with one or
#' more variables presenting concave responses. Default = FALSE.
#' @param tolerance (numeric) value to modify the limit value of the metric
#' used to filter models during model selection if none of the models meet
#' initial considerations. Default = 0.01
#' @param out_dir (character) output directory name to save the main calibration
#' results. Default = NULL.
#' @param parallel (logical) whether to run on parallel or sequential.
#' Default = FALSE.
#' @param n_cores (numeric) number of cores to use. Default = number of free
#' processors - 1.
#' @param verbose (logical) whether to print messages and show progress bar.
#' Default = TRUE
#'
#' @return
#' An object of the class enmpa_calibration containing: selected models,
#' a summary of statistics for all models, results obtained in cross-validation
#' for all models, original data used, weights, and data-partition indices used.
#'
#' @details
#' Model evaluation is done considering the ability to predict presences and
#' absences,as well as model fitting and complexity. Model selection consists
#' of three steps: 1) a first filter to keep the models with ROC AUC >= 0.5
#' (statistically significant models), 2) a second filter to maintain only
#' models that meet the `selection_criterion` ("TSS": TSS >= 0.4; or "ESS":
#' maximum Accuracy - `tolerance`), and 3) from those, pick the ones with
#' delta AIC <= 2.
#'
#'
#' `formula_mode` options determine what strategy to iterate the predictors
#' defined in \code{type} for creating models:
#' - **light**.-- returns simple iterations of complex formulas.
#' - **moderate**.-- returns a comprehensive number of iterations.
#' - **intensive**.-- returns all possible combination. Very time-consuming for
#' 6 or more independent variables.
#' - **complex**.-- returns only the most complex formula.
#'
#' @examples
#' # Load species occurrences and environmental data.
#' data("enm_data", package = "enmpa")
#' head(enm_data)
#'
#' # Calibration using linear (l), quadratic (q), products(p) responses.
#' cal_res <- calibration_glm(data = enm_data, dependent = "Sp",
#'                            independent = c("bio_1", "bio_12"),
#'                            response_type = "lpq", formula_mode = "moderate",
#'                            selection_criterion = "TSS", cv_kfolds = 3,
#'                            exclude_bimodal = TRUE, verbose = FALSE)
#' print(cal_res)
#' summary(cal_res)
#'
#' head(cal_res$calibration_results)
#' head(cal_res$summary)
#' head(cal_res$selected)
#' head(cal_res$data)
#'
#' @export
#'
#' @importFrom utils txtProgressBar write.table
#' @importFrom stats aggregate sd
#' @importFrom snow makeSOCKcluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel clusterExport
#' @importFrom foreach foreach %dopar%

calibration_glm <- function(data, dependent, independent, weights = NULL,
                            response_type = "l", formula_mode = "moderate",
                            minvar=1, maxvar = NULL,  user_formulas = NULL,
                            cv_kfolds = 5, partition_index = NULL, seed = 1,
                            n_threshold = 100, selection_criterion = "TSS",
                            exclude_bimodal = FALSE,  tolerance = 0.01,
                            out_dir = NULL, parallel = FALSE,
                            n_cores = NULL, verbose = TRUE) {

  # initial tests
  if (missing(data) | missing(dependent) | missing(dependent)) {
    stop("Arguments 'data', 'dependent', and 'independent' must be defined.")
  }

  if (is.null(user_formulas) & missing(dependent)) {
    stop("Argument 'respuesta' must be defined if 'user_formulas' in NULL.")
  }

  ## 1. Data partitioning: k-Fold Cross-Validation
  if (is.null(partition_index)){
    k <- cv_kfolds
    data_partition <- kfold_partition(data, dependent = dependent, k = k,
                                      seed = seed)
  } else {
    k <- length(partition_index)
    data_partition <- partition_index
  }

  ## 2. Formula combination
  if (is.null(user_formulas)) {
    if (verbose == TRUE) {
      message("\nEstimating formulas combinations for evaluation.")
    }
    user_formulas <- get_formulas(dependent = dependent,
                                  independent = independent,
                                  type = response_type, mode = formula_mode,
                                  minvar=minvar, maxvar = maxvar)

  } else {
    if (verbose == TRUE) {
    message("\nUsing user-defined formulas.")
    }
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
    if (verbose == TRUE) {
      elapsed <- difftime(Sys.time(), start, units = "secs")
      format_time(elapsed)
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
    if (verbose == TRUE) {
      elapsed <- difftime(Sys.time(), start, units = "secs")
      format_time(elapsed)
    }
  }

  # 4. data output rearrangement
  if (verbose == TRUE) {
    message("\nPreparing results...\n")   ### Final steps
  }

  # Summary stats: mean and SD calculation for each model based on folds
  stats <- evaluation_stats(evaluation_results = glm_res,
                            bimodal_toexclude = exclude_bimodal)

  # selected models
  sel <- model_selection(evaluation_stats = stats,
                         criterion = selection_criterion,
                         exclude_bimodal = exclude_bimodal,
                         tolerance = 0.01)



  # add data_partition in the input data.
  aux_f <- lapply(
    names(data_partition),
    function(x){data.frame(obs = data_partition[[x]],kfold_ID = x)}
  )

  folds <- do.call(rbind, aux_f)
  folds <- folds[order(folds$obs),]
  data_final <- data.frame(data, kfold_ID = folds$kfold_ID)

  # Add ModelID tag
  fx  <- user_formulas
  mid <- data.frame(ModelID = paste0("ModelID_", 1:length(fx)), Formulas = fx)

  glm_res <- merge(glm_res, mid, by = "Formulas", sort = F)
  glm_res <- glm_res[, c("ModelID", names(glm_res)[names(glm_res) != "ModelID"])]

  stats <- merge(stats, mid, by = "Formulas", sort = F)
  stats <- stats[, c("ModelID", names(stats)[names(stats) != "ModelID"])]

  sel <- merge(sel, mid, by = "Formulas", sort = F)
  sel <- sel[, c("ModelID", names(sel)[names(sel) != "ModelID"])]


  # Final output
  output <- new_enmpa_calibration(
    selected = sel,
    summary = stats,
    calibration_results = glm_res,
    data = data_final,
    weights = weights,
    partitioned_data = data_partition)

  # Save calibration tables
  if (!is.null(out_dir) && is.character(out_dir)){
    save_cal(x = output, out_dir = out_dir)
    }

  return(output)
}



