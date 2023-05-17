#' GLM model calibration
#'
#' @param dependent `character`, name of dependent variable.
#' @param data data.frame or matrix of independent variables.
#' @param cv_kfolds `numeric`, number of folds to be using  k-fold
#' cross-validation.
#' @param cformulas a vector of character with the set of formulas to test.
#' Default=NULL.
#' @param type `character`, a character string that must contain "l", "p", "q"
#' or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param parallel `logical`, choose to run on parallel or sequential.
#' Default = NULL.
#' @param ncores `numeric`, number of cores to use. Default = 4.
#' @param seed `numeric`, integer value to specify an initial seed. Default = 1.
#' @param weights a vector with the weights for observations.
#'
#' @return a list of dataframe.
#' @export
#'
#' @importFrom utils txtProgressBar
#' @importFrom stats aggregate sd
#' @importFrom snow makeSOCKcluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom parallel clusterExport
#' @importFrom foreach foreach %dopar%
#'

calibration_GLMs <- function(dependent, data, cv_kfolds = 5, cformulas=NULL,
                             type = "l", parallel = FALSE, ncores = 4,
                             seed = 1, weights = NULL){

  # initial tests
  if (is.null(cformulas) & missing(dependent)) {
    stop("Argument 'respuesta' must be defined if 'cformulas' in NULL.")
  }


  ## 1. Data partitioning: k-Fold Cross-Validation
  k <- cv_kfolds
  data_partition <- kfold_partition(data, k = k, seed = seed)

  ## 2. Formula combination

  if (is.null(cformulas)) {
    message("\nEstimating formulas combinations for evaluation.")

    pnames <- colnames(data)
    t_formulas <- get_formulas(dependent = dependent, independent = pnames,
                              type = type, all_combinations = T)

  } else {
    message("\nUsing user-defined formulas.")

    # Use custom formulas by the user
    t_formulas <- cformulas
  }

  message(paste0("Evaluating a total of ", length(t_formulas), " models.\n"))


  # 3. MAIN FUNCTION ___________________________________________________________

  if (parallel == FALSE) {
    ## SEQUENTIAL RUNNING

    message("Running in Sequential.")

    # time start
    start <- Sys.time()

    ## progress bar
    iterations <- length(t_formulas)
    pb <- utils::txtProgressBar(min = 1, max = iterations, style = 3)

    glm_res <- data.frame() # empty df to store results for each iteration.

    for (i in 1:iterations) {
      utils::setTxtProgressBar(pb, i)

      res <- model_validation(formula = t_formulas[i], data = data,
                              weights = weights,
                              cv = TRUE,
                              partition_index = data_partition)

      # Concatenate by rows each loop iteration
      glm_res <- rbind(glm_res, res)
    }

    # Final time
    time.seq <- Sys.time() - start
    message("\nRunning time: ")
    print(time.seq)

  } else {
    ## PARALLEL RUNNING

    message(paste0("Running in Parallel using ", ncores, " threads."))

    # time start
    start <- Sys.time()

    ## progress bar
    iterations <- length(t_formulas)
    pb <- utils::txtProgressBar(min = 1, max = iterations, style = 3)
    progress <- function(n) { utils::setTxtProgressBar(pb, n)}
    opts <- list(progress = progress)

    ## Make cluster
    cl <- snow::makeSOCKcluster(ncores)
    # export local function
    parallel::clusterExport(cl, c("optimize_metrics", "model_validation"))
    doSNOW::registerDoSNOW(cl)

    glm_res = foreach::foreach(
      i = 1:iterations,
      .combine = "rbind",
      .inorder = FALSE,
      .options.snow = opts ) %dopar% {
        res <- model_validation(formula = t_formulas[i],
                                data = data,
                                weights = weights,
                                cv = TRUE,
                                partition_index = data_partition)
        return(res)
        }

    # Closing cluster
    snow::stopCluster(cl)

    # Final time
    time.seq <- Sys.time() - start
    message("\nRunning time: ")
    print(time.seq)

  }

  # 4. data output rearrangement

  message(paste0("\nResuming data ...\n"))   ### Final steps

  ### Summary stats: mean and SD calculation for each models

  toagg <- colnames(glm_res)[5:ncol(glm_res)]

  xy <- lapply(toagg, function(y) {
    do.call(data.frame,
            aggregate(
              as.formula(paste(y, "~ Formulas + Threshold_criteria")),
              data = glm_res,
              FUN = function(x) c(mean = round(mean(x), 4), sd = round(sd(x),4))
            )
    )
  })

  stats <- do.call(data.frame, lapply(xy, function(y) {y[, 3:4] }))
  colnames(stats) <- do.call(c, lapply(xy, function(y) {colnames(y[, 3:4])}))

  stats <- cbind(xy[[1]][c(1, 2)] , stats[, -c(14, 16)]) # remove sd for AIC and paramenters
  colnames(stats)[c(15, 16)] <- c("parameters", "AIC")


  # recalculate the delta and weighted AIC for the aggregate data
  stats$deltaAIC <- stats$AIC - min(stats$AIC, na.rm = TRUE)
  stats$w.AIC <- (exp(-0.5 * stats$deltaAIC)) / (sum(exp(-0.5 * stats$deltaAIC), na.rm = TRUE))

  stats <- stats[order(stats$Formulas), ]

  ### Best models selection: min ROC AUC > 0.5 & TSS >= 0.4 & deltaAIC < 2

  sel <- stats[stats$ROC_AUC.mean > 0.5 & stats$TSS.mean >= 0.4,]

  if (nrow(sel) == 0) {
    sel <- stats[stats$ROC_AUC.mean > 0.5 & stats$TSS.mean >= max(stats$TSS.mean) - 0.01,]
  }

  sel$deltaAIC <- sel$AIC - min(sel$AIC, na.rm = TRUE)
  sel <- sel[sel$deltaAIC <= 2,]

  # Calculation wAIC of the selected models
  sel$w.AIC <-(exp(-0.5 * sel$deltaAIC)) / (sum(exp(-0.5 * sel$deltaAIC), na.rm = TRUE))


  # Final output
  output <- list( calibration_data = glm_res, summary_stats = stats,
                  best_models_selection = sel )

  return(output)
}



