#' Niche Signal detection using one or multiple variables
#'
#' @description
#' Identifies whether a signal of niche can be detected using one or multiple
#' variables. This is an implementation of the methods developed by
#' Cobos & Peterson (2022) <doi:10.17161/bi.v17i.15985> that
#' focuses on identifying niche signals in presence-absence data.
#'
#' @usage
#' niche_signal(data, condition, variables, method = "univariate",
#'              permanova_method = "mahalanobis", iterations = 1000,
#'              set_seed = 1, verbose = TRUE, ...)
#'
#' @param data matrix or data.frame containing at least the following
#' information: a column representing \code{condition} (positive = 1 or
#' negative = 0), and one or more columns representing environmental variables.
#' @param condition (character) name of the column with numeric information
#' about detection (positive = 1 or negative = 0).
#' @param variables (character) vector of one or more names of columns to be
#' used as environmental variables. If \code{method} = "univariate", only one
#' variable is used; for \code{method} = "permanova", multiple variables can
#' be used.
#' @param method (character) name of the method to be used for niche comparison.
#' Default = "univariate".
#' @param permanova_method (character) name of the dissimilarity index to be
#' used as \code{method} in \code{\link[vegan]{adonis2}}. See all options in
#' \code{\link[vegan]{vegdist}}. Default = "mahalanobis".
#' @param iterations (numeric) number of iterations to be used in analysis.
#' Default = 1000. If \code{method} = "permanova", permutations = iterations - 1.
#' @param set_seed (numeric) integer value to specify a initial seed.
#' Default = 1.
#' @param verbose (logical) whether or not to print messages about the process.
#' Default = TRUE.
#' @param ... other arguments to be passed to \code{\link[vegan]{adonis2}}.
#'
#' @return
#' A list with results from analysis depending on `method`.
#'
#' @export
#'
#' @importFrom vegan adonis2
#' @importFrom stats median sd ecdf
#'
#' @rdname niche_signal
#'
#' @examples
#' # Load species occurrences and environmental data.
#' data("enm_data", package = "enmpa")
#' head(enm_data)
#'
#' # Detection of niche signal using an univariate non-parametric test
#' sn_bio1 <- niche_signal(data = enm_data, variables = "bio_1",
#'                         condition = "Sp", method = "univariate")
#' sn_bio1
#'
#' sn_bio12 <- niche_signal(data = enm_data, variables = "bio_12",
#'                          condition = "Sp", method = "univariate")
#' sn_bio12


niche_signal <- function(data, condition, variables, method = "univariate",
                         permanova_method = "mahalanobis", iterations = 1000,
                         set_seed = 1, verbose = TRUE, ...) {
  # initial tests
  if (!method %in% c("univariate", "permanova")) {
    stop("Argument 'method' not valid, options are 'univariate' and 'permanova'")
  }
  if (missing(variables)) {
    stop("Argument 'variables' must be defined")
  }

  # number of positives
  n_pos <- nrow(data[data[, condition] == 1, ])

  # running analysis
  if (method == "univariate") {
    if (length(variables) > 1) {
      if (verbose) {
        message("More than one element detected in 'variables', analysis",
                "\nwill run with the first variable with method 'univariate'")
      }

      variables <- variables[1]
    }

    res <- niche_signal_univariate(data, condition, variables, iterations,
                                   set_seed, verbose)

  } else {
    if (verbose) {
      message("Number of permutations for permanova = interations - 1")
    }

    res <- niche_signal_permanova(data, condition, variables,
                                  permutations = (iterations - 1),
                                  permanova_method, verbose, ...)
  }

  # preparing final result
  res <- list(
    data = data, modified_data = res$modified_data,
    summary = list(variables = variables, condition = condition,
                   method = method, n_complete = nrow(data),
                   n_positive = n_pos),
    univariate_results = res$analysis_results,
    permanova_results = res$permanova_results
  )

  # returning result
  return(res)
}


#' @rdname niche_signal
#' @param variable (character) name of the column containing data to be used
#' as environmental variable.
#' @export
#' @usage
#' niche_signal_univariate(data, condition, variable, iterations = 1000,
#'                         set_seed = 1, verbose = TRUE)

niche_signal_univariate <- function(data, condition, variable, iterations = 1000,
                                    set_seed = 1, verbose = TRUE) {
  # initial tests
  if (missing(data)) {
    stop("Argument 'data' must be defined")
  }
  if (missing(condition)) {
    stop("Argument 'condition' must be defined")
  }
  if (missing(variable)) {
    stop("Argument 'variable' must be defined")
  }

  # number of infected hosts
  if (verbose) {
    message("Preparing data")
  }
  n_inf <- nrow(data[data[, condition] == 1, ])

  # representation of niches via sd and range
  posit <- data[, condition] == 1
  pa_mean <- mean(data[posit, variable])
  pa_med <- median(data[posit, variable])
  pa_sd <- sd(data[posit, variable])
  pa_range <- diff(range(data[posit, variable]))

  h_mean <- mean(data[, variable])
  h_med <- median(data[, variable])
  h_sd <- sd(data[, variable])
  h_range <- diff(range(data[, variable]))

  # positive cases null distribution of niches
  if (verbose) {
    message("Running analysis...")
  }
  ## mean
  set.seed(set_seed)
  n_mean <- vapply(1:iterations, FUN.VALUE = numeric(1), function(x) {
    mean(sample(data[, variable], n_inf))
  })

  ## median
  set.seed(set_seed)
  n_med <- vapply(1:iterations, FUN.VALUE = numeric(1), function(x) {
    median(sample(data[, variable], n_inf))
  })

  ## sd
  set.seed(set_seed)
  n_sd <- vapply(1:iterations, FUN.VALUE = numeric(1), function(x) {
    sd(sample(data[, variable], n_inf))
  })

  ## range
  set.seed(set_seed)
  n_range <- vapply(1:iterations, FUN.VALUE = numeric(1), function(x) {
    diff(range(sample(data[, variable], n_inf)))
  })

  # helper to calculated p value according to position of observed in null
  p_calc <- function(observed, null_distribution) {
    ef <- ecdf(null_distribution)
    ef(observed)
  }

  # pseudo p values
  pml <- p_calc(pa_mean, n_mean)
  pmdl <- p_calc(pa_med, n_med)
  psl <- p_calc(pa_sd, n_sd)
  prl <- p_calc(pa_range, n_range)

  # hypothesis test
  tm <- ifelse(pml <= 0.025 | pml >= 0.975, "rejected", "accepted")
  tmd <- ifelse(pmdl <= 0.025 | pmdl >= 0.975, "rejected", "accepted")
  ts <- ifelse(psl <= 0.025 | psl >= 0.975, "rejected", "accepted")
  tr <- ifelse(prl <= 0.025 | prl >= 0.975, "rejected", "accepted")

  # position of observed
  qm <- ifelse(tm == "accepted", "indistinct",
               ifelse(pml <= 0.025, "lower", "higher"))
  qmd <- ifelse(tmd == "accepted", "indistinct",
                ifelse(pmdl <= 0.025, "lower", "higher"))
  qs <- ifelse(ts == "accepted", "indistinct",
               ifelse(psl <= 0.025, "lower", "higher"))
  qr <- ifelse(tr == "accepted", "indistinct",
               ifelse(prl <= 0.025, "lower", "higher"))


  # return results
  results <- list(
    data = data,
    summary = list(variable = variable, condition = condition,
                   n_complete = nrow(data), n_positive = n_inf),
    analysis_results = list(
      hypothesis_test = c(h0_mean = tm, h0_median = tmd, h0_SD = ts,
                          h0_range = tr, positive_mean_vs_null = qm,
                          positive_median_vs_null = qmd,
                          positive_SD_vs_null = qs,
                          positive_range_vs_null = qr),
      complete_niche = c(mean = h_mean, median = h_med, SD = h_sd,
                         range = h_range),
      positive_niche = c(mean = pa_mean, median = pa_med, SD = pa_sd,
                         range = pa_range),
      null_results = list(mean = n_mean, median = n_med, SD = n_sd,
                          range = n_range)
    )
  )

  return(results)
}




#' @rdname niche_signal
#' @param permutations number of permutations to be performed.
#' @export
#' @usage
#' niche_signal_permanova(data, condition, variables, permutations = 999,
#'                        permanova_method = "mahalanobis", verbose = TRUE, ...)

niche_signal_permanova <- function(data, condition, variables, permutations = 999,
                                   permanova_method = "mahalanobis",
                                   verbose = TRUE, ...) {
  # initial tests
  if (missing(data)) {
    stop("Argument 'data' must be defined")
  }
  if (missing(condition)) {
    stop("Argument 'condition' must be defined")
  }
  if (missing(variables)) {
    stop("Argument 'variables' must be defined")
  }

  # preparing data
  if (verbose) {
    message("Preparing data")
  }
  n_com <- nrow(data)
  dataihost <- data[data[, condition] == 1, ]
  n_inf <- nrow(dataihost)
  dataihost[, condition] <- 0
  data <- rbind(data, dataihost)
  form <- paste("as.matrix(data[, variables]) ~", condition)

  # running analysis
  if (verbose) {
    message("Running analysis...")
  }
  perm <- vegan::adonis2(as.formula(form), data = data,
                         permutations = permutations,
                         method = permanova_method, ...)

  # results preparation
  results <- list(modified_data = data,
                  summary = list(variables = variables, condition = condition,
                                 n_complete = n_com, n_positive = n_inf),
                  permanova_results = perm)

  return(results)
}
