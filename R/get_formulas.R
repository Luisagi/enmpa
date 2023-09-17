#' Get formulas according to types of responses needed
#'
#' @description
#' Generate forms based on independent variables that can predict the dependent
#' variable, taking into account the required response types. If necessary,
#' calculate all possible combinations of these formulas considering different
#' variable combinations.
#'
#' @usage
#' get_formulas(dependent, independent, type = "l", all_combinations = TRUE,
#'              minvar=1, maxvar = NULL)
#'
#' @param dependent (character) name of dependent variable.
#' @param independent (character) a vector of names of independent variables.
#' @param type (character) a character string that must contain "l", "p", "q"
#' or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param all_combinations (logical) whether to produce all combinations,
#' default = TRUE. FALSE returns only the most complex formula defined in type.
#' @param minvar (numeric) minimum number of features.
#' @param maxvar (numeric) maximum number of features.
#'
#' @return
#' A character vector containing the resulting formula(s).
#'
#' @importFrom utils combn
#'
#' @export
#'
#' @examples
#' # example variables
#' dep <- "presence"
#' ind <- c("temperature", "humidity")
#'
#' # one formula according to "type"
#' formula <- get_formulas(dep, ind, type = "lqp", all_combinations = FALSE)
#'
#' # all combinations according to type
#' formulas <- get_formulas(dep, ind, type = "lqp", all_combinations = TRUE)

get_formulas <- function(dependent, independent, type = "l",
                         all_combinations = TRUE,
                         minvar=1, maxvar = NULL) {

  # initial test
  if (!is.character(dependent) || length(dependent) != 1) {
    stop("'dependent' must be a unique response variable name.")
  }
  if (!is.character(dependent)) {
    stop("'independent' must be a character vector.")
  }
  if (is.character(type)) {
    if (!all(unlist(strsplit(type, "")) %in% c("l", "p", "q"))) {
      stop("'type' must be: 'l', 'p', 'q', or a combination of those three.")
    }
  } else {
    stop("'type' must be of class character.")
  }

  if (!is.logical(all_combinations)) {
    stop("'all_combinations' must be logical (TRUE or FALSE).")
  }

  predictors <- independent
  npred <- length(predictors)

  # remove the response variable if given in data
  if (dependent %in% predictors) {
    predictors <- predictors[predictors != dependent]
  }

  # produce formulas
  aux <- " "

  # Lineal response
  if (grepl("l", type)) {
    aux <- paste(aux, paste(predictors, collapse = " + "), sep = " + ")
  }

  # Quadratic response
  if (grepl("q", type)) {
    for (v in 1:length(predictors)) {
      aux <- paste(aux, paste0("I(", predictors[v], "^2)"), sep = " + ")
    }
  }

  # Interaction between two variables
  if (grepl("p", type)) {
    if (npred > 1) {
      inter_tab <- utils::combn(predictors, 2)
      aux_inter <- paste0(" + ",
                          paste(apply(inter_tab, 2, paste, collapse = ":"),
                                collapse = " + "))

      if (length(aux_inter) > 0) {
        aux <- paste0(aux, aux_inter)
      }
    } else {
      if (grepl("l", type) | grepl("q", type)) {
        message("'p' is is only possible with 2 or more independent variables.",
                "\nReturning other combinations.")
      } else {
        stop("'p' is is only possible with 2 or more independent variables.",
             "\nTry other combinations of type.")
      }
    }
  }

  # Create all possible combination
  if (all_combinations) {
    ## unlist predictors in formula
    vec <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))[-1]


    # Number of combinations
    # total <- sapply(1:length(vec), function(x){
    #   factorial(length(vec)) / (factorial(x) * factorial(length(vec) - x))
    # })
    #
    # sum(total)

    if (!is.null(maxvar)){
      if (maxvar > length(vec)) {
        message("Maximum number of features: ", length(vec))
        maxvar = length(vec)
      }
    } else {
      maxvar = length(vec)
    }

    range_var <- minvar:maxvar

    if (length(range_var) >= 20){
      l <- length(range_var)
      range_var <- range_var[-c(l %/% 2 - 1, l %/% 2, l %/% 2 + 1)]

    }

    ## Get all combinations
    all_comb <- lapply(range_var, utils::combn, x = vec, simplify = FALSE)
    all_comb <- unlist(all_comb, recursive = FALSE)

    out <- sapply(all_comb, function(x) {
      paste0(dependent, " ~ ", paste(x, collapse = " + "))
    })

  } else{
    out <- gsub("  \\+", paste(dependent, "~"), aux)
  }

  # Return formula(s)
  return(out)
}

