#' Get GLM formulas according to defined response types
#'
#' @description
#' Generate GLM formulas for independent variables predicting a dependent
#' variable, taking into account response types required. All possible
#' combinations of variables can be created using arguments of the function.
#'
#' @usage
#' get_formulas(dependent, independent, type = "l", mode = "moderate",
#'              minvar = 1, maxvar = NULL)
#'
#' @param dependent (character) name of dependent variable.
#' @param independent (character) vector of name(s) of independent variable(s).
#' @param type (character) a character string that must contain "l",
#' "p", "q" or a combination of them. l = lineal, q = quadratic,
#' p = interaction between two variables. Default = "l".
#' @param mode (character) (character) a character string to indicate the strategy to
#' create the formulas for candidate models. Options are: "light", "moderate",
#' "intensive", or "complex". Default = "moderate".
#' @param minvar (numeric) minimum number of independent variables in formulas.
#' @param maxvar (numeric) maximum number of independent variables in formulas.
#'
#' @details
#' `mode` options determine what strategy to iterate the predictors
#' defined in \code{type} for creating models:
#' - **light**.-- returns simple iterations of complex formulas.
#' - **moderate**.-- returns a comprehensive number of iterations.
#' - **intensive**.-- returns all possible combination. Very time-consuming for
#' 6 or more independent variables.
#' - **complex**.-- returns only the most complex formula.
#'
#' @return
#' A character vector containing the resulting formula(s).
#'
#' @importFrom utils combn
#'
#' @export
#' @rdname get_formulas
#'
#' @examples
#' # example variables
#' dep <- "sp"
#' ind <- c("temp", "rain", "slope")
#'
#' # The most complex formula according to "type"
#' get_formulas(dep, ind, type = "lqp", mode = "complex")
#'
#' # mode = 'light', combinations according to type
#' get_formulas(dep, ind, type = "lqp", mode = "light")
#'
#' # mode = 'light', combinations according to type
#' get_formulas(dep, ind, type = "lqp", mode = "intensive")

get_formulas <- function(dependent, independent, type = "l",
                         mode = "moderate", minvar = 1, maxvar = NULL) {

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

  if (! mode %in% c("light", "moderate", "intensive", "complex")) {
    stop("'mode' must be defined as 'light', 'moderate', 'intensive' or 'complex'")
  }

  if (mode == "light") {

    red_var_comb <- aux_var_comb(independent, minvar = 2, maxvar = maxvar)
    reponse_comb <- aux_string_comb(type)

    output <-lapply(red_var_comb, function(y){
      get_formulas_main(dependent, independent = y, type = type, complex = TRUE)
    })

    return(unlist(output, use.names = F ))
  }

  if (mode == "moderate") {

    red_var_comb <- aux_var_comb(independent, minvar = 2, maxvar = maxvar)
    reponse_comb <- aux_string_comb(type)

    output <- lapply(reponse_comb, function(x){
      lapply(red_var_comb, function(y){

        get_formulas_main(dependent, independent = y, type = x, complex = TRUE)
      })
    })

    return(unlist(output, use.names = F ))
  }

  if (mode == "intensive"){

    output <- get_formulas_main(dependent, independent, type = type,
                                complex = FALSE, minvar = minvar,
                                maxvar = maxvar)
    return(output)
  }

  if (mode == "complex"){
    output <- get_formulas_main(dependent, independent, type = type,
                                complex = TRUE, minvar = minvar,
                                maxvar = maxvar)
    return(output)
  }
}

#' @export
#' @rdname get_formulas
#' @param complex (logical) whether to return the most complex formula.
#' @usage
#' get_formulas_main(dependent, independent, type = "l",
#'                   complex = FALSE, minvar = 1, maxvar = NULL)

get_formulas_main <- function(dependent, independent, type = "l",
                              complex = FALSE, minvar = 1 , maxvar = NULL) {

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

  if (!is.logical(complex)) {
    stop("'complex' must be logical (TRUE or FALSE).")
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
  if (!complex) {
    # unlist predictors in formula
    vec <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))[-1]

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

    output <- sapply(all_comb, function(x) {
      paste0(dependent, " ~ ", paste(x, collapse = " + "))
    })

  } else{
    output <- gsub("  \\+", paste(dependent, "~"), aux)
  }

  # Return formula(s)
  return(output)
}

#' @export
#' @rdname get_formulas
#' @param var_names sames as `independent`.

# Variable combination from KUENM
aux_var_comb <- function(var_names, minvar = 2, maxvar = NULL) {

  if(is.null(maxvar)) {maxvar <- length(var_names)}

  var_comb <- lapply(minvar:maxvar, function(x) {
    comb <- combn(var_names, m = x)
    comb_vs <- lapply(1:dim(comb)[2], function(y) {comb[, y]})
  })

  var_combs <- do.call(c, var_comb)
  names(var_combs) <- paste0("Set_", 1:length(var_combs))
  return(var_combs)
}

#' @export
#' @rdname get_formulas
#' @param string same as `type`.

# Function to generate all possible combinations of a string
aux_string_comb <- function(string) {

  bins <- strsplit(string, '')[[1]]
  n <- length(bins)

  all_comb <- lapply(1:n, function(i) {combn(bins, i, simplify = FALSE)})
  all_comb <- unlist(all_comb, recursive = FALSE)

  output <- lapply(all_comb, function(x){paste(x, collapse = "")})
  return(output)
}
