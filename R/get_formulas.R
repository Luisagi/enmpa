#' Get formula combinations
#'
#' @description
#' It provides a method for calculating all possible or the most complex formula(s).
#'
#' @usage
#' get_formulas(resp_var = "species", data = data, type = "l",  all_comb = T)
#'
#' @param resp_var `character`, name of the column with the presence-absence data.
#' @param data a data.frame or  matrix of variables
#' @param type `character`, a character string that must contain 'l', 'p', 'q'
#' or a combination of them. l= lineal q = quadratic p = interaction between two variables.
#' Default = 'l'.
#' @param all_comb `logical`, return all combination (Default=TRUE) or only the most complex formula (FALSE).
#'
#' @return a character vector containing the resulting formula combination(s).
#'
#' @importFrom utils combn
#'


get_formulas <- function(resp_var, data, type = 'l', all_comb = T) {

  # initial test
  if (!is.character(resp_var) || length(resp_var) != 1) {
    stop("'resp_var' must be a unique response variable name.")
  }
  if (!is.data.frame(data) &&  !is.matrix(data)) {
    stop("'data' must be a data.frame or matrix.")
  }
  if (!all(unlist(strsplit(type, "")) %in% c("l", "p", "q"))) {
    warning(
      "'type' must be contained in c('l', 'p', 'q') or a combination of them. Using the default option type = 'l'."
    )
    type = 'l'
  }

  explVarNames <- colnames(data)
  if (resp_var %in% explVarNames) {
    # remove the response variable if given in data
    data <- data[,-which(explVarNames == resp_var), drop = FALSE]
    explVarNames <- colnames(data)
  }

  ## Formula creation

  aux <- c(1)

  # Lineal response
  if (grepl("l", type)) {
    aux <- paste(aux, paste(explVarNames, collapse = " + "), sep = " + ")
  }

  # Quadratic response
  if (grepl("q", type)) {
    for (v in 1:ncol(data)) {
      if (is.numeric(data[, v])) {
        aux <- paste(aux, paste0("I(", explVarNames[v], "^2)"), sep = " + ")
      }
    }
  }
  # Interaction between two variables
  if (grepl("p", type)) {
    aux_inter <- c()
    inter_tab <- utils::combn(explVarNames, 2)
    aux_inter <- paste(aux_inter, paste(apply(inter_tab, 2, paste, collapse = ":"), collapse = " + "), sep = " + ")

    if (length(aux_inter) > 0) {
      aux <- paste0(aux, aux_inter)
    }
  }


  if (all_comb == T) {

    ## Create all possible combination
    vec <- unlist(strsplit(gsub(" ", "", aux), split = "[+]"))
    vec <- vec[-1] # Remove the aux 1

    # Get all combinations
    list_formulas <-
      lapply(1:length(vec),
             utils::combn,
             x = vec,
             simplify = FALSE)
    all_comb <- unlist(list_formulas, recursive = FALSE)

    for (i in 1:length(all_comb)) {
      all_comb[[i]] <-
        as.character(paste0(resp_var, " ~ ", paste(all_comb[[i]], collapse = " + ")))
    }

    out <- unlist(all_comb)

    ## Return a list of formulas
    return(out)

  } else{
    ## Return the most complex formula
    out <- as.character(paste0(resp_var, " ~ ", sub("1 \\+ ", "", aux)))
    return(out)
  }
}
