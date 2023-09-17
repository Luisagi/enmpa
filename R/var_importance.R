#' Variable importance
#'
#' @description
#' It calculates the relative importance of the predictor variables based on
#' explained deviance.
#'
#' @usage
#' var_importance(x)
#'
#' @param x an object of class `glm` or a list of them which inherit
#' from the class `lm`.
#'
#' @return  data.frame containing the relative contribution of each variable.
#'
#' @export
#'
#' @importFrom stats update as.formula deviance coef
#'

var_importance <- function(x){

  # initial tests
  if (missing(x)) {
    stop("Argument 'model' must be defined.")
  }
  if (check_if_glm_list(x)){

    aux <- lapply(x, function(y){var_importance_ind(y)})
    tab_contr <- do.call(rbind, aux)[,1:2]

    tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
    rownames(tab_contr) <- NULL

  } else {
    tab_contr <- var_importance_ind(x)
  }
  return(tab_contr)
}
