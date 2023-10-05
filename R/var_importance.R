#' Variable importance
#'
#' @description
#' The function calculates the relative importance of predictor variables
#' based on the concept of explained deviance. This is achieved by iteratively
#' fitting a GLMs multiple times, where each iteration involves leaving out a
#' different predictor variable to observe its impact on the model's performance.
#'
#' The process begins by fitting the full GLM model, which includes all predictor
#' variables. Subsequently, separate GLM models are fitted, excluding one
#' variable at a time to assess the influence of its absence on the model's
#' performance. By systematically evaluating the effect of removing each
#' predictor variable, the function provides valuable insights into their
#' individual contributions to the model's overall performance and explanatory
#' power.
#'
#' @usage
#' var_importance(x)
#'
#' @param x an object of class `glm` or a list of them which inherit
#' from the class `lm`.
#'
#' @return  data.frame containing the relative contribution of each variable.
#'
#' @examples
#' # Load two fitted models
#' load(system.file("extdata", "glm_fitted.RData", package = "enmpa"))
#'
#' # Variable importance for single models
#' var_importance(fits$Model_ID_1)
#' var_importance(fits$Model_ID_2)
#'
#' # Variable importance for multiple models
#' var_importance(fits)
#'
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
