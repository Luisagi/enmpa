#' Variable importance for GLMs
#'
#' @description
#' Calculates the relative importance of predictor variables based on the
#' concept of explained deviance. This is achieved by fitting a GLMs multiple
#' times, each time leaving out a different predictor variable to observe its
#' impact on the model's performance.
#'
#' @usage
#' var_importance(fitted)
#'
#' @param fitted an object of class `glm` or a list of GLMs obtained using the
#' functions \code{\link{fit_selected}} or \code{\link{fit_glms}}.
#'
#' @details
#' The process begins by fitting the full GLM model, which includes all predictor
#' variables. Subsequently, separate GLM models are fitted, excluding one
#' variable at a time to assess the influence of its absence on the model's
#' performance. By systematically evaluating the effect of removing each
#' predictor variable, the function provides valuable insights into their
#' individual contributions to the model's overall performance and explanatory
#' power.
#'
#' @return
#' A data.frame containing the relative contribution of each variable. An
#' identification for distinct models is added if `fitted` contains multiple
#' models.
#'
#' @export
#'
#' @importFrom stats update as.formula deviance coef
#'
#' @examples
#' # Load a fitted selected model
#' data(sel_fit, package = "enmpa")
#'
#' # Variable importance for single models
#' var_importance(sel_fit$ModelID_7)
#'
#' # Variable importance for multiple models (only one model in this list)
#' var_importance(sel_fit)

var_importance <- function(fitted){
  # initial tests
  if (missing(fitted)) {
    stop("Argument 'model' must be defined.")
  }

  fitted$selected <- NULL

  if (check_if_glm_list(fitted)){
    aux <- lapply(fitted, function(y) {var_importance_ind(y)})
    tab_contr <- do.call(rbind, aux)[, 1:2]

    tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
    rownames(tab_contr) <- NULL

  } else {
    tab_contr <- var_importance_ind(fitted)
  }

  return(tab_contr)
}
