#' Variable importance
#'
#' @description
#' The function calculates the relative importance of the predictor variables
#' based on explained deviance.
#'
#' @usage
#' var_importance(model)
#'
#' @param model an object of class "glm" which inherits from the class "lm".
#'
#' @return  data.frame
#'
#'
#' @export
#'
#'
#' @importFrom stats update as.formula deviance coef


var_importance <- function(model){

  # initial tests
  if (missing(model)) {
    stop("Argument 'model' must be defined.")
  }

  # deviance of the full model
  dev_full <- deviance(model)

  # deviance of the reduced models
  dev_reduction <- sapply(names(coef(model))[-1], function(x) {
    dev_full - get_red_dev(model, x)
  })

  deviance_importance <- dev_reduction / sum(dev_reduction)

  # preparing results
  tab_contr <- data.frame(predictors = names(deviance_importance),
                          stringsAsFactors = FALSE)
  tab_contr$contribution <- deviance_importance

  ord <- order(tab_contr$contribution, decreasing = TRUE)
  tab_contr <- tab_contr[ord,]
  tab_contr$cum_contribution <- cumsum(tab_contr$contribution)

  # returning results
  return(tab_contr)
}






