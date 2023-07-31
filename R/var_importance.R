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

  # Aux function to get deviance of a reduced model
  get_red_dev <- function(full_model, reduce_var){
    reduce_model <- suppressWarnings(update(full_model,
                           as.formula(paste("~.-", reduce_var)),
                           data = full_model$data))
    return(deviance(reduce_model))
  }

  # deviance of the full model
  dev_full <- deviance(model)

  # deviance of the reduced models
  dev_reduction <- sapply( names(coef(model))[-1], function(x){
    dev_full - get_red_dev(model, x)
  })

  deviance_importance <- dev_reduction / sum(dev_reduction)

  tab_contr <- data.frame(features = names(deviance_importance), stringsAsFactors = FALSE)
  tab_contr$contr <- deviance_importance

  ord <- order(tab_contr$contr, decreasing = TRUE)
  tab_contr <- tab_contr[ord,]
  tab_contr$cum_contr <- cumsum(tab_contr$contr)
  return(tab_contr)
}

model <- preds$fitted_models$Model_ID_1

var_importance(model)






