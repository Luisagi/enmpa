#' Fitting the selected models
#'
#' @description
#' Function that facilitates the fitting step of multiple models.
#'
#' @param formulas (character) a vector containing the formula(s) of model(s).
#' @param data data.frame or matrix with the dependent and independent variables.
#' @param weights (numeric) a vector with the weights for observations.

#' @return a list of fitted models
#'
#' @export
#'
#' @importFrom stats glm as.formula binomial
#'

fit_glm <- function(formulas, data, weights = NULL){

  # Model fitting
  fits <- lapply(formulas, function(y){
    suppressWarnings(
      glm(formula = as.formula(y), family = binomial(link = "logit"),
          data = data,
          weights = weights)
    )
  })

  names(fits) <- paste0("Model_ID_", 1:length(formulas))

  return(fits)
}



