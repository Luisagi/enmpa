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
#' @examples
#' # Load species occurrences and environmental data.
#' enm_data <- read.csv(system.file("extdata", "pa_data.csv", package = "enmpa"))
#'
#' # Custom formulas
#' forms <- c("Sp ~ bio_1 + I(bio_1^2) + I(bio_12^2)",
#'            "Sp ~ bio_12 + I(bio_1^2) + I(bio_12^2)")
#'
#' # Fitting models
#' fits <- fit_glm(forms, data = enm_data)
#'
#' fits$Model_ID_1
#' fits$Model_ID_2
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



