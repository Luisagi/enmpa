#' Fitting selected GLMs models
#'
#' @description
#' Functions to facilitate fitting multiple GLMs.
#'
#' @param glm_calibration a list resulting from \code{\link{calibration_glm}}.
#' Models fitted are those in the slot "selected".
#' @param formulas (character) a vector containing the formula(s) for GLM(s).
#' @param data data.frame with the dependent and independent variables.
#' @param weights (numeric) a vector with the weights for observations.
#' Default = NULL.
#' @param id (character) id code for models fitted. Default = NULL.
#'
#' @return
#' A list of fitted GLMs.
#'
#' For `fit_selected`, an enmpa `fitted models` object.
#'
#' @export
#' @importFrom stats glm as.formula binomial
#' @rdname fit_glms
#'
#' @examples
#' # GLM calibration results
#' data(cal_res, package = "enmpa")
#'
#' # Fitting selected models
#' sel_fit <- fit_selected(cal_res)
#'
#' sel_fit
#'
#' # Custom formulas
#' forms <- c("Sp ~ bio_1 + I(bio_1^2) + I(bio_12^2)",
#'            "Sp ~ bio_12 + I(bio_1^2) + I(bio_12^2)")
#'
#' # Fitting models
#' fits <- fit_glms(forms, data = cal_res$data)
#'
#' fits$ModelID_1

fit_selected <- function(glm_calibration) {
  if (missing(glm_calibration)) {
    stop("Arguments 'glm_calibration' must be defined.")
  }

  glms_fitted_list_full <- fit_glms(formulas = glm_calibration$selected$Formulas,
                                    data = glm_calibration$data,
                                    weights = glm_calibration$weights,
                                    id = glm_calibration$selected$ModelID)

  # deleted redundant data to save memory
  glms_fitted_list_full <- lapply(glms_fitted_list_full, function(x){
    x$data <- NULL
    x$weights <- NULL
    return(x)
  })

  output <- new_enmpa_fitted_models(data = glm_calibration$data,
                                    selected = glm_calibration$selected,
                                    weights = glm_calibration$weights,
                                    glms_fitted = glms_fitted_list_full)
  return(output)
}


#' @rdname fit_glms
#' @usage fit_glms(formulas, data, weights = NULL, id = NULL)
#' @export

fit_glms <- function(formulas, data, weights = NULL, id = NULL) {
  if (missing(formulas)) {
    stop("Arguments 'formulas' must be defined.")
  }
  if (missing(data)) {
    stop("Arguments 'data' must be defined.")
  }

  # Model fitting
  fits <- lapply(formulas, function(y) {
    suppressWarnings(
      glm(formula = as.formula(y), family = binomial(link = "logit"),
          data = data, weights = weights)
    )
  })

  if (is.null(id)) {
    names(fits) <- paste0("ModelID_", 1:length(formulas))
  } else {
    names(fits) <- id
  }
  return(fits)
}



