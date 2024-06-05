#' Variable importance for GLMs
#'
#' @description
#' Calculates the relative importance of predictor variables based on the
#' concept of explained deviance. This is achieved by fitting a GLMs multiple
#' times, each time leaving out a different predictor variable to observe its
#' impact on the model's performance.
#'
#' @usage
#' var_importance(fitted, modelID = NULL, data = NULL)
#'
#' @param fitted an object of class `glm`, a list of GLMs obtained using the
#' function \code{\link{fit_glms}}, or an object `enmpa_fitted_models` from
#' \code{\link{fit_selected}} .
#' @param modelID (character) vector of ModelID(s) to be considered when the
#' `fitted` input is of the class `enmpa_fitted_models`. By default all models
#' are included. Default = NULL.
#' @param data data.frame or matrix of data used in the model calibration step.
#' It must be defined in case the model entered does not explicitly include a
#' data component. Default = NULL.
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
#' var_importance(sel_fit, modelID = "ModelID_7")
#'
#' # Variable importance for multiple models (only one model in this list)
#' var_importance(sel_fit)

var_importance <- function(fitted, modelID = NULL, data = NULL){
  # initial tests
  if (missing(fitted)) {
    stop("Argument 'model' must be defined.")
  }

  if (!class(fitted)[1] %in% c("glm", "enmpa_fitted_models", "list")){
    stop("'fitted' must be of class 'glm','list', or 'enmpa_fitted_models'")
  }

  #  for an individual GLM______________________________________________________
  if (class(fitted)[1] == "glm") {

    if (!is.null(fitted$data)){
    tab_contr <- var_importance_ind(fitted, data = fitted$data,
                                    weights = fitted$weights)

    } else {
      stop(paste0("The argument 'data' must be defined in case the model entered",
                  " does not explicitly include a data component."))
    }
  }

  #  List of GLMs_______________________________________________________________
  if (check_if_glm_list(fitted)){

    # if data argument is not empty
    if (!is.null(data) && length(fitted) > 1 ){
      aux <- lapply(fitted, function(y) {var_importance_ind(y, data, weights)})
      tab_contr <- do.call(rbind, aux)[, 1:2]
      tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
      rownames(tab_contr) <- NULL

      # standardize product names
      tab_contr$predictor <- standardize_interaction_names(tab_contr$predictor)
    }

    if (!is.null(data) && length(fitted) == 1 ){
      tab_contr <- var_importance_ind(fitted[1][[1]], data = fitted$data,
                                      weights = fitted$weights)
    }

    # if data argument is empty
    if (is.null(fitted[1][[1]]$data)){
      stop("Calibration data must be defined.")

    } else {
      data <- fitted[1][[1]]$data

      if (length(fitted) > 1){
        aux <- lapply(fitted, function(y) {var_importance_ind(y, data, weights)})
        tab_contr <- do.call(rbind, aux)[, 1:2]
        tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
        rownames(tab_contr) <- NULL

        # standardize product names
        tab_contr$predictor <- standardize_interaction_names(tab_contr$predictor)

      } else {
        tab_contr <- var_importance_ind(fitted[1][[1]], data = fitted$data,
                                        weights = fitted$weights)
      }
    }
  }


  #  using de 'enmpa fitted object'_____________________________________________
  if (class(fitted)[1] == "enmpa_fitted_models") {

    data  <- fitted$data
    weights <- fitted$weights
    list_glms <-  fitted$glms_fitted

    if (is.null(modelID)){

      aux <- lapply(list_glms, function(y) {var_importance_ind(y, data, weights)})
      tab_contr <- do.call(rbind, aux)[, 1:2]
      tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
      rownames(tab_contr) <- NULL

      # standardize product names
      tab_contr$predictor <- standardize_interaction_names(tab_contr$predictor)

    } else {
      tab_contr <- var_importance_ind(list_glms[modelID][[1]], data, weights )
    }
  }
  return(tab_contr)

}
