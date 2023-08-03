# Aux function to get deviance of a model after excluding predictors
get_red_dev <- function(full_model, reduce_var) {
  # initial tests
  if (missing(full_model)) {
    stop("Argument 'full_model' must be defined.")
  }
  if (missing(reduce_var)) {
    stop("Argument 'reduce_var' must be defined.")
  }

  reduce_model <- suppressWarnings(
    update(full_model, as.formula(paste("~.-", reduce_var)),
           data = full_model$data)
  )

  return(deviance(reduce_model))
}
