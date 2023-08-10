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

# aux function to get the individual response of a variable keeping constant
# the others predictors

response <- function(model, variable, n = 100, new_data = NULL,
                     new_range = NULL) {

  # initial tests
  if (missing(model) | missing(variable)) {
    stop("Argument 'model' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  # It gets only the variable names used in the fitted model
  vnames <- colSums(sapply(colnames(model$data), grepl, names(coef(model)[-1]))) > 0

  if (any(!variable %in% names(vnames))) {
    stop("The name of the 'variable' was not defined correctly.")
  }

  # Extract calibration data from the model object
  cal_data <- model$data[, vnames]

  # Extract the limits of the calibration data
  cal_maxs <-  apply(cal_data, 2, FUN = max)
  cal_mins <-  apply(cal_data, 2, FUN = min)

  # Get the average of all variables
  means <- apply(cal_data, 2, FUN = mean)

  # Range variable in all the extent
  if (is.null(new_data) & is.null(new_range)) {
    rangev <- range(cal_data[, variable])

  } else {
    if (!is.null(new_range)) {
      rangev <- new_range
    } else {
      if (class(new_data)[1] == "SpatRaster") {
        rangev <- terra::minmax(new_data[[variable]])
      } else {
        rangev <- range(new_data[, variable])
      }
    }
  }

  newvar <- seq(rangev[1], rangev[2], length = n)


  m <- data.frame(matrix(means, n, length(means), byrow = T))
  colnames(m) <- names(means)

  m[, variable] <- newvar

  # Response of the variable
  m$predicted <- stats::predict(model, m, type = "response")

  output <- list(
    list(
      response = m[,c(variable, "predicted")],
      cal_limits = range(cal_mins[variable], cal_maxs[variable])
    )
  )

  return(output)

}
