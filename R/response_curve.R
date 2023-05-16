#' Response curve: It evaluates the response of the variable and its limits
#'
#' @param model an object of class "glm" which inherits from the class "lm".
#' @param variable `character`, name of the variable to be plotted.
#' @param n `numeric`, an integer guiding the number of breaks. Default n = 100
#' @param new_data a `SpatRaster`, data.frame or  matrix of variables
#' representing the range of values for the complete extent of the study area.
#' Default = NULL
#' @param new_range `numeric vector`, a numerical vector with the lower and
#' upper limits of the variable. Default = NULL
#'
#' @export
#'
#' @importFrom stats predict coef
#' @importFrom graphics abline
#' @importFrom terra minmax


response_curve <- function(model, variable, n = 100, new_data = NULL,
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

  if (!variable %in% vnames){
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

  newvar <- seq(rangev[1] - 0.1 * (rangev[2] - rangev[1]),
                rangev[2] + 0.1 * (rangev[2] - rangev[1]),
                length = n)

  m <- data.frame(matrix(means, n , length(means), byrow = T))
  colnames(m) <- names(means)

  m[, variable] <- newvar

  # Response of the variable
  m$predicted <- stats::predict(model, m, type = "response")

  # Plotting curve
  plot(m[, variable], m$predicted, type = "l", ylim = c(0, 1),
       xlab = variable, ylab = "Probability")

  # It adds the calibration limits
  abline(v = c(cal_mins[variable], cal_maxs[variable]),
         col = c("red", "red"),
         lty = c(2, 2),
         lwd = c(1, 1)
  )
}
