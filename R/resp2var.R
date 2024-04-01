#' Two-Way interaction response plot
#'
#' @description
#' A view of the species probability into a two-dimensional
#' environmental space.
#'
#' @usage
#' resp2var(model, variable1 , variable2, n = 100, new_data = NULL,
#'          extrapolate = FALSE, color.palette	= NULL,
#'          xlab = NULL, ylab = NULL, ...)
#
#' @param model an object of class `glm` or a list of GLMs obtained using the
#' functions \code{\link{fit_selected}} or \code{\link{fit_glms}}.
#' @param variable1 (character) name of the variable to be plotted in x axis.
#' @param variable2 (character) name of the variable to be plotted in y axis.
#' @param n (numeric) an integer guiding the number of breaks. Default = 100
#' @param new_data a `SpatRaster`, data.frame, or  matrix of variables
#' representing the range of variable values in an area of interest.
#' Default = NULL.
#' @param extrapolate (logical) whether to allow extrapolation to study the
#' behavior of the response outside the calibration limits. Ignored if
#' `new_data` is defined. Default = TRUE.
#' @param color.palette (function) a color palette function to be used to assign
#' colors in the plot. Default = function(n) rev(hcl.colors(n, "terrain")).
#' @param xlab (character) a label for the x axis. The default, NULL, uses the
#' name defined in `variable1`.
#' @param ylab (character) a label for the y axis. The default, NULL, uses the
#' name defined in `variable2`.
#' @param ... additional arguments passed to
#' \code{\link[graphics]{filled.contour}}.
#'
#' @details
#' The function calculates probabilities by focusing on each combination of the
#' two supplied environmental variable while keeping all other variables constant
#' at their mean values.
#'
#'
#' @return
#' A plot with the response interaction of two environmental dimensions for
#' `variable1` and `variable2`, and don't return anything.
#'
#' @export
#' @rdname resp2var
#'
#' @importFrom stats predict coef
#' @importFrom graphics filled.contour par title
#' @importFrom terra minmax
#' @importFrom grDevices hcl.colors
#'
#' @examples
#' # Load a fitted selected model
#' data(sel_fit, package = "enmpa")
#'
#' # Two-Way interaction response plot in the calibration limits
#' resp2var(sel_fit$ModelID_7, variable1 = "bio_1", variable2 = "bio_12")
#'
#' # Two-Way interaction response plot allowing extrapolation
#' resp2var(sel_fit$ModelID_7, variable1 = "bio_1", variable2 = "bio_12",
#'          extrapolation = FALSE)

resp2var <- function(model, variable1 , variable2, n = 100, new_data = NULL,
                     extrapolate = FALSE, color.palette	= NULL,
                     xlab = NULL, ylab = NULL, ...) {

  # initial tests
  if (missing(model) | missing(variable1) | missing(variable2)) {
    stop("Argument 'model' or 'variables' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }

  if (is.null(xlab)) xlab <- variable1
  if (is.null(ylab)) ylab <- variable2
  if (is.null(color.palette)) color.palette = function(n) rev(hcl.colors(n, "terrain"))
  # if (is.null(color.palette)) color.palette = function(n) hcl.colors(n)


  variables <- c(variable1, variable2)

  # It gets only the variable names used in the fitted model
  vnames <- colSums(
    sapply(colnames(model$data), grepl, names(coef(model)[-1]))
  ) > 0

  if (any(!variables %in% names(vnames))) {
    stop("The name of the 'variables' was not defined correctly.")
  }

  # Extract calibration data from the model object
  cal_data <- model$data[, vnames]

  # Extract the limits of the calibration data
  cal_maxs <-  apply(cal_data, 2, FUN = max)
  cal_mins <-  apply(cal_data, 2, FUN = min)

  # Get the average of all variabless
  means <- apply(cal_data, 2, FUN = mean)


  if (is.null(new_data)) {

    if (extrapolate){

      rr1 <- range(cal_data[, variables[1]]) # range of the calibration data
      rr2 <- range(cal_data[, variables[2]]) # range of the calibration data

      l_limit1 <- rr1[1] - 0.11 * diff(rr1)
      u_limit1 <- rr1[2] + 0.11 * diff(rr1)

      l_limit2 <- rr2[1] - 0.11 * diff(rr2)
      u_limit2 <- rr2[2] + 0.11 * diff(rr2)


      rangev1 <- c(l_limit1, u_limit1)
      rangev2 <- c(l_limit2, u_limit2)

    } else {

      rangev1 <- range(cal_data[, variables[1]])
      rangev2 <- range(cal_data[, variables[2]])
    }

  } else {

    if (class(new_data)[1] == "SpatRaster") {
      rangev1 <- terra::minmax(new_data[[variables[1]]])
      rangev2 <- terra::minmax(new_data[[variables[2]]])

    } else {
      rangev1 <- range(new_data[, variables[1]])
      rangev2 <- range(new_data[, variables[2]])
    }

  }

  newvar <- expand.grid(x = seq(rangev1[1], rangev1[2], length = n),
                        y = seq(rangev2[1], rangev2[2], length = n))


  m <- data.frame(matrix(means, nrow(newvar), length(means), byrow = T))
  colnames(m) <- names(means)

  m[, variables[1]] <- newvar[, 1]
  m[, variables[2]] <- newvar[, 2]

  # Response of the variables
  predicted <- stats::predict(model, m, type = "response")

  # Arguments for filled.contour
  # "x,y" locations of grid lines at which the values in z are measured
  # "z" a numeric matrix containing the values to be plotted
  x = unique(m[, variables[1]])
  y = unique(m[, variables[2]])
  z = matrix(data = predicted, nrow = n, ncol = n)

  filled.contour(x, y, z,
                 nlevels = 10,
                 color.palette = color.palette,
                 key.title = {par(cex.main = 0.9); title(main = NULL)},
                 xlab = xlab, ylab = ylab, ...
  )
}
