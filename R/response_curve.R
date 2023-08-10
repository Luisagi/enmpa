#' Response curve: It evaluates the response of the variable and its limits.
#'
#' @param model an object of class "glm" which inherits from the class "lm".
#' @param variable `character vector`, name or names of the variables to be plotted.
#' @param n `numeric`, an integer guiding the number of breaks. Default n = 100
#' @param new_data a `SpatRaster`, data.frame or  matrix of variables
#' representing the range of values for the complete extent of the study area.
#' Default = NULL
#' @param new_range `numeric vector`, a numerical vector with the lower and
#' upper limits of the variable. Default = NULL
#' @param rescale probability 0-1. Default = FALSE.
#'
#' @export
#'
#' @importFrom stats predict coef
#' @importFrom graphics abline
#' @importFrom terra minmax


response_curve <- function(model, variable, n = 100, new_data = NULL,
                           new_range = NULL, rescale = FALSE) {

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


  res_list <- sapply(variable,  function(x){

    # Call the aux function "response()"
    response(model = model, variable = x, n = n,
             new_data = new_data, new_range = new_range)
  })


  for (i in names(res_list)){

    m <- res_list[[i]][[1]]        # Variable response
    limits <- res_list[[i]][[2]]   # Calibration limits

    Sys.sleep(2)

    if (rescale){
      # Plotting curve
      plot(m[, i], m$predicted, type = "l",ylim = c(0, 1),
           xlab = i, ylab = "Probability")
    } else{
      # Plotting curve
      plot(m[, i], m$predicted, type = "l",
           xlab = i, ylab = "Probability")
    }

    # It adds the calibration limits
    abline(v = limits,
           col = c("red", "red"),
           lty = c(2, 2),
           lwd = c(1, 1)
    )



  }
}
