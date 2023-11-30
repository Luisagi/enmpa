# ------------------------------------------------------------------------------
# Aux function to evaluate the Variable Contribution of the predictors
#

#to get deviance of a model after excluding predictors
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

# get variable contribution for an individual model
var_importance_ind <- function(model){

  # initial tests
  if (missing(model)) {
    stop("Argument 'model' must be defined.")
  }

  # deviance of the full model
  dev_full <- deviance(model)

  # deviance of the reduced models
  dev_reduction <- sapply(names(coef(model))[-1], function(x) {
    dev_full - get_red_dev(model, x)
  })

  deviance_importance <- dev_reduction / sum(dev_reduction)

  # preparing results
  tab_contr <- data.frame(predictor = names(deviance_importance),
                          stringsAsFactors = FALSE)
  tab_contr$contribution <- deviance_importance

  ord <- order(tab_contr$contribution, decreasing = TRUE)
  tab_contr <- tab_contr[ord,]
  tab_contr$cum_contribution <- cumsum(tab_contr$contribution)

  # returning results
  return(tab_contr)
}


# ------------------------------------------------------------------------------
# Aux function to calculate the Response Curves
#


# aux function to get the individual response of a variable keeping constant
# the others predictors
response <- function(model, variable, n = 100, new_data = NULL,
                     extrapolate = FALSE) {

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
  vnames <- colSums(
    sapply(colnames(model$data), grepl, names(coef(model)[-1]))
    ) > 0

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


  if (is.null(new_data)) {

    if (extrapolate){

      rr <- range(cal_data[, variable]) # range of the calibration data
      extension <- 0.11 * diff(rr)

      l_limit <- rr[1] - extension
      u_limit <- rr[2] + extension

      rangev <- c(l_limit, u_limit)

    } else {
      rangev <- range(cal_data[, variable])
    }

  } else {

    if (class(new_data)[1] == "SpatRaster") {
      rangev <- terra::minmax(new_data[[variable]])
    } else {
      rangev <- range(new_data[, variable])
    }

  }

  newvar <- seq(rangev[1], rangev[2], length = n)


  m <- data.frame(matrix(means, n, length(means), byrow = T))
  colnames(m) <- names(means)

  m[, variable] <- newvar

  # Response of the variable
  m$predicted <- stats::predict(model, m, type = "response")

  return(m[,c(variable, "predicted")])

}


# Response curve for a single model
response_curve_ind <- function(model, variable, n = 100, new_data = NULL,
                               extrapolate = FALSE, xlab = NULL, ylab = NULL,
                               col = NULL, ...) {

  # initial tests
  if (missing(model) | missing(variable)) {
    stop("Argument 'model' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }


  response_out <- response(model = model, variable = variable, n = n,
                           extrapolate = extrapolate, new_data = new_data)

  limits <- range(model$data[,variable])

  ## Plotting curve
  # Create a list of arguments to pass to the plot function
  plotcurve_args <- list(x = response_out[, variable],
                         y = response_out$predicted,
                         type = "l",
                         xlab= xlab,
                         ylab = ylab,
                         col = col,
                         ...)

  # plot using do.call()
  do.call(plot, plotcurve_args)

  abline(v = limits,
         col = c("black", "black"),
         lty = c(2, 2),
         lwd = c(1, 1)
         )
}

# Consensus response curve
response_curve_cons <- function(model, variable, n = 100, extrapolate = FALSE,
                                new_data = NULL, xlab = NULL, ylab = NULL,
                                col = NULL, ...) {

  # initial tests
  if (missing(model) | missing(variable)) {
    stop("Argument 'model' or 'variable' must be defined.")
  }

  if (!is.null(new_data)) {
    if (!class(new_data)[1] %in% c("matrix", "data.frame", "SpatRaster")) {
      stop("'new_data' must be of class 'matrix', 'data.frame', 'SpatRaster'")
    }
  }


  # extract the response of the variable for each models
  response_out <-
    lapply(model, function(x){

      # It gets only the variable names used in the fitted model
      vnames <- colSums(sapply(colnames(x$data), grepl, names(coef(x)[-1]))) > 0

      if (any(!variable %in% names(vnames))) {
        stop("The name of the 'variable' was not defined correctly.")
      }

      coefs <- coef(x)[-1]
      variable_q <- c(variable, paste0("I(", variable,"^2)")) # find the quadratic too

      if ( sum(variable_q %in% names(coefs))){
        x <- response(x, variable, new_data = new_data, extrapolate = extrapolate)
        return(x)
      } else{
        return(NULL)
      }
    })

  response_out <- do.call(rbind, response_out)
  limits <- range(model[[1]]$data[, variable])


  x <- response_out[, variable]
  y <- response_out$predicted

  # Fit GAM model
  fitting <- mgcv::gam(y ~ s(x, bs = "cs"))

  # Generate predicted values and standard error.
  x_seq <- seq(min(x), max(x), length.out = 100)
  pred <- predict(fitting, newdata = data.frame(x = x_seq), se = T)

  # Extract predicted values, confidence intervals (95%), and standard errors
  y_pred <- pred$fit
  lower_ci <- y_pred - 1.96 * pred$se.fit
  upper_ci <- y_pred + 1.96 * pred$se.fit


  ## Plotting curve
  # Create a list of arguments to pass to the plot function
  plotcurve_args <- list(x = x,
                         y = y,
                         type = "n",
                         xlab= xlab,
                         ylab = ylab,
                         ...)

  # plot using do.call()
  do.call(plot, plotcurve_args)

  # Create shading interval using polygon
  x_polygon <- c(x_seq, rev(x_seq))
  y_polygon <- c(lower_ci, rev(upper_ci))
  polygon(x_polygon, y_polygon, col = "lightgrey", border = NA)

  # Add the regression curve
  lines(x_seq, y_pred, col = col)

  # It adds the calibration limits
  abline(v = limits,
         col = c("black", "black"),
         lty = c(2, 2),
         lwd = c(1, 1)
  )
}

# ------------------------------------------------------------------------------
# Aux function to calculate consensus predictions (or "ensemble").
#

# Consensus forecasts are obtained by combining the forecasts from a collection
# (or “ensemble”) of model predictions

consensus_p <- function(predictions, weights = NULL){


  if (!is.null(weights)) {

    if (sum(weights) != 1){
      stop("'weights' must sum to 1 to calculate the weighted average.")

      } else {
        # Mean
        c_mean <- terra::app(predictions, mean)

        # Median
        c_media <- terra::app(predictions, median)

        # Weighted average
        c_wmean <- terra::app(predictions*weights, sum)

        # Variance between the consensus predictions
        c_var <- terra::app(c(c_mean, c_media, c_wmean), var)

        cons <- c(c_mean, c_media, c_wmean, c_var)
        names(cons) <- c("Mean", "Median", "Weighted_average",
                         "Consensus_variance")
    }
  } else {

    # Mean
    c_mean <- terra::app(predictions, mean)

    # Median
    c_media <- terra::app(predictions, median)

    # Variance between the consensus predictions
    c_var <- terra::app(c(c_mean, c_media), var)

    cons <- c(c_mean, c_media, c_var)
    names(cons) <- c("Mean", "Median", "Consensus_variance")
    }

  return(cons)

}

# ------------------------------------------------------------------------------
# Aux to check if an argument is a list of glm objects
check_if_glm_list <- function(arg) {
  if (methods::is(arg, "list") && all(sapply(arg, function(x) "glm" %in%  class(x)))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# ------------------------------------------------------------------------------
# Aux to save calibration result tables
save_cal <- function(x, out_dir) {

  if (missing(out_dir)) {
    stop("Argument 'out_dir' must be defined.")
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  write.table(x$calibration_results, row.names = FALSE, sep = ",",
              file = paste0(out_dir, "/1_Full_report.csv"))

  write.table(x$summary, row.names = FALSE, sep = ",",
              file = paste0(out_dir, "/2_Summary.csv"))

  write.table(x$selected, row.names = FALSE, sep = ";",
              file = paste0(out_dir, "/3_Selected_models.csv"))

  write.table(x$data, row.names = FALSE, sep = ",",
              file = paste0(out_dir, "/4_Data_splitted.csv"))


}

