################################################################################
##
## Response curve: It evaluates the response of the variable and its limits
##

## Usage:
# mod: fitted model
# var: variable of interest
# env: raster layers used for the fitting of the model
# N: number of steps (default 100)

response_curve <- function(mod, var, envar , N = 100) {

  require(terra)
  # get variables names from model object
  varname_aux <-
    colSums(sapply(colnames(mod$data), grepl, names(coef(mod)[-1]))) > 0

  # calibration data
  cal_data <- mod$data[, varname_aux]

  # Extract the limits of the calibration data
  cal_maxs <-  apply(cal_data, 2, FUN = max)
  cal_mins <-  apply(cal_data, 2, FUN = min)

  # Get the average of all variables
  means <- apply(cal_data, 2, FUN = mean)

  # range variable in all the extent
  l <- terra::minmax(envar[[var]])
  newvar <-
    seq(l[1] - 0.1 * (l[2] - l[1]), l[2] + 0.1 * (l[2] - l[1]), length = N)

  m <- data.frame(matrix(means, N , length(means), byrow = T))
  colnames(m) <- names(means)
  m[, var] <- newvar


  # predictions of the model for the newdata
  m$predicted <- terra::predict(mod, m, type = "response")

  # Plotting curve
  plot(m[, var], m$predicted, type = "l", ylim = c(0, 1),
       xlab = var, ylab = "Probability of the Species")

  # It adds the calibration limits
  abline(v = c(cal_mins[var], cal_maxs[var]),
         col = c("red", "red"),
         lty = c(2, 2), lwd = c(1, 1)
  )

  #out <- plot(m[,var], m$predicted, type = "l", xlab = var, ylab = "Probability of the Species")
  #return(out)
}
