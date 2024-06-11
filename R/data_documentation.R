#' Example of results obtained from GLM calibration using enmpa
#'
#' An object of the class enmpa_calibration storing the results from GLM
#' calibration.
#'
#' @format An object of class enmpa_calibration with results from the function
#' `\link{calibration_glm}`.
#'
#' @examples
#' data("cal_res", package = "enmpa")
#'
#' str(cal_res)
"cal_res"



#' Example data used to run model calibration exercises
#'
#' @description A dataset containing information on presence and absence, and
#' independent variables used to fit GLM models.
#'
#' @format A data frame with 5627 rows and 3 columns.
#' \describe{
#'   \item{Sp}{numeric, values of 0 = absence and 1 = presence.}
#'   \item{bio_1}{numeric, temperature values.}
#'   \item{bio_12}{numeric, precipitation values.}
#' }
#'
#' @examples
#' data("enm_data", package = "enmpa")
#' head(enm_data)
"enm_data"



#' Example data used to test models
#'
#' @description A dataset containing information on presence and absence, and
#' independent variables used to fit GLM models.
#'
#' @format A data frame with 100 rows and 3 columns.
#' \describe{
#'   \item{Sp}{numeric, values of 0 = absence and 1 = presence.}
#'   \item{lon}{numeric, longitude values.}
#'   \item{lat}{numeric, latitude values.}
#' }
#'
#' @examples
#' data("test", package = "enmpa")
#' head(test)
"test"



#' Example of selected models fitted
#'
#' @description An object of the class enmpa_fitted_models containing
#' fitted selected model(s) and the information from model evaluation for such
#' model(s).
#'
#' @format A list with four elements.
#' \describe{
#'   \item{glms_fitted}{a fitted glm (ModelID_7).}
#'   \item{selected}{a data.frame with results from evaluation of ModelID_7}
#'   \item{data}{a data.frame containing information on presence and absence, and
#'   independent variables used to fit GLM models.}
#'   \item{weights}{a vector with the weights for observations.}
#' }
#'
#' @examples
#' data("sel_fit", package = "enmpa")
"sel_fit"
