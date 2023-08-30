#' Variable importance
#'
#' @description
#' The function calculates the relative importance of the predictor variables
#' based on explained deviance.
#'
#' @usage
#' var_importance(x, plot = TRUE)
#'
#' @param x an object of class "glm" or a "list" of them which inherit
#' from the class "lm".
#'
#' @param plot `logical`, whether plot the variable contribution. Default = TRUE.
#'
#' @return  data.frame
#'
#'
#' @export
#'
#'
#' @importFrom stats update as.formula deviance coef
#' @importFrom graphics barplot boxplot text


var_importance <- function(x, plot = TRUE){

  # initial tests
  if (missing(x)) {
    stop("Argument 'model' must be defined.")
  }

  if (check_if_glm_list(x)){

    aux <- lapply(x, function(y){var_importance_ind(y)})
    tab_contr <- do.call(rbind, aux)[,1:2]

    tab_contr$Models <- rep(names(aux), times = sapply(aux, nrow))
    rownames(tab_contr) <- NULL

    if (plot){
      # Create a boxplot
      boxplot(contribution ~ predictor, data = tab_contr,
              xlab = NULL, ylab = "Relative contribution",
              main = "Variable importance",
              ylim = c(0, max(tab_contr$contribution) + 0.2)
              )
      #Calculate means and counts per group
      means  <- tapply(tab_contr$contribution, tab_contr$predictor, mean)
      counts <- tapply(tab_contr$contribution, tab_contr$predictor, length)

      # Common y coordinate for text labels in the upper part of the plot
      text_y <- max(tab_contr$contribution) + 0.15

      # Add means and counts to the plot (same level)
      for (i in 1:length(unique(tab_contr$predictor))) {
        text(i, text_y, paste("Mean =", round(means[i], 2)), pos = 3)
        text(i, text_y , paste("N =", counts[i]), pos = 1)
      }
    }


  } else {
    tab_contr <- var_importance_ind(x)

    if (plot){
      # Create a barplot
      barplot(tab_contr$contribution, names = tab_contr$predictor, horiz = FALSE)
    }
  }

  return(tab_contr)

}






