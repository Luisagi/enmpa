#' K-fold data partitioning
#'
#' @description
#'
#' The function is designed to partition the available data into k equal-sized
#' subsets or folds while maintaining the global proportion of presence-absences
#' in each fold. This approach leads to a more robust evaluation of the model's
#' performance, as it accounts for the inherent variability in the data.
#'
#' @usage
#' kfold_partition(data, dependent, k = 2, seed = 1)
#'
#' @param data data.frame or matrix containing at least two columns.
#' @param dependent (character) column name that contains the absence and presence records as 0
#' and 1.
#' @param k (numeric) the number of groups that the given data is to be split
#' into.
#' @param seed (numeric) integer value to specify an initial seed. Default = 1.
#'
#' @return
#' A list of vectors with the indices of rows corresponding to each fold.
#'
#'
#' @examples
#'
#' data <- data.frame(species = c(rep(0, 80), rep (1,20)),
#'                    variable1 = rnorm(100),
#'                    variable2 = rpois(100, 2))
#'
#' kfolds <- kfold_partition(data, dependent = "species", k = 2)
#'
#' data[kfolds$Fold_1,]
#' data[kfolds$Fold_2,]
#'
#'
#' @export
#'

kfold_partition <- function(data, dependent, k = 2,  seed = 1){

  # initial tests
  if(missing(data) | missing(dependent)) {
    stop("Argument 'data' or 'dependent' must be defined.")
  }

  if (k < 2) {
    stop("Number of k-folds must be equal or greater than 2.")
  }

  # We extract the positions for presences and absences then to keep the original
  # ratio presences/absences we do it for each set separately
  pre <- which(data[, dependent] == 1)
  aus <- which(data[, dependent] == 0)

  #To each original position we assign a value from 1:k and the is randomly shuffled
  set.seed(seed)
  foldp <- sample(cut(seq(1, length(pre)), breaks = k, labels = FALSE))
  folda <- sample(cut(seq(1, length(aus)), breaks = k, labels = FALSE))

  # We name the "fold" vector with the positions of the original dataset
  names(foldp) <- pre
  names(folda) <- aus

  # Combination of P/A
  all <- c(foldp, folda)

  out <- list() # empty list when are stored the index of each FOLD

  for (i in 1:k) {
    out[[paste0("Fold_", i)]] <- as.numeric(names(all[all == i]))
  }

  return(out)
}

