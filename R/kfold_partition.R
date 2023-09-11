#' K-fold data partitioning
#'
#' @param data `data.frame` or `matrix` containing at least two columns. The
#' first column must contain absence and presence records as 0 and 1.
#' @param k `numeric`, the number of groups that the given data is to be split
#' into.
#' @param seed `numeric`, integer value to specify an initial seed. Default = 1.
#'
#' @return
#' A list of vectors with the indices of rows corresponding to each fold.
#'
#' @export
#'

kfold_partition <- function(data, k = 2,  seed = 1){

  # initial tests
  if(missing(data)) {
    stop("Argument 'data' must be defined.")
  }

  if (k < 2) {
    stop("Number of k-folds must be equal or graeter than 2.")
  }

  # We extract the positions for presences and absences then to keep the original
  # ratio presences/absences we do it for each set separately
  pre <- which(data[, 1] == 1)
  aus <- which(data[, 1] == 0)

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

