################################################################################
##
##  Cross-validation based on k-fold data partitioning
##

### Usage
# data: dataframe
# k: number of k-folds
# seed: set a value to initialize the random number generator


kfold_partition <- function(x, k,  seed = 1){

  if (k < 2) {
    stop(paste0("\n Number of k-folds must be >= 2. \n"))
  }

  # We extract the positions for presences and absences then to keep the original
  # ratio presences/absences we do it for each set separately
  pre <- which(x[,1] == 1)
  aus <- which(x[,1] == 0)

  #To each original position we assign a value from 1:k and the is randomly shuffled
  set.seed(seed)
  foldp <- sample(cut(seq(1, length(pre)), breaks= k, labels=FALSE))
  folda <- sample(cut(seq(1, length(aus)), breaks= k, labels=FALSE))

  # We name the "fold" vector with the positions of the original dataset
  names(foldp) <- pre
  names(folda) <-aus

  # Combination of P/A
  all <- c(foldp, folda)

  out <- list() # empty list when are stored the index of each FOLD

  for(i in 1:k){
    out[[paste0("Fold.", i)]] <- as.numeric(names(all[all==i]))
  }
  return(out)
}

### TEST _______________________________________________________________________

## It checks that all partitioned dataset keep 0/1 proportions
# mock <- data.frame(
#   occ = c(rep(0,700), rep(1,300)),
#   var1 = rnorm(1000),
#   var2 = rpois(1000, 1)
# )
#
# data.partition <- kfold(mock, k = 5)
#
# sapply(1:5, function(x){
#   tb <- table(mock[data.partition[[x]],1])
#   tb[2]/tb[1]
#   })

