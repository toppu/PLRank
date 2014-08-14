
#' Get the index of the each fold in train and test dataset
#'
#' @param nb.row number of rows in dataset
#' @param nb.fold number of folds
#' @return index of each fold (train and test dataset)
#' @details
#' An example for the input data of 145 instances, 10 folds \cr
#' The output:
#' \tabular{ccccccc}{
#' Fold \tab TestS \tab TestE \tab TrainS1 \tab TrainE1 \tab TrainS2 \tab TrainE2 \cr
#' 1 \tab 1 \tab 15 \tab 16 \tab 145 \tab - \tab - \cr
#' 2 \tab 16 \tab 30 \tab 1 \tab 15 \tab 31 \tab 145 \cr
#' 3 \tab 31 \tab 45 \tab 1 \tab 30 \tab 46 \tab 145 \cr
#' . \cr
#' . \cr
#' . \cr
#' 9 \tab 121 \tab 135 \tab 1 \tab 120 \tab 136 \tab 145 \cr
#' 10 \tab 136 \tab 145 \tab 1 \tab 135 \tab - \tab -
#' }
#' @examples
#' nb.row = 145
#' nb.fold = 10
#' eval.CV.getFolds(nb.row, nb.fold) 
#' @export
cv.getFolds <- function(nb.row, nb.fold) {
  
  # Divide the dataset according to the number of folds 
  numberOfRowsPerFold <- ceiling(nb.row/nb.fold)
  testS <- 0 # Test index of start row at fold i-th
  testE <- 0 # Test index of end row at fold i-th
  trainS1 <- 0 # Train index of start row at fold i-th, 1st portion
  trainE1 <- 0 # Train index of end row at fold i-th, 1st portion
  trainS2 <- 0 # Train index of start row at fold i-th, 2nd portion
  trainE2 <- 0 # Train index of end row at fold i-th, 2nd portion
  result <- data.frame(Fold=0, TestS=0, TestE=0, TrainS1=0, TrainE1=0, TrainS2=0, TrainE2=0)
  
  i<-0
  for (startOfRow in seq(1,nb.row,numberOfRowsPerFold)) {
    i<-i+1
    testS <- startOfRow
    testE <- startOfRow+numberOfRowsPerFold-1
    
    if (startOfRow == 1) { # 1st fold
      trainS1 <- testE+1
      trainE1 <- nb.row
    } else {
      if (i == nb.fold) {  # last fold
        testE <- nb.row
        trainS1 <- 1
        trainE1 <- testS-1
        trainS2 <- 0
        trainE2 <- 0
      } else { # folds in between 1st and lat
        trainS1 <- 1
        trainE1 <- testS-1
        trainS2 <- testE+1
        trainE2 <- nb.row
      }
    }        
    
    result[i,] <- data.frame(i, testS, testE, trainS1, trainE1, trainS2, trainE2)
    
  } # end for
  
  return(result)
  
}