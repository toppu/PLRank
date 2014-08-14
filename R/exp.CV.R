#' K-flods cross validation
#'
#' @param file lable ranking dataset
#' @param method method to fit Plackett-Luce model
#' @param k number of k-neiborhoods
#' @param nFlods number of flods in the cross validation
#' @param randomness logical indicating whether 
#' @return Kendall's tau, Spearman's rank and log-likelihood
#' @references
#' ref1
#' ref2
#' ref3
#' @examples
#' exp.CV("iris", method="plmm", k=15, nFlods=10, randomness=TRUE)
#' @export
eval.CV <- function(file, method, k=15, nFolds=10, randomness=TRUE) {

dset <- read.table(File, sep=",")
dset <- data.frame(scale(dset[,1:dim(dset)[2]-1]), Ranks=dset[,dim(dset)[2]]) # Standardized the features

if (randomness) {
  dset <- dset[order(dset$Ranks),] # Sort dataset according to their class
  dset <- sortRandom(dset) # Sort dataset according random random column
  dset <- dset[2:dim(dset)[2]]
}

pl1_ta <- c(0) # number of Kendall's tau for 1-vase model
pl1_sp <- c(0) # number of Spearman rank correlation for 1-vase model

fold <- getFold(nbRows, nbFold) # get fold index

# run cross validation
for (run in 1:nRuns) {
 
  # Test and train dataset
  testRows <- c(fold$TestS[run]:fold$TestE[run])
  trainRows <- c(fold$TrainS1[run]:fold$TrainE1[run], fold$TrainS2[run]:fold$TrainE2[run])
  testFeatures <-  dset[testRows, 1:(dim(dset)[2]-1)]
  trainFeatures <- dset[trainRows, 1:(dim(dset)[2]-1)] 
  testClasses <- dset[testRows, dim(dset)[2]]
  trainClasses <- dset[trainRows, dim(dset)[2]]
  
  # actual rank, used to compare with the estimated rank
  actual_rank <- gsub('L','',testClasses)
  actual_rank <- gsub('>',' ',actual_rank)
  actual_rank <- data.frame(strsplit(actual_rank,' '))
  colnames(actual_rank) <- c(1:dim(actual_rank)[2])
  actual_rank <- t(actual_rank) 
  class(actual_rank) = "numeric"
  
  # Search and find the ranks according to nearest neighbors
  instance <- get.knnx(trainFeatures, testFeatures, K, algo="kd_tree")
  ranks <- getRanks(trainClasses, instance$nn.index)
  
  #
  # Loop through all ranks in the test dataset and estimate the parameters
  #
  for (nTest in 1:dim(ranks)[2]) {
    temp <- gsub('L','',ranks[,nbTest])
    temp <- gsub('>',' ',temp)
    test <- data.frame(strsplit(temp,' '))
    colnames(test) <- c(1:dim(test)[2])
    test <- t(test) # all ranks in each row of test set
    class(test) = "numeric"
    ranks <- formDset(test)
    
    # initial result variables
    if (nTest==1) {
      pl1_para <- matrix(0,dim(ranks)[2], dim(test)[2]) # estimate parameters
      pl1_rank <- pl1_para # rank order of 1-vase model
      pl2_rank <- pl1_para # rank order of 2-vase model
      pl3_rank <- pl1_para # rank order mixed between 1-vase and selected 2-vase model
      pl1_ll <- c(0)

    }
  }
  
}

}


formDset <- function(rank) {
  #
  # Transform rank datasets to the Nascar like datasets
  # Input : ranks
  #       1 2 3
  #       3 1 2
  #   
  # Output : datasets that works with function plackmm()
  # Column 1:  individual ID (1 through M)
  # Column 2:  contest ID (1 through N)
  # Column 3:  rank 
  #       1 1 1
  #       2 1 2
  #       3 1 3
  #       3 2 1
  #       1 2 2
  #       2 2 3
  row <- dim(rank)[1]
  col <- dim(rank)[2]
  data <- matrix(0,row*col,3)
  
  for (i in 1:row) {
    
    start <- i*col-col+1
    end <- i*col
    
    # individual ID (1 through M)
    data[start:end,1] <- t(rank[i,])
    
    # contest ID (1 through N)
    data[start:end,2] <- i
    
    # rank
    data[start:end,3] <- 1:col
  }
  
  data<-data.frame(data)
  
  return(data)
}