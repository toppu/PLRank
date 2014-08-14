#' Perform k-fold cross validation
#' 
#' 
#' @param dset label ranking dataset
#' @param k the number of nearest neighbors to search. The default value is set to 10
#' @param nFolds the number of folds for the cross validation
#' @param nRuns the number of runs for the cross validation
#' @param alpha the default value is set to 0.05
#' @return ???
#' @examples
#' # perform the 10 runs of 10-folds cross validation on the Iris dataset
#' dset = data(data.iris)
#' cv.knn(dset, k=15, nFolds=10, nRuns=10, alpha=0.05)
#' @export

cv.knn <- function(dset, k=10, nFolds=10, nRuns=10, alpha=0.05) {
library(FNN)
#source("libs/plmm.R")
#source("libs/pl2.R")
#source("libs/lib_data.R")
#source("libs/lib_eval.R")
#source("libs/lib_knn.R")
#source("libs/lib_pmr.R")
#File <- "dataset/wine"
#dset <- read.table(File, sep=",")
K <- k
nbFold <- nFolds
nbRuns <- nRuns
nbRows <- dim(dset)[1] # number of instances (rows)
nbCols <- dim(dset)[2] # number of attributes including label ranking (columns)

# count the number of labels
s=dset[1,nbCols] # The string to search in
s=sapply(s,as.character)
p <- "L" # The character to search for
s2 <- gsub(p,"",s)
nbLabel <- numOcc <- nchar(s) - nchar(s2)

pl_h <- c(0) # number of times the 2-vase model is selected
nbClassify <- 0 # number of all test rows that have been classified
h <- 0 # logical number of loglikelihood ratio test, h=1: 2-vase model is preferred, h=0: 1-vase model is preferred 
nb <- 0

pl1_ta <- c(0) # number of Kendall's tau for 1-vase model
pl2_ta <- c(0) # number of Kendall's tau for 2-vase model
pl3_ta <- c(0) # number of Kendall's tau for mix model (1-vase and 2-vase model)

pl1_sp <- c(0) # number of Spearman rank correlation for 1-vase model
pl2_sp <- c(0) # number of Spearman rank correlation for 2-vase model
pl3_sp <- c(0) # number of Spearman rank correlation mix model (1-vase and 2-vase model)

# Average Kendall's tau for each run in 10-fold cross-validation
pl1_ta_cv_avg <- c(0) 
pl2_ta_cv_avg <- c(0)
pl3_ta_cv_avg <- c(0)

# Average Spearman rank correlation for each run in k-fold cross-validation
pl1_sp_cv_avg <- c(0)
pl2_sp_cv_avg <- c(0)
pl3_sp_cv_avg <- c(0)

# Average likelihood ratio test
pl1_ll_avg <- c(0)
pl2_ll_avg <- c(0)
pl3_ll_avg <- c(0)

dset <- data.frame(scale(dset[,1:dim(dset)[2]-1]), Ranks=dset[,dim(dset)[2]]) # Standardized the features
dset <- dset[order(dset$Ranks),] # Sort dataset according to their class
dset <- sortRandom(dset) # Sort dataset according random random column
dset <- dset[2:dim(dset)[2]]
fold <- cv.getFolds(nbRows, nbFold) # get fold index

# run cross validation
for (run in 1:nbRuns) {
  
  message("Evaluating fold...", run)
  
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
  ranks <- knn.getRanks(trainClasses, instance$nn.index)
  
  #
  # Loop through all ranks in the test dataset and estimate the parameters
  #
  for (nbTest in 1:dim(ranks)[2]) {
    nbClassify=nbClassify+1
    
    #
    # Modify the ranks to match with the data structure requied in pl.R and pl.extend.R
    #
    temp <- gsub('L','',ranks[,nbTest])
    temp <- gsub('>',' ',temp)
    test <- data.frame(strsplit(temp,' '))
    colnames(test) <- c(1:dim(test)[2])
    test <- t(test) # all ranks in each row of test set
    class(test) = "numeric"
    #data.rank.plmm <- formDsetNascar(test)
    
    # initial result variables
    if (nbTest==1) {
      pl1_para <- matrix(0,dim(ranks)[2], dim(test)[2]) # estimate parameters
      pl2_para <- pl1_para # estimate parameters
      pl1_rank <- pl1_para # rank order of 1-vase model
      pl2_rank <- pl1_para # rank order of 2-vase model
      pl3_rank <- pl1_para # rank order mixed between 1-vase and selected 2-vase model
      pl1_ll <- c(0)
      pl2_ll <- c(0)
      pl3_ll <- c(0)
    }
    
    skipDset <- FALSE 
    skip2vase <- FALSE
    # Check if the object is always ranked first or last
    # we then don't apply the algorithm on this object/dataset
    #if (check_rank) {
    #  r <- checkRank(test)
    #  if ( r$rank_first || r$rank_last ) 
    #  {skipDset = TRUE}
    # }
    
    # Skip 2vase-model
    #if (dim(data.rank)[1]==1) {
    #  skip2vase <- TRUE 
    #}
    
    # Check if the optimization is not successful
    # we don't account this in the calculation
    #tmp1 <- try(pl(data.rank), TRUE)
    
    # plmm
    tmp1 <- est.PL.MM(test)
    if (is.nan(tmp1$loglikelihood)) { 
      pl1_ll[nbTest] <- 3333 
      message("Nan produced")
      skip2vase <- TRUE
    } else {
      pl1_ll[nbTest] <- tmp1$loglikelihood
    }
    pl1_rank[nbTest,] <- order(tmp1$para, decreasing = TRUE) # rank index
    
    # pl
    #pl1_rank[nbTest,] <- order(tmp1$par, decreasing = TRUE) # rank index
    #pl1_ll[nbTest] <- -tmp1$value
    #browser()
    # pl2
    if (skip2vase) {
      pl2_rank[nbTest,] <- pl1_rank[nbTest,]
      pl2_ll[nbTest] <- pl1_ll[nbTest]
    } else {
      tmp2 <- getLL(test) # getLL has try() to check error
      pl2_ll[nbTest] <- tmp2$loglikelihood # loglikelihood
      tmp3 <- getParaV2(tmp2$para, tmp2$L) # parameters and cutpoint (L)
      pl2_rank[nbTest,] <- tmp3$rank
    }
    
    ###########################
    # loglikelihood ratio test
    ###########################
    h <- eval.LRatioTest(pl2_ll[nbTest], pl1_ll[nbTest], nbLabel, alpha) # degree of freedom = nbLabel-1
    if (h == 1) { # 2-vase model is selected
      pl3_rank[nbTest,] <- pl2_rank[nbTest,]
      pl_h[nbClassify] <- 1
      pl3_ll[nbTest] <- pl2_ll[nbTest]
    } else { # 1-vase model is selected
      pl3_rank[nbTest,] <- pl1_rank[nbTest,]
      pl_h[nbClassify] <- 0
      pl3_ll[nbTest] <- pl1_ll[nbTest]
    }
    
    #########################
    # Evaulations
    ######################### 
    message(nbClassify,"/",nbRows)
    # Construct a matrix
    t1 <- data.frame(as.numeric(actual_rank[nbTest,]), pl1_rank[nbTest,])
    t2 <- data.frame(as.numeric(actual_rank[nbTest,]), pl2_rank[nbTest,])
    t3 <- data.frame(as.numeric(actual_rank[nbTest,]), pl3_rank[nbTest,])
    
    # Kendall's tau
    pl1_ta[nbTest] <- eval.KendallTau(t1)
    pl2_ta[nbTest] <- eval.KendallTau(t2)
    pl3_ta[nbTest] <- eval.KendallTau(t3) 
    
    # Spearman rank correlation
    pl1_sp[nbTest]  <- eval.SpearmanRho(t1)
    pl2_sp[nbTest]  <- eval.SpearmanRho(t2)
    pl3_sp[nbTest]  <- eval.SpearmanRho(t3)
    
  }
  
  nb <- nb+1
  pl1_ta_cv_avg[nb] <- mean(pl1_ta[which(pl1_ta!=3333)])
  pl2_ta_cv_avg[nb] <- mean(pl2_ta[which(pl2_ta!=3333)])
  pl3_ta_cv_avg[nb] <- mean(pl3_ta[which(pl3_ta!=3333)])
  pl1_sp_cv_avg[nb] <- mean(pl1_sp[which(pl1_sp!=3333)])
  pl2_sp_cv_avg[nb] <- mean(pl2_sp[which(pl2_sp!=3333)])
  pl3_sp_cv_avg[nb] <- mean(pl3_sp[which(pl3_sp!=3333)])
  pl1_ll_avg[nb] <- mean(pl1_ll[which(pl1_ll!=3333)])
  pl2_ll_avg[nb] <- mean(pl2_ll[which(pl2_ll!=3333)])
  pl3_ll_avg[nb] <- mean(pl3_ll[which(pl3_ll!=3333)])
  
} #end cross validation

result <- list("pl1_ta"=mean(pl1_ta_cv_avg[which(pl1_ta_cv_avg!=3333)]), 
               "pl2_ta"=mean(pl2_ta_cv_avg[which(pl2_ta_cv_avg!=3333)]), 
               "pl3_ta"=mean(pl3_ta_cv_avg[which(pl3_ta_cv_avg!=3333)]),
               "pl1_ta_cv"=pl1_ta_cv_avg,
               "pl2_ta_cv"=pl2_ta_cv_avg,
               "pl3_ta_cv"=pl3_ta_cv_avg,
               "pl1_sp"=mean(pl1_sp_cv_avg[which(pl1_sp_cv_avg!=3333)]), 
               "pl2_sp"=mean(pl2_sp_cv_avg[which(pl2_sp_cv_avg!=3333)]), 
               "pl3_sp"=mean(pl3_sp_cv_avg[which(pl3_sp_cv_avg!=3333)]),
               "pl1_sp_cv"=pl1_sp_cv_avg,
               "pl2_sp_cv"=pl2_sp_cv_avg,
               "pl3_sp_cv"=pl3_sp_cv_avg,
               "pl1_ll"=mean(pl1_ll_avg[which(pl1_ll_avg!=3333)]), 
               "pl2_ll"=mean(pl2_ll_avg[which(pl2_ll_avg!=3333)]), 
               "pl3_ll"=mean(pl3_ll_avg[which(pl3_ll_avg!=3333)]), 
               "pl_h"=pl_h)

}

#
# sortRandom create a randomCol using for sorting the dataset
# sort the dataset accoding to the randomCol 
# this gives cross validation randomness
#
sortRandom <- function(dset) {
  
  dsetRowNumber <- dim(dset)[1] # Total number of rows
  set.seed(1)
  randomCol <- runif(dsetRowNumber) # A random column
  temp <- cbind(randomCol,dset)
  dsetSorted <- temp[order(temp$randomCol),]
  
  return(dsetSorted)
  
}

#
# check is any object is always ranked first or last
#
checkRank <- function(rank) {
  rank_first = FALSE
  rank_last = FALSE
  ncols = dim(rank)[2]
  
  # always rank first
  if ( length(unique(rank[,1])) == 1 )
    rank_first = TRUE
  
  # always rank last
  if ( length(unique(rank[,ncols])) == 1 )
    rank_last = TRUE
  
  result <- list("rank_first"=rank_first, "rank_last"=rank_last)
}

#
# Find the switch point (L) that maximizes the likelihood
#
# input: 
# - dset
# return: 
# - switch point
# - loglikelihood
# - parameters
getLL <- function(dset) {
  
  nitem <- dim(dset)[2]
  ll <- c(0)
  result.extend <- matrix(0,nitem-1,nitem*2)
  
  for (i in 1:(nitem-2)) {
    tmp <- try(est.PL2.NR(dset, i),TRUE)
    if ( inherits(tmp, "try-error") ) {
      ll[i] <- -999999 # this won't be select
    } else {
      result.extend[i,] <- tmp$para
      ll[i] <- tmp$loglikelihood
    }
  }
  
  L <- which.max(ll)
  result <- list("L"=L,"loglikelihood"=ll[L], "para"=result.extend[L,])
  
  return(result)
  
}

#
# getParaV2 get the most probable rank
#
getParaV2 <- function(para, L) {
  
  labels <- length(para)/2
  v_rank <- c(0)
  v_para <- c(0)
  v1 <- para[1:labels]/sum(para[1:labels])
  r1 <- order(v1, decreasing = TRUE) # rank index
  v2 <- para[(labels+1):(2*labels)]/sum(para[(labels+1):(2*labels)])
  r2 <- order(v2, decreasing = TRUE) # rank index
  
  # skip V2 if V2 contains the same parameters/rank (e.g. 1 1 1)
  if (all(r2==1)) {
    v_rank <- r1
    v_para <- v1
  } else {
    v_rank[1:L] <- r1[1:L]
    v_para[1:L] <- v1[r1[1:L]]
    v_rank[(L+1):labels] <- setdiff(r2,v_rank) 
    v_para[(L+1):labels] <- v2[v_rank[(L+1):labels]]
  }
  v_para = v_para/sum(v_para)
  result <- list("para"=v_para[order(v_rank)], "rank"=v_rank)
  
  return(result)
}