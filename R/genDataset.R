#' Generate artificial dataset from single vase model distribution
#' 
#' This is useful for performing inferrence tasks for ranking model
#' @param nLabels number of labels to rank
#' @param nObs number of observations
#' @return Probability distribution and ranks
#' @examples 
#' ## Create artificial dataset with 4 labels to rank and 10 instances
#' lables = 4
#' observations = 10
#' dataset = genRank1v(lables, observations) 
#' @export
genRank1v <- function (nLabels, nObs) {
  #
  # Generate artificial datasets for single vase model
  #   generate random numbers with fixed sum (=1)
  #   draw sample ranks 
  # Input :
  #   nLabels : #of labels
  #   nObs : #of instances/observations
  # Output :
  #   data$para : generated parameters
  #   data$rank : 1 2 3 means L1>L2>L3
  #         1 2 3
  #         3 1 2
  #
  
  # Generate ramdom probabilities for objects
  tmp <- matrix(0,1,nLabels)
  ranks <- matrix(0,nObs,nLabels)
  tmp <- runif(nLabels)
  v <- tmp/sum(tmp)
  
  # Draw ranks
  for (i in 1:nObs) {
    
    index <- matrix(0,1,nLabels)
    o <- v
    o_cum <- cumsum(v)
    
    for (j in 1:nLabels-1) {
      
      draw <- runif(1)
      index <- which(o_cum>=draw) # index which prob >= than o[i]
      ranks[i,j] <- index[1] # 1st index is the drawn object
      index_new <- setdiff(which(v>0), ranks[i,])
      
      if (length(index_new) == 1) { # last rank
        ranks[i,nLabels] <- index_new
      } else {
        o_new <- o/sum(o[index_new])
        o_new[ranks[i,1:j]] <- 0 #remove the one that has been drawn
        o <- o_new
        o_cum <- cumsum(o)
        o_cum[ranks[i,1:j]] <- 0
      }
      
    } # end for
  } # end for
  
  data <- list("para"=v, "rank"=ranks)
  
  return(data)
} # end function

#' Generate artificial dataset from two vases model distribution
#' 
#' This is useful for performing inferrence tasks for ranking model
#' @param nLabels number of labels to rank
#' @param nObs number of observations
#' @param L lag or switch position between 1st vase and 2nd vase
#' @return Probability distribution for the 1st vase, 2nd vase and ranks
#' @examples 
#' ## Create artificial dataset with 4 labels to rank and 10 instances
#' lables = 4
#' observations = 10
#' L = 2
#' dataset = genRank2v(lables, observations, L) 
#' @export
genRank2v <- function (nLabels, nObs, L) {
  #
  # Generate artificial datasets for two vases model
  #   generate random numbers with fixed sum (=1)
  #   draw sample ranks    
  # 
  # Input :
  #   nLabels : #of labels
  #   nObs : #of instances/observations
  #   L : cutpoint of the 1st vase and 2nd vast
  # Output :
  #   data$para1 : generated parameters of the 1st vase
  #   data$para2 : generated parameters of the 2nd vase    
  #   data$rank : 1 2 3 means L1>L2>L3
  #         1 2 3
  #         3 1 2
  #
  
  tmp1=matrix(0,1,nLabels)
  tmp2=matrix(0,1,nLabels)
  ranks=matrix(0,nObs,nLabels)
  
  # Generate ramdom probabilities for objects
  tmp1 <- runif(nLabels)
  tmp2 <- runif(nLabels)
  v1 <- tmp1/sum(tmp1)
  v2 <- tmp2/sum(tmp2)
  
  for (i in 1:nObs) {
    
    index <- matrix(0,1,nLabels)
    o1 = v1
    o2 = v2
    o1_cum = cumsum(v1);
    o2_cum = cumsum(v2);
    
    for (j in 1:nLabels-1) {
      
      draw <- runif(1)
      if (j>L) { # switch to vase 2
        index = which(o2_cum>=draw)
        v = v2
      } else { # vase 1
        index = which(o1_cum>=draw) 
        v = v1
      }
      
      # 1st index is the drawn object
      ranks[i,j] = index[1]
      
      # Standardized the rest of objects
      index_new = setdiff(which(v>0),ranks[i,])
      
      if (length(index_new) == 1) { # this is the last rank
        ranks[i,nLabels] = index_new
      } else {
        o1_new = o1/sum(o1[index_new])
        o1_new[ranks[i,1:j]] = 0
        o1 = o1_new
        o1_cum = cumsum(o1)
        o1_cum[ranks[i,1:j]] = 0
        
        o2_new = o2/sum(o2[index_new])
        o2_new[ranks[i,1:j]] = 0
        o2 = o2_new
        o2_cum = cumsum(o2)
        o2_cum[ranks[i,1:j]] = 0
      }
      
    }
    
  }
  
  data <- list("para1"=v1, "para2"=v2, "rank"=ranks)
  
  return(data)
} # end function

genDsetPL <- function(nLabels, nObs, L, seeds) {
  #
  # Generate dataset that matches the input data structure of pl.R and pl.extend.R
  #
  # Input :
  #   nLabels : #of labels
  #   nObs : #of instances/observations
  #   L : cutpoint/switch point of the 1st vase and 2nd vast
  #
  # Output :
  #   data$para  : generated parameters (1-vase model)
  #   data$para1 : generated parameters of the 1st vase (2-vase model)
  #   data$para2 : generated parameters of the 2nd vase (2-vase model)    
  #   data$rank  : n:number of duplicated rows
  #       X1 X2 X3 n
  #       1  2  3  1    
  #       2  3  1  1    this row means X1 ranks 2nd, X2 ranks 3rd and X3 ranks 1st
  #   data$rank_org : 1 2 3 means L1>L2>L3
  #       1 2 3
  #       3 1 2
  
  if (L==0) 
    data <- genRank1v(nLabels, nObs, seeds)
  else
    data <- genRank2v(nLabels, nObs, L, seeds)
  
  ranks <- data$rank
  M <- max(dim(ranks)[1]) # total number of rows (instances)
  N <- max(dim(ranks)[2]) # total number columns (objects in ranks)
  f <- matrix(0,M,N) # index of X-i in Rank-j
  c <- matrix(1,M,1) # number of repeats of Rank-j
  
  # Transform the original rank to pl.R rank data
  for (i in 1:M) {
    for (j in 1:N) {
      f[i,j] <- which(ranks[i,]==j)
    }
  }
  
  df<-data.frame(f)
  dset<-aggregate(list(n=rep(1,nrow(df))), df, length) # Count the duplicated rows
  
  if (L==0)
    data <- list("para"=data$para, "rank"=dset, "rank_org"=ranks)
  else
    data <- list("para1"=data$para1, "para2"=data$para2, "rank"=dset, "rank_org"=ranks)
  
  return(data)
}

formDsetPL <- function(rank) {
  #
  # Transform rank datasets to match the input data structure of pl.R and pl.extend.R
  # Input : rank
  #       1 2 3
  #       3 1 2 
  # data: n-number of duplicated rows
  #       X1 X2 X3 n
  #       1  2  3  1    
  #       2  3  1  1    this row means X1 ranks 2nd, X2 ranks 3rd and X3 ranks 1st
  
  M <- max(dim(rank)[1]) # total number of rows (instances)
  N <- max(dim(rank)[2]) # total number columns (objects in ranks)
  f <- matrix(0,M,N) # index of X-i in Rank-j
  c <- matrix(1,M,1) # number of repeats of Rank-j
  
  # Transform the original rank to pl.R rank data
  for (i in 1:M) {
    for (j in 1:N) {
      f[i,j] <- which(rank[i,]==j)
    }
  }
  
  df<-data.frame(f)
  data<-aggregate(list(n=rep(1,nrow(df))), df, length) # Count the duplicated rows
  
  return(data)
}

formDsetNascar <- function(rank) {
  #
  # Transform rank datasets to the Nascar like datasets (followed Hunter)
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

#
# Find the cutpoint (L) that maximizes the likelihood
#
# input: data (rank)
getLL <- function(data) {
  
  nitem <- dim(data)[2]-1
  ll <- c(0)
  result.extend <- matrix(0,nitem-1,nitem*2)
  
  for (i in 1:(nitem-2)) {
    tmp <- try(pl2(data, i),TRUE)
    if ( inherits(tmp, "try-error") ) {
      ll[i] <- -999999 # this won't be select
    } else {
      result.extend[i,] <- tmp$par
      ll[i] <- -tmp$value
    }
  }
  
  L <- which.max(ll)
  result <- list("L"=L,"LL"=ll[L], "par"=result.extend[L,])
  
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

#
# check
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
# get actual rank
#
getActualRank <- function(rank) {
  
  actual_rank <- gsub('L','',rank)
  actual_rank <- gsub('>',' ',actual_rank)
  actual_rank <- data.frame(strsplit(actual_rank,' '))
  colnames(actual_rank) <- c(1:dim(actual_rank)[2])
  actual_rank <- t(actual_rank) 
  class(actual_rank) = "numeric"
  
  return(actual_rank)
}