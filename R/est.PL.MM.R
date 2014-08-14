#' MM method for estimating Plackett-Luce model parameters
#' 
#' est.PL.MM uses MM (minorization-maximization) algorithms to fit the Plackett-Luce model. 
#' The R code was modified from the original Matlab code (plackmm) which can be found at 
#' http://www.stat.psu.edu/~dhunter/code/btmatlab/
#'  
#' @param dset ranking dataset
#' @return Estimated parameters, log-likelihood and number of iterations
#' @references 
#' Hunter, D.R. (2004). MM algorithms for generalized Bradley-Terry models. Ann. of Stats., 32, 384-406
#' Plackett, R. (1975). The analysis of permutations. Applied Stat., 24, 193-202.
#' @examples
#' # complete rankings
#' # create 2 ranks and 3 labels
#' ranks = matrix(0,2,3) # rows:number of ranks, column:number of labels
#' ranks[1,] = c(1,2,3) # L1>L2>L3
#' ranks[2,] = c(3,1,2) # L3>L1>L2
#' est.PL.MM(ranks)
#' 
#' # incomplate rankings
#' # create 3 ranks and 3 labels
#' ranks = matrix(0,3,3) # rows:number of ranks, column:number of labels
#' ranks[1,] = c(1,2,3) # L1>L2>L3
#' ranks[2,] = c(3,1,2) # L3>L1>L2
#' ranks[3,] = c(2,1,0) # L2>L1
#' est.PL.MM(ranks)
#' @export
est.PL.MM <- function(dset) {

  dset = formDsetPLMM(dset) # Transform rank dataset to the Nascar like dataset
  M<-max(dset[,1]) # M = total # of individuals
  N<-max(dset[,2]) # N = total # of contests
  P<-max(dset[,3]) # P = largest # of individuals per contest
  f<-matrix(0,P,N) # f[i,j] = one who placed i in contest j
  r<-matrix(0,M,N) # r[i,j] = place of i in contest j, modified so that it correctly addresses the vectorized form of f.
  for(i in 1:dim(dset)[1]) {
    f[dset[i,3],dset[i,2]] <- dset[i,1]
    r[dset[i,1],dset[i,2]] <- dset[i,3] + P*(dset[i,2]-1)
  }
  r2 <- r
  
  w <- matrix(0,M,1) # w[i] = # times i placed higher than last
  pp <- colSums(f>0) # pp[j] = # individuals in contest j
  for(i in 1:N) {
    tmp <- f[1:(pp[i]-1),i]
    w[tmp] <- 1+w[tmp]
  }
  pp <- pp+seq(0,(N-1)*P,P) # Indices in f of lastplace finishers
  gamma <- matrix(1,M,1) # (unscaled) initial gamma vector
  dgamma <- 1
  iterations <- 0
  s <- 0.0001
  ll <- 0
  ll_old <- 1
  ll_change <- 0
  max_iteration = 500
  
  while ( max(svd(dgamma)$d) > s ) { # Matrix norm, 2-norm
    iterations <- iterations+1
    if ( abs(ll-ll_old) < s || is.nan(abs(ll-ll_old)) || iterations > max_iteration) {
      break # exit the loop
    }
    
    g <- f
    g[f>0] <- gamma[f[f>0]]
    g <- apply(g[seq(P,1,-1),],2,cumsum) # cumsum(g(P:-1:1,:));
    g <- g[seq(P,1,-1),]
    g[pp] <- 0
    g[g>0] <- 1./g[g>0]
    #  At this point, g[i,j] should be the reciprocal of the
    #  sum of gamma's for places i and higher in the jth contest
    #  except for i=lastplace.
    
    # To calculate the loglikelihood
    ll_old <- ll
    ll <- t(w)%*%log(gamma)+sum(sum(log(g[g>0])))
    
    g <- apply(g,2,cumsum)
    #  Now g[i,j] should be the sum of all the denominators
    #  for ith place in the jth contest.
    
    #  Now for the denominator in gamma[i], we need to 
    #  add up all the g[r[i,j],j] for nonzero r[i,j].
    #  The r matrix was initialized so that its entries would
    #  correctly index the appropriate entries in the g
    #  matrix if the g matrix were vectorized.
    r2[r>0] <- g[r[r>0]]
    sr2 <- rowSums(r2)
    newgamma <- w/sr2
    dgamma <- newgamma-gamma;
    gamma <- newgamma;
    
  } # end while loop
  
  v <- gamma/sum(gamma)
  result <- list("para"=v,"loglikelihood"=ll, "iterations"=iterations)
  
  return(result)
  
}


formDsetPLMM <- function(rank) {
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
