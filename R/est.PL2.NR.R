#' NR method for estimating Plackett-Luce (2-vases model) parameters
#' 
#' uses Newton-Raphson algorithm to fit the Plackett-Luce with 2-vases model. 
#' 
#' @param dset ranking dataset
#' @param L switch point between the first vase and second vase
#' @return Estimated parameters, log-likelihood and number of iterations
#' @export
est.PL2.NR <- function(dset, L){
  ## N = number of rankings
  ## k = number of items
  ## L = number of the last item in Vase 1
  ## L between 1 and k-1, i.e. L=k-1 is equivalent to classical Luce model 
  ## 2-VASE extended Plackett-Luce model (if L<nb.item-1)
  ## Plackett-Luce model (if L=nb.item-1)
  dset <- formDsetPLNR(dset) # convert rank data
  oset <- rinv(dset)
  ## oset = inverse(pi) 
  ## oset[i,j] = the item that has rank j in the i-th ranking
  ## dim(oset) = N*(k+1)
  ## last colomn of oset = number of repetitions in each ranking
  nitem <- ncol(dset)-1
  
  ###########################################################
  ## - loglikelihood function
  ###########################################################
  ## oset[i,j] = the item that has rank j in the i-th ranking
  ## util[l,j] = log V(l,j) = log(independent utility of item j in vase l)
  ## 2-vase model : l=1 if j <= L; l=2 if j>L
  ## L between 1 and k-1
  loglik_pl_extend <- function(util,L){
    pr <- rep(1,nrow(oset))
    ## To get centered log V 
    ## dim(util = 2 * k)
    util <- t(matrix(util,nitem,2))
    util <- util - rowMeans(util)
    deno <- rep(1,2)
    
    ## To get P(pi_n|V)
    for (i in 1:nrow(oset)) {
      #browser()
      deno[1] <- sum(exp(util[1,as.numeric(oset[i,1:nitem])])) 
      deno[2] <- sum(exp(util[2,as.numeric(oset[i,(L+1):nitem])])) ## L <= nitem - 1 !
      
      for (j in 1:(nitem-1)){
        if(j <= L) {
          l=1
        } else {
          l=2
        }
        #print(j)
        #browser()
        deno_bis <- sum(exp(util[l, as.numeric(oset[i,j:nitem])]))
        pr[i] <- pr[i] * exp(util[l,oset[i,j]]) / deno[l]
        deno[l] <- deno[l] - exp(util[l,oset[i,j]]) 
      }
    }
    ## To calculate logL = sum_{n=1}^N log P(pi_n|V)
    ## Then convert to - logL  (to be minimized)
    ll <- rep(0,nrow(oset))
    for (i in 1:nrow(oset)){
      ll[i] = -log(pr[i])*oset[i,(nitem+1)]
    }
    sum(ll)
  }
  
  ###########################################################
  ## - loglikelihood function (1-vase model)
  ###########################################################
  loglik_pl <- function(util){
    pr <- rep(1,nrow(oset))
    
    ## To get centered log V 
    temp_mean <- mean(util)
    for (i in 1:nitem){
      util[i] <- util[i] - temp_mean
    }
    
    ## To get P(pi_n|V)
    for (i in 1:nrow(oset)){
      deno <- sum(exp(util))
      for (j in 1:(nitem-1)){
        pr[i] <- pr[i] * exp(util[oset[i,j]]) / deno
        deno <- deno - exp(util[oset[i,j]])
      }
    }
    ## To calculate logL = sum_{n=1}^N log P(pi_n|V)
    ## Then convert to - logL  (to be minimized)
    ll <- rep(0,nrow(oset))
    for (i in 1:nrow(oset)){
      ll[i] = -log(pr[i])*oset[i,(nitem+1)]
    }
    sum(ll)
  }
  
  if(L < nitem-1){
    out1 <- optim(par = rep(1,2*nitem), loglik_pl_extend, L=L, method = "BFGS")
    #out1 <- optim(par = rep(1,2*nitem), loglik_pl_extend, L=L, method = "L-BFGS-B", hessian = TRUE)
    #out1 <- optim(par = rep(1,2*nitem), loglik_pl_extend, L=L, method = "L-BFGS-B", hessian = TRUE, lower = 0)
  } else {
    out1 <- optim(rep(1,nitem), loglik_pl, NULL, method = "BFGS")
    #out1 <- nlm(loglik_pl, rep(1,nitem), hessian=TRUE,steptol=1e-4, gradtol=1e-4)
  } 
  
  result = list("para"=out1$par, "loglikelihood"=-out1$value)
  return(result)
}

#
# Inverse of a ranking dataset
# rank pi -> pi^{-1}
#
rinv <- function(dset){
  nitem <- ncol(dset)-1
  temp <- matrix(data = 0, nrow = nrow(dset), ncol = nitem, byrow = TRUE)
  rorder <- matrix(data = 0, nrow = nrow(dset), ncol = nitem, byrow = TRUE)
  rorder <- dset[,1:nitem]
  
  ## compute the ordering of the observations
  for (i in 1:(nrow(rorder))){
    for (j in 1:nitem){
      for (k in 1:nitem){
        if (rorder[i,j] == k){
          temp[i,k] <- j
        }
      }
    }
  }
  return(data.frame(temp,n=dset[,(nitem+1)]))
}

#
# form dataset 
#
formDsetPLNR <- function(dset) {
  
  M <- max(dim(dset)[1]) # total number of rows (instances)
  N <- max(dim(dset)[2]) # total number columns (objects in ranks)
  f <- matrix(0,M,N) # index of X-i in Rank-j
  #c <- matrix(1,M,1) # number of repeats of Rank-j 
  for (i in 1:M) {
    for (j in 1:N) {
      f[i,j] <- which(dset[i,]==j)
    }
  }
  result <- data.frame(f)
  result <- aggregate(list(n=rep(1,nrow(result))), result, length) # Count the duplicated rows

  return(result)
  
}