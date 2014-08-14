#' Mean absolute error performance
#'
#' eval.MAE measures the performace as the mean of absolute errors of the actual parameters and predicted parameters.
#' @param actual a numeric vector of actual parameters. 
#' @param predicted a numeric vector of predicted parameters.
#' @return  The mean absolute error.
#' @examples   
#' actual <- c(1,2,1)
#' predicted <- c(1,1,1)
#' eval.MAE(actual,predicted)
#' @export
eval.MAE <- function(actual, predicted) {
  
  err <- actual-predicted
  abs_err <- abs(err) # the absolute value of the error
  maeAbs <- mean(abs_err) # the mean of the absolute error
  
  return(maeAbs)
}

#' Likelihood ratio test
#'
#' eval.LRatioTest performs a statistical test using the loglikelihood objective function
#' to compare the fits of two models. 
#'  
#' @param   altLogL loglikelihood of an alternative model.
#' @param   nullLogL loglikelihood of a null model.
#' @param   dof degree of freedoms. dof = m-1 where m is the number of labels to rank
#' @param   alpha significance level (default=0.05).
#' @return  A logical value (h-value) with the rejection decision from conducting a likelihood ratio test of model specification.
#' @examples
#' altLogL = c(-10.87)   # loglikelihood from an alternative model
#' nullLogL = c(-10.039) # loglikelihood from a hull model
#' dof = 2 # degrees of freedom
#' eval.LRatioTest(altLogL, nullLogL, dof)       
#' @export
eval.LRatioTest <- function(altLogL, nullLogL, dof, alpha=0.05) {
  
  h<-0
  
  lrs <- 2*abs(altLogL-nullLogL)
  p <- pchisq(lrs, dof, lower.tail = FALSE)
  if (p<alpha) h<-1
  
  return(h)
}


#' Number of concordant pairs (Kendall's tau)
#'
#' eval.getConcordantPairs returns the number of concordant pairs referred to 
#' Kendall's tau correlation coefficient definition
#' 
#' @param   t  m*2 matrix where the first column is the actual rank
#' and the second column is the predicted rank
#' @return  The number of concordant pairs
#' @examples
#' actula_rank = c(1,2,3)     # L1>L2>L3
#' predicted_rank = c(1,3,2)  # L1>L3>L2
#' t <- data.frame(actula_rank, predicted_rank)
#' eval.getConcordantPairs(t)
#' @export
eval.getConcordantPairs <- function(t) {
  c <- 0
  t <- t[order(t[,1]),] # sort the data according to the 1st col (actual rank)
  nrows <- dim(t)[1]
  for (i in 1:(nrows-1)) {
    c <- c+sum(t[(i+1):nrows,2]>t[i,2])
  }
  
  return(c)
}

#' Number of disconcordant pairs (Kendall's tau)
#'
#' eval.getDisconcordantPairs returns the number of disconcordant pairs referred to 
#' Kendall's tau correlation coefficient definition
#' 
#' @param   t   m*2 matrix where the first column is the actual rank
#' and the second column is the predicted rank
#' @return  The number of disconcordant pairs
#' @examples
#' #' actula_rank = c(1,2,3)     # L1>L2>L3
#' predicted_rank = c(1,3,2)  # L1>L3>L2
#' t <- data.frame(actula_rank, predicted_rank)
#' eval.getDisconcordantPairs(t)
#' @export
eval.getDisconcordantPairs <- function(t) {
  d <- 0
  t <- t[order(t[,1]),] # sort the data according to the 1st col (actual rank)
  nrows <- dim(t)[1]
  for (i in 1:(nrows-1)) {
    d <- d+sum(t[(i+1):nrows,2]<t[i,2])
  }
  
  return(d)
}

#' Kendall's tau rank correlation coefficient
#'
#' eval.KendallTau returns Kendall's tau rank correlation coefficient.
#' 
#' @param t   m*2 matrix where
#'   : 1st column is the actual rank
#'   : 2nd column is the predicted rank
#' @return  Kendall's tau rank correlation coefficient which is based on this fomular:
#'   C-D / (1/2)n(n-1) where
#'   C is number of concordant pairs
#'   D is number of disconcordant pairs
#'   n is number of labels to rank
#' @examples
#' actula_rank = c(1,2,3)     # L1>L2>L3
#' predicted_rank = c(1,3,2)  # L1>L3>L2
#' t <- data.frame(actula_rank, predicted_rank)
#' eval.KendallTau(t)
#' @export 
eval.KendallTau <- function(t) {
  n <- dim(t)[1] # number of labels/ranks
  t <- t[order(t[,1]),] # sort the data according to the 1st col (actual rank)
  C <- eval.getConcordantPairs(t) # number of concordant pairs
  D <- eval.getDisconcordantPairs(t) # number of disconcordant pairs
  tau_a <- (C-D) / (n*(n-1)/2) # tau a
  
  return(tau_a)
}

#' Spearman's rank correlation coefficient
#'
#' eval.SpearmanRho returns the Spearman's rank correlation coefficient or Spearman's rho
#' @param t  m*2 matrix where
#'   : 1st column is the actual rank
#'   : 2nd column is the predicted rank
#' @return Spearman's rho which is based on this fomular:
#' 1 - ( (6*sum(d^2))/n(n^2-1) ) , where d is a displacement of each element in the rank
#' @examples
#' actula_rank = c(1,2,3)     # L1>L2>L3
#' predicted_rank = c(1,3,2)  # L1>L3>L2
#' t <- data.frame(actula_rank, predicted_rank)
#' eval.SpearmanRho(t)
#' @export
eval.SpearmanRho <- function(t) {
  n <- dim(t)[1] # number of labels/ranks
  d <- (t[,1]-t[,2]) # Displacement: distance an element i moved due to σ =|i − σ(i)|
  sp_rank <- 1 - ( (6*sum(d^2)) / (n*(n^2-1)) )
  
  return(sp_rank)
}

