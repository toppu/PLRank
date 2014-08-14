#' Find the nearest neighbors
#' 
#' Finds the nearest neighbor in X for each point in Y. 
#' X is an mx-by-n matrix and Y is an my-by-n matrix. 
#' Rows of X and Y correspond to observations and columns correspond to variables.
#' 
#' @param data an input data matrix. 
#' @param query a query data matrix
#' @param k the number of nearest neighbors to search. The default value is set to 10
#' @param algo nearest neightbor seaching algorithms. The default value is kd-tree
#' @return ???
#' @export
knn.search <- function(X, Y, k="10", algo="kd_tree") {
  
}

#' Returns complete ranks associated to the nearest neighbors
#'
#' @param data
#' @param instance
#' @export
knn.getRanks <- function(dset, instance) {
  # initialize result matrix
  ranks <- data.frame(Ranks=dset[instance[1,]])
  
  for (i in 1:dim(instance)[1]) {
    ranks[i] <- data.frame(Ranks=dset[instance[i,]])
  }
  
  return(ranks)
}
