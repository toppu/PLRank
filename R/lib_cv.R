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