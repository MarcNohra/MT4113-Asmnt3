errorTypesRatio <- function(pValues, alpha){
  return((length(which(pValues <= alpha))) / length(pValues))
}
