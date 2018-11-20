# errorTypesRatio <- function(pValues, alpha, effectSize){
#   # Purpose:
#   #   Calculate the power or size of test
#   # Inputs:
#   #   pValues - matrix - list of all the pValues
#   #   alpha - decimal
#   #   
#   # Output
#   #   ...
#   # Notes:
#   #   The pValues parameter has to be...
#   numberOfSamples <- dim(pValues)[2]
#   effectSize.length <- length(effectSize)
#   
#   errorRatio <- (length(which(pValues <= alpha))) / length(pValues)
#   return(errorRatio)
# }

errorTypesRatio <- function(pValues, alpha){
  return((length(which(pValues <= alpha))) / length(pValues))
}
