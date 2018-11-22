errorTypesRatio <- function(pValues, alpha){
  # Purpose:
  #   This function calculate the statistical power or size of a test.
  #   Since the power and size can be calculated using the same formula
  #   we merge them into one function and returns the size or power depending
  #   on the simulated data
  # Inputs:
  #   pValues - numeric vector - the list of pValues to check
  #   alpha - decimal - the significance level
  
  return((length(which(pValues <= alpha))) / length(pValues))
}
