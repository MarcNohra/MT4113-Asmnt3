parametricTesting <- function(simulations) {
  # Purpose:
  #   Calculate the parametric test for the simulated data.
  #   This function takes all the simulations and perfoms T(x) on every sample,
  #   it returns the pValues of every sample
  # Input:
  #   simulations - dataset
  # Output:
  # pValues - numeric vector
  
  # Get the number of samples
  numberOfSimulations <- dim(simulations)[3]
  
  # Initialising the p-value vector
  pValues <- numeric(numberOfSimulations)
  
  for(i in 1:numberOfSimulations){
    # Get the p-value of every simulation
    pValues[i] <- t.test(simulations[, 1, i], simulations[, 2, i])$p.value
  }
  return(pValues)
}

nonParametricTesting <- function(simulations){
  # Purpose:
  #   Calculate the non-parametric test for the simulated data.
  #   This function takes all the simulations and perfoms T(x) on every sample,
  #   it returns the pValues of every sample
  # Input:
  #   simulations - dataset
  # Output:
  # pValues - numeric vector
  
  # Get the number of samples
  numberOfSimulations <- dim(simulations)[3]
  
  # Initialising the p-value vector
  pValues <- numeric(numberOfSimulations)
  
  for(i in 1:numberOfSimulations) {
    # Get the p-value of every simulation
    pValues[i] <- wilcox.test(simulations[, 1, i], simulations[, 2, i])$p.value
  }
  return(pValues)
}

parametricTesting.parallel <- function(index, simulations) {
  # Purpose:
  #   Calculate the parametric test for the simulated data.
  #   This function is another version of parametricTesting but for the
  #   parallel version
  #   It returns the pValue of a sample
  # Input:
  #   index - integer - the index of the selected simulation
  #   simulations - dataset
  # Output:
  # the pValue of the test
  
  return(t.test(simulations[, 1, index], simulations[, 2, index])$p.value)
}

nonParametricTesting.parallel <- function(index, simulations) {
  # Purpose:
  #   Calculate the non-parametric test for the simulated data.
  #   This function is another version of nonParametricTesting but for the
  #   parallel version
  #   It returns the pValue of a sample
  # Input:
  #   index - integer - the index of the selected simulation
  #   simulations - dataset
  # Output:
  # the pValue of the test
  
  return(wilcox.test(simulations[, 1, index], simulations[, 2, index])$p.value)
}

statisticalTests <- function(index, simulations) {
  # Purpose:
  #   Calculate the parametric and non-parametric test for the simulated data.
  #   This function is used for the optimised parallel function to calculate 
  #   the 2 pvalues at the same time
  #   It returns the pValues of a sample for the parametric and non-parametric 
  # Input:
  #   index - integer - the index of the selected simulation
  #   simulations - dataset
  # Output:
  # pValues - matrix
  
  pValues <- matrix(data = NA, nrow = 1, ncol = 2)
  
  pValues[1, 1] <- t.test(simulations[, 1, index], simulations[, 2, index])$p.value
  pValues[1, 2] <- wilcox.test(simulations[, 1, index], simulations[, 2, index])$p.value
  
  return(pValues)
}
