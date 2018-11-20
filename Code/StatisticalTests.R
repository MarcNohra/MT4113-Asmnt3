parametricTesting <- function(simulations) {
  # Purpose:
  #   Calculate T(x) for the simulated data.
  #   This function takes all the simulations and perfoms T(x) on every sample
  # Input:
  #   simulations - dataset
  # Output:
  # 
  
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
  return(t.test(simulations[, 1, index], simulations[, 2, index])$p.value)
}

nonParametricTesting.parallel <- function(index, simulations) {
  return(wilcox.test(simulations[, 1, index], simulations[, 2, index])$p.value)
}

statisticalTests <- function(index, simulations) {
  pValues <- matrix(data = NA, nrow = 1, ncol = 2)
  
  pValues[1, 1] <- t.test(simulations[, 1, index], simulations[, 2, index])$p.value
  pValues[1, 2] <- wilcox.test(simulations[, 1, index], simulations[, 2, index])$p.value
  
  return(pValues)
}
