if(!require(truncnorm)) install.packages("truncnorm")

# Monte Carlo Simulations -------------------------------------------------

monteCarlo <- function(seedNumber, reps, sampleSize, exp_mean, fit.beta.1,
                       fit.beta.2, scale.beta) {
  # Purpose:
  #   Run a monte carlo simulation
  # Inputs:
  #   seedNumber - integer - parameter to reproduce the same random numbers
  #   reps - integer - number of monte carlo repetitions
  #   sampleSize - integer - the size of the samples to create
  # Outputs:
  #   
  
  # Set the seed for reproducibility
  set.seed(seedNumber)
  
  # Initialising the simulations variable to return
  simulations <- array(data = NA, dim = c(sampleSize, 2, reps),
                       dimnames = list(c(), c("Poor", "Rich"), c()))
  
  for(i in 1:reps) {
    # Beta distribution
    randBeta <- rbeta(sampleSize,fit.beta.1, fit.beta.2)
    randBeta <- randBeta * scale.beta
    
    randExp <- rbeta(sampleSize,fit.beta.1, fit.beta.2)
    randExp <- randExp * scale.beta
    
    # Exponential distribution
    # randExp <- rexp(sampleSize, 1/exp_mean)
    
    # Insert the data to the simulations variable
    simulations[, 1, i] <- randBeta
    simulations[, 2, i] <- randExp
  }
  return(simulations)
}

monteCarlo.NormalDist <- function(seedNumber, reps, sampleSize, mean1, mean2,
                                  sd1, sd2, distributionType = "Normal",
                                  min1 = NULL, min2 = NULL,
                                  max1 = NULL, max2 = NULL) {
  # Purpose:
  #   Run a monte carlo simulation
  # Inputs:
  #   seedNumber - integer - parameter to reproduce the same random numbers
  #   reps - integer - number of monte carlo repetitions
  #   sampleSize - integer - the size of the samples to create
  # Outputs:
  #   
  
  # Set the seed for reproducibility
  set.seed(seedNumber)
  
  # Initialising the simulations variable to return
  # Creating a 3d array, the rows represent the samples, the columns represent
  # the parameter (rich/poor) and the depth represents the simulations
  simulations <- array(data = NA, dim = c(sampleSize, 2, reps),
                       dimnames = list(c(), c("Poor", "Rich"), c()))
  
  # Initialise the distribution vectors
  poorLifeExp <- numeric(sampleSize)
  richLifeExp <- numeric(sampleSize)
  
  for(i in 1:reps) {
    # Check for the requested distribution type
    if(distributionType == "Normal") {
      # Normal Life Expectancy distribution for the poor countries
      poorLifeExp <- rnorm(sampleSize, mean1, sd1)
      
      # Normal Life Expectancy distribution for the rich countries
      richLifeExp <- rnorm(sampleSize, mean2, sd2)
    }
    
    else if(distributionType == "Truncate") {
      # Truncated Life Expectancy distribution for the poor countries
      poorLifeExp <- rtruncnorm(sampleSize, min1, max1, mean1, sd1)
      
      # Truncated Life Expectancy distribution for the rich countries
      richLifeExp <- rtruncnorm(sampleSize, min2, max2, mean2, sd2)
    }
    
    else{
      stop("Parameter error")
    }
    
    # Insert the data to the simulations variable
    simulations[, 1, i] <- poorLifeExp
    simulations[, 2, i] <- richLifeExp
  }
  return(simulations)
}
