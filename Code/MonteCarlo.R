if(!require(truncnorm)) install.packages("truncnorm")

monteCarlo <- function(reps, sampleSize, mean1, mean2,
                                  sd1, sd2, distributionType = "Normal",
                                  min1 = NULL, min2 = NULL,
                                  max1 = NULL, max2 = NULL) {
  # Purpose:
  #   Run a monte carlo simulation depending on the distributionType
  #   It can be normally distributed or truncated
  # Inputs:
  #   reps - integer - number of monte carlo repetitions
  #   sampleSize - integer - the size of the samples to create
  #   mean1, mean2, sd1, sd2 - decimals - parameters for the distributions
  #   distributionType - character - can be "Normal" or "Truncate"
  #   min1, min2, max1, max2 - decimals - parameters for the distributions
  # Outputs:
  #   simulations - 3 dimentional array - the rows represent the samples,
  # the columns represent the Groups and the 3rd dimension represent the 
  # repetitions or resamples
  
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
