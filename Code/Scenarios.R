if(!require(doParallel)) install.packages("doParallel")
if(!require(foreach)) install.packages("foreach")

scenarios <- function(sampleSizes, mean1, mean2, sd1, sd2, seedNumber = 6272,
                      reps = 1000){
  
  # Get the number of iterations
  numberOfRepetitions <- length(sampleSizes)
  
  # Initialising the strength variables (power/size)
  sizeRatio.param <- numeric(numberOfRepetitions)
  sizeRatio.nonParam <- numeric(numberOfRepetitions)
  strengthRatio <- matrix(data = NA, nrow = numberOfRepetitions,
                          ncol = 2,
                          dimnames = list(sampleSizes,
                                          c("Parametric", "Non Parametric")))
  
  # Initialise an iterator
  j <- 0
  
  for(i in sampleSizes) {
    j <- j + 1
    
    # Run the simulations
    simulations <- monteCarlo.NormalDist(seedNumber = seedNumber, reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2)
    
    # Calculate the pValues from the parametric testing
    param.pValues <- parametricTesting(simulations)
    
    # Calculate the pValues from the non paramteric tesing
    nonParam.pValues <- nonParametricTesting(simulations)
    
    # Calculate the size/power of our parametric test
    strengthRatio[j, 1] <- errorTypesRatio(param.pValues, 0.05)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(nonParam.pValues, 0.05)
  }
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}

scenarios.parallel <- function(sampleSizes, mean1, mean2, sd1, sd2, seedNumber = 6272,
                      reps = 1000){
  # Available cores
  nCores <- detectCores()
  
  # Create a cluster
  myClust <- makeCluster(nCores - 1, type = "PSOCK")
  
  # Register cluster for parallel
  # registerDoParallel(myClust)
  
  # Get the number of iterations
  numberOfRepetitions <- length(sampleSizes)
  
  # Initialising the strength variables (power/size)
  sizeRatio.param <- numeric(numberOfRepetitions)
  sizeRatio.nonParam <- numeric(numberOfRepetitions)
  strengthRatio <- matrix(data = NA, nrow = numberOfRepetitions,
                          ncol = 2,
                          dimnames = list(sampleSizes,
                                          c("Parametric", "Non Parametric")))
  
  # Initialise an iterator
  j <- 0
  
  # Initialise indexes for the cluster
  indexes <- matrix(data = 1:reps, nrow = reps, ncol = 1)
  
  for(i in sampleSizes) {
    j <- j + 1
    
    # Run the simulations
    simulations <- monteCarlo.NormalDist(seedNumber = seedNumber, reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2)
    
    # Initialise the returned parametric and non parametric pValues vectors
    param.pValues <- numeric(reps)
    nonParam.pValues <- numeric(reps)
    
    # Calculate the pValues from the parametric testing
    param.pValues <- parApply(myClust, indexes, 1, parametricTesting.parallel,
                             simulations = simulations)
    
    # Calculate the pValues from the non paramteric tesing
    nonParam.pValues <- parApply(myClust, indexes, 1, nonParametricTesting.parallel,
                                 simulations = simulations)
    
    # Calculate the size/power of our parametric test
    strengthRatio[j, 1] <- errorTypesRatio(param.pValues, 0.05)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(nonParam.pValues, 0.05)
  }
  
  # Terminate the workers
  stopCluster(myClust)
  
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}

scenarios.parallel.single <- function(sampleSizes, mean1, mean2, sd1, sd2, seedNumber = 6272,
                               reps = 1000) {
  # Available cores
  nCores <- detectCores()
  
  # Create a cluster
  myClust <- makeCluster(nCores - 1, type = "PSOCK")
  
  # Register cluster for parallel
  # registerDoParallel(myClust)
  
  # Get the number of iterations
  numberOfRepetitions <- length(sampleSizes)
  
  # Initialising the strength variables (power/size)
  sizeRatio.param <- numeric(numberOfRepetitions)
  sizeRatio.nonParam <- numeric(numberOfRepetitions)
  strengthRatio <- matrix(data = NA, nrow = numberOfRepetitions,
                          ncol = 2,
                          dimnames = list(sampleSizes,
                                          c("Parametric", "Non Parametric")))
  
  # Initialise an iterator
  j <- 0
  
  # Initialise indexes for the cluster
  indexes <- matrix(data = 1:reps, nrow = reps, ncol = 1)
  
  for(i in sampleSizes) {
    j <- j + 1
    
    # Run the simulations
    simulations <- monteCarlo.NormalDist(seedNumber = seedNumber, reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2)
    
    # Initialise the returned parametric and non parametric pValues martix
    pValues <- matrix(data = NA, nrow = reps, ncol = reps)
    
    # Calculate the pValues from both tests
    pValues <- parApply(myClust, indexes, 1, statisticalTests, 
                        simulations = simulations)
    
    # Calculate the size/power of our parametric test
    strengthRatio[j, 1] <- errorTypesRatio(pValues[1, ], 0.05)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(pValues[2, ], 0.05)
  }
  
  # Terminate the workers
  stopCluster(myClust)
  
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}
