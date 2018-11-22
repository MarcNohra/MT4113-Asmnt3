if(!require(doParallel)) install.packages("doParallel")
if(!require(foreach)) install.packages("foreach")
if(!require(gridExtra)) install.packages("gridExtra")

scenarios <- function(sampleSizes, mean1, mean2, sd1, sd2, seedNumber = 6272,
                      reps = 1000, alpha = 0.05){
  
  # Set the seed for reproducibility
  set.seed(seedNumber)
  
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
    simulations <- monteCarlo.NormalDist(reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2)
    
    # Calculate the pValues from the parametric testing
    param.pValues <- parametricTesting(simulations)
    
    # Calculate the pValues from the non paramteric tesing
    nonParam.pValues <- nonParametricTesting(simulations)
    
    # Calculate the size/power of our parametric test
    strengthRatio[j, 1] <- errorTypesRatio(param.pValues, alpha)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(nonParam.pValues, alpha)
  }
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}

scenarios.parallel <- function(sampleSizes, mean1, mean2, sd1, sd2, seedNumber = 6272,
                      reps = 1000, alpha = 0.05, distributionType = "Normal",
                      min1 = NULL, min2 = NULL,
                      max1 = NULL, max2 = NULL){
  # Set the seed for reproducibility
  set.seed(seedNumber)
  
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
    simulations <- monteCarlo.NormalDist(reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2,
                                         distributionType = distributionType,
                                         min1 = min1, min2 = min2,
                                         max1 = max1, max2 = max2)
    
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
    strengthRatio[j, 1] <- errorTypesRatio(param.pValues, alpha)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(nonParam.pValues, alpha)
  }
  
  # Terminate the workers
  stopCluster(myClust)
  
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}

scenarios.parallel.single <- function(sampleSizes, mean1, mean2, sd1, sd2,
                                      reps = 1000, alpha = 0.05,
                                      distributionType = "Normal",
                                      min1 = NULL, min2 = NULL,
                                      max1 = NULL, max2 = NULL) {
  
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
    simulations <- monteCarlo.NormalDist(reps = reps,
                                         sampleSize = i,
                                         mean1 = mean1, mean2 = mean2,
                                         sd1 = sd1, sd2 = sd2,
                                         distributionType = distributionType,
                                         min1 = min1, min2 = min2,
                                         max1 = max1, max2 = max2)
    
    # Initialise the returned parametric and non parametric pValues martix
    pValues <- matrix(data = NA, nrow = reps, ncol = 2)
    
    # Calculate the pValues from both tests
    pValues <- parApply(myClust, indexes, 1, statisticalTests, 
                        simulations = simulations)
    
    
    
    # Calculate the size/power of our parametric test
    strengthRatio[j, 1] <- errorTypesRatio(pValues[1, ], alpha)
    
    # Calculate the size/power of our non parametric test
    strengthRatio[j, 2] <- errorTypesRatio(pValues[2, ], alpha)
  }
  
  # Terminate the workers
  stopCluster(myClust)
  
  # Add a suffix to the sample sizes
  rownames(strengthRatio) <- paste(rownames(strengthRatio), "samples")
  return(strengthRatio)
}


scenarios.run <- function(sampleSizes, variable.parameter, mean1, mean2,
                          sd1, sd2, scenario.type, distributionType = "Normal",
                          min1 = NULL, min2 = NULL,
                          max1 = NULL, max2 = NULL, seedNumber = 6272) {
  
  # Set the seed for reproducibility
  set.seed(seedNumber)
  
  # Get the length of the sampleSizes vector
  sampleSizes.length <- length(sampleSizes)
  
  # Get the length of the variable parameter
  variable.parameter.length <- length(variable.parameter)
  
  # Initialise parametric summary
  scenario.summary.parametric <- matrix(data = NA, nrow = sampleSizes.length,
                                        ncol = variable.parameter.length,
                                        dimnames = list(sampleSizes,
                                                        variable.parameter))
  
  # Initialise non-parametric summary
  scenario.summary.nonParametric <- matrix(data = NA, nrow = sampleSizes.length,
                                           ncol = variable.parameter.length,
                                           dimnames = list(sampleSizes,
                                                           variable.parameter))
  
  # Initialise the power/size matrix
  strengthRatio <- matrix(data = NA, nrow = sampleSizes.length, ncol = 2)
  
  # Initialise iterator
  j <- 0
  
  for (i in variable.parameter) {
    # Increment the iterator
    j <- j + 1
    
    # Run the scenario depending on the type
    if(scenario.type == "Effect Size") {
      strengthRatio <- scenarios.parallel.single(sampleSizes = sampleSizes,
                                                 mean1 = mean1, mean2 = mean2 + i,
                                                 sd1 = sd1, sd2 = sd2,
                                                 distributionType = distributionType,
                                                 min1 = min1, min2 = min2,
                                                 max1 = max1, max2 = max2)
    }
    else if(scenario.type == "Alpha") {
      strengthRatio <- scenarios.parallel.single(sampleSizes = sampleSizes,
                                                 mean1 = mean1, mean2 = mean2,
                                                 sd1 = sd1, sd2 = sd2,
                                                 alpha = i,
                                                 distributionType = distributionType,
                                                 min1 = min1, min2 = min2,
                                                 max1 = max1, max2 = max2)
    }
    else if(scenario.type == "Variance") {
      strengthRatio <- scenarios.parallel.single(sampleSizes = sampleSizes,
                                                 mean1 = mean1, mean2 = mean2,
                                                 sd1 = sd1, sd2 = sd2 + i,
                                                 distributionType = distributionType,
                                                 min1 = min1, min2 = min2,
                                                 max1 = max1, max2 = max2)
    }
    else {
      stop("Parameter Error")
    }
    
    # Add the result to the summary variables
    scenario.summary.parametric[, j] <- strengthRatio[, 1]
    scenario.summary.nonParametric[, j] <- strengthRatio[, 2]
  }
  
  # Add row names
  rownames(scenario.summary.parametric) <-
    paste(rownames(scenario.summary.parametric), "samples")
  rownames(scenario.summary.nonParametric) <-
    paste(rownames(scenario.summary.nonParametric), "samples")
  
  # Add column names
  colnames(scenario.summary.parametric) <-
    paste(scenario.type, colnames(scenario.summary.parametric))
  colnames(scenario.summary.nonParametric) <-
    paste(scenario.type, colnames(scenario.summary.nonParametric))
  
  return(list("Parametric" = scenario.summary.parametric,
              "Non Parametric" = scenario.summary.nonParametric))
}

scenarios.plot <- function(data, sampleSizes, variable.parameter,
                           legendTitle = "") {
  # Create a parametric and non parametric result data frame to draw the plot
  dataFrame.param <- data.frame()
  dataFrame.nonParam <- data.frame()
  
  # Initialise iterator
  iterator <- 0
  
  # Create the 2 datasets, one for the parametric results and one for the
  # non parametric
  for(i in variable.parameter) {
    # Increment the iterator
    iterator <- iterator + 1
    
    # Add the data for the parametric values
    dataFrame.param <-
      rbind(dataFrame.param,
            data.frame(Group = "Parametric",
                       VariableParam = i,
                       SampleSize = sampleSizes,
                       Values = data$Parametric[, iterator]))
    
    # Add the data for the non parametric values
    dataFrame.nonParam <-
      rbind(dataFrame.nonParam,
            data.frame(Group = "Non Parametric",
                       VariableParam = i,
                       SampleSize = sampleSizes,
                       Values = data$`Non Parametric`[, iterator]))
  }
  
  # Draw plot for the power/size Values ~ SampleSize for different
  # variable parameters, for the parametric results
  p1 <- ggplot(data = dataFrame.param,
               mapping = aes(x = SampleSize, y = Values,
                             colour = as.factor(VariableParam))) +
    geom_point() +
    geom_line() + 
    ggtitle("Parametric Test Strength") +
    labs(colour = legendTitle) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Draw plot for the power/size values ~ SampleSize for different
  # variable parameters, for the non parametric results
  p2 <- ggplot(data = dataFrame.nonParam,
               mapping = aes(x = SampleSize, y = Values,
                             colour = as.factor(VariableParam))) +
    geom_point() +
    geom_line() +
    ggtitle("Non-Parametric Test Strength") +
    labs(colour = legendTitle) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Display the plots together
  gridExtra::grid.arrange(p1, p2, nrow = 2)
}










