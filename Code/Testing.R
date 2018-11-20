if(!require(gridExtra)) install.packages("gridExtra")


# Scenarios ---------------------------------------------------------------


# Scenario 1:
# Same means, same SDs
# H0: the means are equal should be accepted
scenarios.parallel.single(sampleSizes = c(10, 100, 1000),
                          mean1 = life.expect.poor.summary[[1]],
                          mean2 = life.expect.poor.summary[[1]],
                          sd1 = life.expect.poor.summary[[2]],
                          sd2 = life.expect.poor.summary[[2]])


# Scenario 2:
# Different means same SDs
# H0: the means are equal should be accepted
scenario.2 <- function(effectSize, sampleSizes) {
  # Get the length of the sampleSizes vector
  sampleSizes.length <- length(sampleSizes)
  
  # Get the length of the effectSize
  effectSize.length <- length(effectSize)
  
  # Initialise parametric summary
  scenario.summary.parametric <- matrix(data = NA, nrow = sampleSizes.length,
                             ncol = effectSize.length,
                             dimnames = list(sampleSizes, effectSize))
  
  # Initialise non-parametric summary
  scenario.summary.nonParametric <- matrix(data = NA, nrow = sampleSizes.length,
                                        ncol = effectSize.length,
                                        dimnames = list(sampleSizes, effectSize))
  
  # Initialise the power matrix
  strengthRatio <- matrix(data = NA, nrow = sampleSizes.length, ncol = 2)
  
  # Initialise iterator
  j <- 0
  
  for(i in effectSize){
    j <- j + 1
    
    # Run the scenario
    strengthRatio <- scenarios.parallel.single(sampleSizes = sampleSizes,
                              mean1 = life.expect.poor.summary[[1]],
                              mean2 = (life.expect.poor.summary[[1]] + i),
                              sd1 = life.expect.poor.summary[[2]],
                              sd2 = life.expect.poor.summary[[2]])
    
    # Add the result to the summary
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
    paste("Effect Size", colnames(scenario.summary.parametric))
  colnames(scenario.summary.nonParametric) <-
    paste("Effect Size", colnames(scenario.summary.nonParametric))
  
  return(list("Parametric" = scenario.summary.parametric,
              "Non Parametric" = scenario.summary.nonParametric))
}
scenario2.plot <- function(scenario2.res, effectSizes, sampleSizes){
  # Create a parametric result data frame to draw a plot
  scenario.2.dataFrame.param <- data.frame()
  scenario.2.dataFrame.nonParam <- data.frame()
  
  # Initialise iterator
  scenario2.iterator <- 0
  
  # Create the 2 datasets, one for the parametric results and one for the
  # non parametric
  for(i in effectSizes){
    # Increase the iterator count
    scenario2.iterator <- scenario2.iterator + 1
    
    # Add the data for the parametric values
    scenario.2.dataFrame.param <-
      rbind(scenario.2.dataFrame.param,
            data.frame(Group = "Parametric",
                       EffectSize = i,
                       SampleSize = sampleSizes,
                       Values = scenario2.res$Parametric[, scenario2.iterator]))
    
    # Add the data for the non parametric values
    scenario.2.dataFrame.nonParam <-
      rbind(scenario.2.dataFrame.nonParam,
            data.frame(Group = "Non Parametric",
                       EffectSize = i,
                       SampleSize = sampleSizes,
                       Values = scenario2.res$`Non Parametric`[, scenario2.iterator]))
  }
  
  # Draw plot for the power Values ~ SampleSize for different Effect Sizes, 
  # for the parametric results
  p1 <- ggplot(data = scenario.2.dataFrame.param,
               mapping = aes(x = SampleSize, y = Values, colour = factor(EffectSize))) +
    geom_point() +
    geom_line()
  
  # Draw plot for the power values ~ SampleSize for different Effect Sizes,
  # for the non parametric results
  p2 <- ggplot(data = scenario.2.dataFrame.nonParam,
               mapping = aes(x = SampleSize, y = Values, colour = factor(EffectSize))) +
    geom_point() +
    geom_line()
  
  # Display the plots together
  gridExtra::grid.arrange(p1, p2, nrow = 2)
}

effectSizes <- c(1, 1.2, 3, 5)
sampleSizes <- c(10, 100, 300, 1000)
scenario2.res <- scenario.2(effectSizes, sampleSizes)
scenario2.res
# Draw plots
scenario2.plot(scenario2.res, effectSizes, sampleSizes)


# Scenario 3:



# Performence Testing -----------------------------------------------------

# Scenario 1:
# Same means, same SDs
# H0: the means are equal should be accepted
scenarios(sampleSizes = c(10, 100, 200, 300),
          mean1 = life.expect.poor.summary[[1]],
          mean2 = life.expect.poor.summary[[1]],
          sd1 = life.expect.poor.summary[[2]],
          sd2 = life.expect.poor.summary[[2]])

profvis::profvis(scenarios(sampleSizes = c(10, 100, 200, 1000),
                           mean1 = life.expect.poor.summary[[1]],
                           mean2 = life.expect.poor.summary[[1]],
                           sd1 = life.expect.poor.summary[[2]],
                           sd2 = life.expect.poor.summary[[2]]))

# 7s
system.time(scenarios(sampleSizes = c(10, 100, 200, 1000),
                      mean1 = life.expect.poor.summary[[1]],
                      mean2 = life.expect.poor.summary[[1]],
                      sd1 = life.expect.poor.summary[[2]],
                      sd2 = life.expect.poor.summary[[2]]))

# 4.4s / 19.2
system.time(scenarios.parallel(sampleSizes = c(10, 100, 200, 1000),
                      mean1 = life.expect.poor.summary[[1]],
                      mean2 = life.expect.poor.summary[[1]],
                      sd1 = life.expect.poor.summary[[2]],
                      sd2 = life.expect.poor.summary[[2]]))

# 3.8 / 14.6
system.time(scenarios.parallel.single(sampleSizes = c(10, 100, 200, 1000),
                               mean1 = life.expect.poor.summary[[1]],
                               mean2 = life.expect.poor.summary[[1]],
                               sd1 = life.expect.poor.summary[[2]],
                               sd2 = life.expect.poor.summary[[2]]))

# Same means and SDs
#   n     10        100       1000
# param:  0.05     0.051      0.045
# non pa: 0.055    0.046      0.044

# Scenario 2:
# Different means and SDs
# H0: the means are equal should be accepted
scenarios.parallel.single(sampleSizes = c(10, 100, 1000),
          mean1 = life.expect.poor.summary[[1]],
          mean2 = (life.expect.poor.summary[[1]] + 5),
          sd1 = life.expect.poor.summary[[2]],
          sd2 = life.expect.poor.summary[[2]] + 1)

# Diff means and SDs
#   n      10       100       1000
# param:  0.165     0.936     1
# non pa: 0.145     0.923     1










