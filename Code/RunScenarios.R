# Scenario 0:
# Beta Distribution

xt <- subGapMinder$life_expectancy / ( max( subGapMinder$life_expectancy ) + .0001 )
fit.beta <- fitdistr(xt, "beta", start = list(shape1 = 2, shape2 = 5))
scale <- ( max( subGapMinder$life_expectancy ) + .0001 )

simulation <- monteCarlo(6272, 1e3, 1000, 1, fit.beta$estimate[[1]], fit.beta$estimate[[2]], scale)
param.pValues <- parametricTesting(simulation)
nonParam.pValues <- nonParametricTesting(simulation)
paramErr <- errorTypesRatio(param.pValues, 0.05)
nparamErr <- errorTypesRatio(nonParam.pValues, 0.05)
paramErr
nparamErr
hist(simulation)

# Comparing
# Run the scenario
sampleSizes <- c(10, 100, 1000)
variances <- c(0)
scenario1.res <- scenarios.run(sampleSizes = sampleSizes,
                               variable.parameter = variances,
                               mean1 = 3, mean2 = 3, sd1 = 1, sd2 = 1,
                               scenario.type = "Variance", seedNumber = 6272)
scenario1.res

# Scenario 1: -------------------------------------------------------------
# Same means, different SDs
# H0: the means are equal should be accepted

# Initialise parameters
sampleSizes <- c(10, 100, 300, 1000)
variances <- c(1, 1.2, 2, 4)

# Run the scenario
scenario1.res <- scenarios.run(sampleSizes, variances,
                               mean1 = life.expect.poor.summary[[1]],
                               mean2 = life.expect.poor.summary[[1]],
                               sd1 = life.expect.poor.summary[[2]],
                               sd2 = life.expect.poor.summary[[2]],
                               scenario.type = "Variance", seedNumber = 6272)

# Display results
scenario1.res

# Plot the result
scenarios.plot(scenario1.res, sampleSizes, variances,
               "Standard Deviation\nIncreased By")

# Scenario 2: -------------------------------------------------------------
# Different means same SDs

# Initialise parameters
sampleSizes <- c(10, 100, 1000)
effectSizes <- c(1, 1.2, 3, 5, 10)

# Run the scenario
scenario2.res <- scenarios.run(sampleSizes, effectSizes,
                                    life.expect.poor.summary[[1]],
                                    life.expect.poor.summary[[1]],
                                    life.expect.poor.summary[[2]],
                                    life.expect.poor.summary[[2]],
                                    "Effect Size")

# Display results
scenario2.res

# Plot the result
scenarios.plot(scenario2.res, sampleSizes, effectSizes)


# Scenario 3: -------------------------------------------------------------
# Increase the alpha

# Create the parameters
alphaValues <- c(0.05, 0.06, 0.08, 0.1)
sampleSizes <- c(10, 100, 300, 1000)

# Run Scenario 3
scenario3.res <- scenarios.run(sampleSizes, alphaValues,
                               life.expect.poor.summary[[1]],
                               life.expect.poor.summary[[1]],
                               life.expect.poor.summary[[2]],
                               life.expect.poor.summary[[2]],
                               "Alpha")
# Display Results
scenario3.res

# Plot Results
scenarios.plot(scenario3.res, sampleSizes, alphaValues)

# Scenario 4: -------------------------------------------------------------
# Change the distribution

# Create the parameters
sampleSizes <- c(10, 100, 1000)
effectSizes <- c(0.3, 1, 5, 10)

# Run the scenario
scenario4.res <- scenarios.run(sampleSizes, effectSizes,
                               mean1 = life.expect.rich.summary[[1]],
                               mean2 = life.expect.rich.summary[[1]],
                               sd1 = life.expect.rich.summary[[2]],
                               sd2 = life.expect.rich.summary[[2]],
                               "Effect Size", "Truncate",
                               min1 = life.expect.rich.summary[[3]],
                               min2 = life.expect.rich.summary[[3]],
                               max1 = life.expect.rich.summary[[4]],
                               max2 = life.expect.rich.summary[[4]],
                               seedNumber = 6272)

# Display results
scenario4.res

# Plot the result
scenarios.plot(scenario4.res, sampleSizes, effectSizes)
x <- 2

ggplot() + 
  geom_point(aes(x = effectSizes, y = scenario4.res$Parametric[x, ], colour = "red")) +
  geom_line(aes(x = effectSizes, y = scenario4.res$Parametric[x, ], colour = "red")) +
  geom_point(aes(x = effectSizes, y = scenario4.res$`Non Parametric`[x, ], colour = "blue")) +
  geom_line(aes(x = effectSizes, y = scenario4.res$`Non Parametric`[x, ], colour = "blue"))
  
