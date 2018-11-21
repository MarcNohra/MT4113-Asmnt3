
# Scenario 1: -------------------------------------------------------------
# Same means, different SDs
# H0: the means are equal should be accepted

# Initialise parameters
sampleSizes <- c(10, 100, 300, 1000)
variances <- c(1, 1.2, 1.4, 2, 4)

# Run the scenario
scenario1.res <- scenarios.run(sampleSizes, variances,
                               life.expect.poor.summary[[1]],
                               life.expect.poor.summary[[1]],
                               life.expect.poor.summary[[2]],
                               life.expect.poor.summary[[2]],
                               "Variance")

# Display results
scenario1.res

# Plot the result
scenarios.plot(scenario1.res, sampleSizes, variances)

# Scenario 2: -------------------------------------------------------------
# Different means same SDs

# Initialise parameters
effectSizes <- c(1, 1.2, 3, 5)
sampleSizes <- c(10, 100, 300, 1000)

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

par(mfrow = c(1, 2))
hist(rnorm(1e3, life.expect.poor.summary[[1]], life.expect.poor.summary[[2]]))
hist(rtruncnorm(1e3, life.expect.poor.summary[[3]],
                life.expect.poor.summary[[4]], life.expect.poor.summary[[1]],
                life.expect.poor.summary[[2]]))
par(mfrow = c(1, 1))


