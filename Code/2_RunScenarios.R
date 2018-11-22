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
effectSizes <- c(1, 1.2, 3, 6)

# Run the scenario
scenario2.res <- scenarios.run(sampleSizes, effectSizes,
                               mean1 = life.expect.poor.summary[[1]],
                               mean2 = life.expect.poor.summary[[1]],
                               sd1 = life.expect.poor.summary[[2]],
                               sd2 = life.expect.poor.summary[[2]],
                               scenario.type = "Effect Size",
                               seedNumber = 6272)

# Display results
scenario2.res

# Plot the result
scenarios.plot(scenario2.res, sampleSizes, effectSizes,
               "Effect Size")


# Scenario 3: -------------------------------------------------------------
# Increase the alpha

# Create the parameters
sampleSizes <- c(10, 100, 1000)
alphaValues <- c(0.05, 0.06, 0.08, 0.1)

# Run Scenario 3
scenario3.res <- scenarios.run(sampleSizes, alphaValues,
                               mean1 = life.expect.poor.summary[[1]],
                               mean2 = life.expect.poor.summary[[1]],
                               sd1 = life.expect.poor.summary[[2]],
                               sd2 = life.expect.poor.summary[[2]],
                               scenario.type = "Alpha", seedNumber = 6272)
# Display Results
scenario3.res

# Plot Results
scenarios.plot(scenario3.res, sampleSizes, alphaValues, "Alpha")

# Scenario 4: -------------------------------------------------------------
# Change the distribution

# Create the parameters
sampleSizes <- c(10, 100, 1000)
effectSizes <- c(1, 2, 3)

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

# Plot showing the variation of power with the effect size
ggplot() + 
  geom_point(aes(x = effectSizes, y = scenario4.res$Parametric[2, ],
                 colour = "Parametric")) +
  geom_line(aes(x = effectSizes, y = scenario4.res$Parametric[2, ],
                colour = "Parametric")) +
  geom_point(aes(x = effectSizes, y = scenario4.res$`Non Parametric`[2, ],
                 colour = "Non-Parametric")) +
  geom_line(aes(x = effectSizes, y = scenario4.res$`Non Parametric`[2, ],
                colour = "Non-Parametric")) +
  xlab("Effect Size") + 
  ylab("Power") +
  labs(colour = "Type Of Test") +
  ggtitle("Power variation with effect size") +
  theme(plot.title = element_text(hjust = 0.5))

