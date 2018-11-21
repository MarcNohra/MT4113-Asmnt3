# Performence Testing -----------------------------------------------------

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










