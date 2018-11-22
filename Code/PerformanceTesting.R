# Performence Testing -----------------------------------------------------

# Profiling the basic scenario function to check for possible improvements
profvis::profvis(scenarios(sampleSizes = c(10, 100, 1000),
                           mean1 = life.expect.poor.summary[[1]],
                           mean2 = life.expect.poor.summary[[1]],
                           sd1 = life.expect.poor.summary[[2]],
                           sd2 = life.expect.poor.summary[[2]]))

# Timing the basic function
system.time(scenarios(sampleSizes = c(10, 100, 200, 1000),
                      mean1 = life.expect.poor.summary[[1]],
                      mean2 = life.expect.poor.summary[[1]],
                      sd1 = life.expect.poor.summary[[2]],
                      sd2 = life.expect.poor.summary[[2]]))

# Timing the parallel function
system.time(scenarios.parallel(sampleSizes = c(10, 100, 200, 1000),
                               mean1 = life.expect.poor.summary[[1]],
                               mean2 = life.expect.poor.summary[[1]],
                               sd1 = life.expect.poor.summary[[2]],
                               sd2 = life.expect.poor.summary[[2]]))

# Timing the optimised parallel function
system.time(scenarios.parallel.single(sampleSizes = c(10, 100, 200, 1000),
                                      mean1 = life.expect.poor.summary[[1]],
                                      mean2 = life.expect.poor.summary[[1]],
                                      sd1 = life.expect.poor.summary[[2]],
                                      sd2 = life.expect.poor.summary[[2]]))
