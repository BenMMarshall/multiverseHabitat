testLand <- multiverseHabitat::simulate_landscape(species = "VULTURE", seed = 2022)

testData <- multiverseHabitat::simulate_individual(species = "VULTURE",
                                                  simSteps = 24*60 *365,
                                                  desOptions = 10,
                                                  options = 12,
                                                  landscapeList = testLand,
                                                  seed = 2022)

plot(testData$locations$x, testData$locations$y)
