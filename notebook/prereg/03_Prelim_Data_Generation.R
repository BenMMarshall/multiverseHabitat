library(here)

library(abmAnimalMovement)
library(raster)

seed <- 2022
row = 2000; col = 2000
RandomFields::RFoptions(install="no")
gf1 <- suppressMessages(NLMR::nlm_gaussianfield(ncol = col,
                                                nrow = row,
                                                resolution = 1,
                                                autocorr_range = 40,
                                                mag_var = 5,
                                                nug = 0.2,
                                                mean = 0.5,
                                                user_seed = seed,
                                                rescale = TRUE))

forageQual <- gf1

forageQual[forageQual[] < 0.4] <- 0
# set min 0 max 1, normalise the values between 1 and 0
forageQual[] <- (forageQual[] - min(forageQual[], na.rm = TRUE)) /
  (max(forageQual[], na.rm = TRUE) - min(forageQual[], na.rm = TRUE))

moveQual <- gf1
# areas with high resources are accessible (> 0.6, increased by 0.5), but the
# fastest least resistance routes are actually edge habitat areas (0.6 to 0.3,
# increased by 1). Core areas of low resrouce are also difficult to move
# through.
moveQual[moveQual[] > 0.6] <- moveQual[moveQual[] > 0.6] + 0.5
moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] <-
  moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] + 1
moveQual[] <- (moveQual[] - min(moveQual[], na.rm = TRUE)) /
  (max(moveQual[], na.rm = TRUE) - min(moveQual[], na.rm = TRUE))


shelterQual <- gf1
# shelter sites are best found near the edge of high resource areas, but deeper than the best movement routes
shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] <-
  shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] + 1
shelterQual[] <- (shelterQual[] - min(shelterQual[], na.rm = TRUE)) /
  (max(shelterQual[], na.rm = TRUE) - min(shelterQual[], na.rm = TRUE))

landscapeLayersList <- list(
  "shelter" = matrix(data = raster::getValues(shelterQual),
                     nrow = row,
                     ncol = col),
  "forage" = matrix(data = raster::getValues(forageQual),
                    nrow = row,
                    ncol = col),
  "movement" = matrix(data = raster::getValues(moveQual),
                      nrow = row,
                      ncol = col))

sampledShelters <- sampleRandom(raster(landscapeLayersList$shelter), 2,
                                ext = extent(0.45, 0.65, 0.45, 0.65),
                                rowcol = TRUE)

BADGER_shelterLocs <- data.frame(
  "x" = sampledShelters[,2],
  "y" = sampledShelters[,1])

BADGER_shelterSize <- 8

BADGER_k_step <- c(0.3*60, 1.25*60, 0.25*60)
BADGER_s_step <- c(0.8, 0.25, 0.5)
BADGER_mu_angle <- c(0, 0, 0)
BADGER_k_angle <- c(0.6, 0.99, 0.6)

BADGER_destinationRange <- c(3, 120)
BADGER_destinationDirection <- c(0, 0.01)
BADGER_destinationTransformation <- 2
BADGER_destinationModifier <- 2

BADGER_rescale <- 5

BADGER_avoidLocs <- data.frame(
  "x" = c(1205, 1500, 1165),
  "y" = c(980, 1090, 1250))

BADGER_avoidTransformation <- 2
BADGER_avoidModifier <- 4

BADGER_rest_Cycle <- c(0.12, 0, 24, 24)

# additional cycle
c0 <- c(0.075, 0, 24* (365/2), 24* 365) # seasonal

BADGER_additional_Cycles <- rbind(c0)

b0 <- c(0.97, 0.01, 0.001) # rest
b1 <- c(0.0002, 0.95, 0.0008) # explore/move
b2 <- c(0.001, 0.00001, 0.99) # forage

Default_behaveMatrix <- rbind(b0, b1, b2)

colnames(Default_behaveMatrix) <- c("b0", "b1", "b2")
BADGER_behaveMatrix <- Default_behaveMatrix

startLocation <- sample(900:1100, 2, replace = TRUE)

simSteps <- 24*60 *120

simRes <- abm_simulate(
  start = startLocation,
  timesteps = simSteps,
  des_options = 10,
  options = 12,
  shelterLocations = BADGER_shelterLocs,
  shelterSize = BADGER_shelterSize,
  avoidPoints = BADGER_avoidLocs,
  destinationRange = BADGER_destinationRange,
  destinationDirection = BADGER_destinationDirection,
  destinationTransformation = BADGER_destinationTransformation,
  destinationModifier = BADGER_destinationModifier,
  avoidTransformation = BADGER_avoidTransformation,
  avoidModifier = BADGER_avoidModifier,
  k_step = BADGER_k_step,
  s_step = BADGER_s_step,
  mu_angle = BADGER_mu_angle,
  k_angle = BADGER_k_angle,
  rescale_step2cell = BADGER_rescale,
  behave_Tmat = BADGER_behaveMatrix,
  rest_Cycle = BADGER_rest_Cycle,
  additional_Cycles = BADGER_additional_Cycles,
  shelteringMatrix = landscapeLayersList$shelter,
  foragingMatrix = landscapeLayersList$forage,
  movementMatrix = landscapeLayersList$movement)

write.csv(simRes$locations,
          file = here("notebook", "prereg", "prelimData.csv"),
          row.names = FALSE)
