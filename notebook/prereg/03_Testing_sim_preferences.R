library(multiverseHabitat)
library(dplyr)
library(amt)

# SIM LANDSCAPE

landscape <- multiverseHabitat::simulate_landscape("KINGCOBRA", 2022)

# extractedMovementValues <- c(0, 0)
# while(!all(extractedMovementValues == 2)){
#   sampledShelters <- raster::sampleRandom(raster::raster(landscape$shelter), 2,
#                                           ext = raster::extent(0.45, 0.65, 0.45, 0.65),
#                                           rowcol = TRUE)
#
#   spMovementPoints <- sp::SpatialPoints(sampledShelters[,c("col", "row")],
#                                         sp::CRS(SRS_string = "EPSG:32601"))
#   extractedMovementValues <- raster::extract(landscape$classRaster, spMovementPoints)
# }

# SIM INDIS

simDataList <- lapply(1:15, function(x){
  multiverseHabitat::simulate_individual(
    individualNum = x,
    species = "KINGCOBRA",
    simSteps = 24*60 *365,
    desOptions = 12,
    options = 15,
    landscape,
    seed = 2022)
})

# simData <- multiverseHabitat::simulate_individual(
#   individualNum = 1,
#   species = "BADGER",
#   simSteps = 24*60 *365,
#   desOptions = 10,
#   options = 12,
#   landscape,
#   seed = 2022)
# simList <- list("BADGER_1" = simData)

names(simDataList) <- paste0("KINGCOBRA", 1:15)

# unlist(stringr::str_extract_all(paste0("BADGER_", 1:15)[15], "[:digit:]+"))
# stringr::str_extract(paste0("BADGER_", 1:15)[15], "\\d+")

# DIRECT ESTIMATE TO VALIDATE PREFERENCE FOR C2

diEst <- direct_estimates(simDataList)

diEst %>%
  filter(scale == "destination")


simSingle <- simDataList[[2]]

# data prep ---------------------------------------------------------------

optionsDF <- simSingle$options
chosenMoveDF <- simSingle$locations[,c("timestep", "chosen", "datetime")]

destinationDF <- simSingle$destinations
chosenDestinationDF <- simSingle$locations[,c("timestep", "destination_chosen", "datetime")]
chosenDestinationDF <- chosenDestinationDF %>%
  filter(timestep %in% unique(destinationDF$timestep))

movementScaleData <- left_join(optionsDF,
                               chosenMoveDF,
                               by = "timestep")
movementScaleData <- movementScaleData %>%
  group_by(timestep) %>%
  mutate(case_ = if_else(row_number() == (chosen+1), 1, 0)) %>%
  ungroup()

destinationScaleData <- left_join(destinationDF,
                                  chosenDestinationDF,
                                  by = "timestep")
destinationScaleData <- destinationScaleData %>%
  group_by(timestep) %>%
  mutate(case_ = if_else(row_number() == (destination_chosen+1), 1, 0)) %>%
  ungroup()


# extract classed values --------------------------------------------------

spMovementPoints <- sp::SpatialPoints(movementScaleData[,c("x", "y")],
                                      sp::CRS(SRS_string = "EPSG:32601"))
extractedMovementValues <- raster::extract(landscape$classRaster, spMovementPoints)
movementScaleData$values <- as.factor(paste0("c", extractedMovementValues))

spDestinationPoints <- sp::SpatialPoints(destinationScaleData[,c("destinations_x", "destinations_y")],
                                         sp::CRS(SRS_string = "EPSG:32601"))
extractedDestinationValues <- raster::extract(landscape$classRaster, spDestinationPoints)
destinationScaleData$values <- as.factor(paste0("c", extractedDestinationValues))

destinationScaleData %>%
  print(n = 200)

destinationScaleData %>%
  filter(destination_behave == 0)
