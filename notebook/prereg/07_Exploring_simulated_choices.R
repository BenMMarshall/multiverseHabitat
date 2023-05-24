
### CONVERTED TO FUNCTION and in TARGETS

library(dplyr)
library(stringr)
library(amt)
library(adehabitatHS)

targets::tar_load("simResults")

simNames <- names(simResults)
simDirectResultsList <- vector("list", length = length(simNames))
names(simDirectResultsList) <- simNames
for(nameSingle in simNames){

  # nameSingle <- simNames[1]

  simSingle <- simResults[[nameSingle]]

  targets::tar_load(paste0("landscape_", str_extract(nameSingle, "BADGER|VULTURE|KINGCOBRA")))
  landscape <- get(paste0("landscape_", str_extract(nameSingle, "BADGER|VULTURE|KINGCOBRA")))

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


  # rsf ---------------------------------------------------------------------

  # fit the model using base R glm()
  rsfMovementOUT <- glm(case_ ~ values,
                        family = binomial(),
                        data = movementScaleData)

  rsfMovementDF <- as.data.frame(summary(rsfMovementOUT)$coef)
  method <- rep("rsf", nrow(rsfMovementDF))
  rsfMovementDF <- cbind(rsfMovementDF, method)

  rsfDestinationOUT <- glm(case_ ~ values,
                           family = binomial(),
                           data = destinationScaleData)

  rsfDestinationDF <- as.data.frame(summary(rsfDestinationOUT)$coef)
  method <- rep("rsf", nrow(rsfDestinationDF))
  rsfDestinationDF <- cbind(rsfDestinationDF, method)



  # wides -------------------------------------------------------------------

  availValues_DF <- data.frame(rbind(table(movementScaleData[movementScaleData$case_ == 0,]$values)))
  usedValues_DF <- data.frame(rbind(table(movementScaleData[movementScaleData$case_ == 1,]$values)))

  aClass <- names(availValues_DF)
  uClass <- names(usedValues_DF)

  if(length(aClass) > length(uClass)){

    toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(aClass[!aClass %in% uClass])))
    names(toAdd) <- aClass[!aClass %in% uClass]
    usedValues_DF <- cbind(usedValues_DF, toAdd)

  } else if(length(uClass) > length(aClass)){

    toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(uClass[!uClass %in% aClass])))
    names(toAdd) <- uClass[!uClass %in% aClass]
    availValues_DF <- cbind(availValues_DF, toAdd)

  }

  usedValues_DF <- usedValues_DF[,sort(names(usedValues_DF))]
  availValues_DF <- availValues_DF[,sort(names(availValues_DF))]

  wiOUT <- try(
    adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
  )
  if(class(wiOUT)[1] == "try-error"){
    wiOUT <- wiOUT[1]
  }
  # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)


  availDestinationValues_DF <- data.frame(rbind(table(destinationScaleData[destinationScaleData$case_ == 0,]$values)))
  usedDestinationValues_DF <- data.frame(rbind(table(destinationScaleData[destinationScaleData$case_ == 1,]$values)))

  aClass <- names(availDestinationValues_DF)
  uClass <- names(usedDestinationValues_DF)

  if(length(aClass) > length(uClass)){

    toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(aClass[!aClass %in% uClass])))
    names(toAdd) <- aClass[!aClass %in% uClass]
    usedDestinationValues_DF <- cbind(usedDestinationValues_DF, toAdd)

  } else if(length(uClass) > length(aClass)){

    toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(uClass[!uClass %in% aClass])))
    names(toAdd) <- uClass[!uClass %in% aClass]
    availDestinationValues_DF <- cbind(availDestinationValues_DF, toAdd)

  }

  usedDestinationValues_DF <- usedDestinationValues_DF[,sort(names(usedDestinationValues_DF))]
  availDestinationValues_DF <- availDestinationValues_DF[,sort(names(availDestinationValues_DF))]

  wiDestinationOUT <- try(
    adehabitatHS::widesIII(u = usedDestinationValues_DF, a = availDestinationValues_DF)
  )
  if(class(wiDestinationOUT)[1] == "try-error"){
    wiDestinationOUT <- wiDestinationOUT[1]
  }
  # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)




  # issf --------------------------------------------------------------------

  movementScaleData$t <- as.POSIXct(movementScaleData$datetime)
  movementTrack <- amt::make_track(tbl = movementScaleData,
                                   .x = x, .y = y, .t = t,
                                   step_id_ = timestep,
                                   case_ = case_, values = values,
                                   crs = 32601)
  movementSteps <- amt::steps(movementTrack,
                              keep_cols = "start")

  mFormFull <- case_ ~
    values +
    sl_ + log(sl_) + cos(ta_) +
    strata(step_id_)

  ssfMovementOUT <- amt::fit_issf(data = movementSteps,
                                  formula = mFormFull,
                                  model = TRUE)

  ssfMovementDF <- as.data.frame(summary(ssfMovementOUT)$coef)
  method <- rep("ssf", nrow(ssfMovementDF))
  ssfMovementDF <- cbind(ssfMovementDF, method)


  destinationScaleData$t <- as.POSIXct(destinationScaleData$datetime)
  destinationTrack <- amt::make_track(tbl = destinationScaleData,
                                      .x = destinations_x, .y = destinations_y, .t = t,
                                      step_id_ = timestep,
                                      case_ = case_, values = values,
                                      crs = 32601)
  destinationSteps <- amt::steps(destinationTrack,
                                 keep_cols = "start")

  mFormFull <- case_ ~
    values +
    sl_ + log(sl_) + cos(ta_) +
    strata(step_id_)

  ssfdestinationOUT <- amt::fit_issf(data = destinationSteps,
                                     formula = mFormFull,
                                     model = TRUE)

  ssfdestinationDF <- as.data.frame(summary(ssfdestinationOUT)$coef)
  method <- rep("ssf", nrow(ssfdestinationDF))
  ssfdestinationDF <- cbind(ssfdestinationDF, method)

  # compile results ---------------------------------------------------------

  indiResults <- do.call(rbind, list(
    multiverseHabitat::extract_estimate(rsfMovementDF),
    multiverseHabitat::extract_estimate(wiOUT),
    multiverseHabitat::extract_estimate(ssfMovementDF),
    multiverseHabitat::extract_estimate(rsfDestinationDF),
    multiverseHabitat::extract_estimate(wiDestinationOUT),
    multiverseHabitat::extract_estimate(ssfdestinationDF))
  )
  indiResults$scale <- c(rep("movement", 3), rep("destination", 3))
  indiResults$indi <- str_extract(nameSingle, "[:digit:]")
  indiResults$species <- str_extract(nameSingle, "BADGER|VULTURE|KINGCOBRA")

  simDirectResultsList[[nameSingle]] <- indiResults

  print(paste0(indiResults$species[1],
               indiResults$indi[1], " --- Done"))

}
simDirectResults <- do.call(rbind, simDirectResultsList)

simDirectResults
