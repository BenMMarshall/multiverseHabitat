#' Run wides based analyses
#'
#' @name method_indi_wides
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param spSamp c("rd", "st")
#' @param availableArea
#' @param availablePointsPer
#' @return a
#'
#' @export
method_indi_wides <- function(
    # first two can be for individuals, as they will be provided by previous nodes
  movementData,
  landscape,
  # below can all be programmed as single values as the
  # targets workflow will be used to feed multiple values
  # in
  #### designType, # design type not needed cos typeII is more pop-level analysis?
  # could be swapped for random/systematic point selection
  spSamp,
  availableArea,
  availablePointsPer,
  # ... is needed so this function can work alongside RSF without throwing
  # unused argument error for the inclusion of weighting in RSF function
  ...){

  if(!require(sp)){
    stop("sp not installed")
  }

  # generate points based on the availableArea and the number of points
  ### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
  suppressWarnings({
    availPoints <- sp::spsample(availableArea,
                                n = nrow(movementData) * availablePointsPer,
                                type = ifelse(spSamp == "rd", "random", "stratified"))
  })

  # extract the habitat types each point is located within
  availValues <- raster::extract(landscape$classRaster, availPoints)

  availValues_DF <- data.frame(rbind(table(availValues)))
  names(availValues_DF) <- sub("X", "c", names(availValues_DF))

  suppressWarnings({
    usedValues <- raster::extract(landscape$classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                                 sp::CRS(SRS_string = "EPSG:32601")))
  })
  usedValues_DF <- data.frame(rbind(table(usedValues)))
  names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))

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
  # so because the difference is use the available, we can do III design with the
  # II set up just with different availabilities. Is in a try() function because
  # of the instances where habitats are used, but not available bceause of the
  # random sampling of available points

  print("wides")

  wiOUT <- try(
    adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
  )
  if(class(wiOUT)[1] == "try-error"){
    wiOUT <- wiOUT[1]
  }
  # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)

  return(multiverseHabitat::extract_estimate(wiOUT))

}
