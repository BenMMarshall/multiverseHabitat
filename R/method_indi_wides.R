#' Run wides based analyses
#'
#' @name method_indi_wides
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param availableArea
#' @param availablePoints
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
  availableArea,
  availablePoints,
  # ... is needed so this function can work alongside RSF without throwing
  # unused argument error for the inclusion of weighting in RSF function
  ...){

  if(!require(sp)){
    stop("sp not installed")
  }

  # generate points based on the availableArea and the number of points
  ### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
  availPoints <- sp::spsample(x = availableArea,
                              n = availablePoints,
                              type = "random")

  classRaster <- raster::raster(nrows = nrow(landscape$classified),
                        ncols = ncol(landscape$classified),
                        xmn = 0, xmx = nrow(landscape$classified),
                        ymn = 0, ymx = ncol(landscape$classified),
                        crs = CRS(SRS_string = "EPSG:32601"),
                        # need to transpose cos matrix and raster deal with rows and col differently
                        vals = t(landscape$classified))
  # and flip to full match the raster with the matrix used in the sims
  classRaster <- raster::flip(classRaster)

  # extract the habitat types each point is located within
  availValues <- raster::extract(classRaster, availPoints)

  availValues_DF <- data.frame(rbind(table(availValues)))
  names(availValues_DF) <- sub("X", "c", names(availValues_DF))

  usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                               sp::CRS(SRS_string = "EPSG:32601")))
  usedValues_DF <- data.frame(rbind(table(usedValues)))
  names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))

  aClass <- names(usedValues_DF)
  uClass <- names(usedValues_DF)

  if(length(aClass) > length(uClass)){

    toAdd <- data.frame(0)
    names(toAdd) <- aClass[!aClass %in% uClass]
    usedValues_DF <- cbind(usedValues_DF, toAdd)

  } else if(length(uClass) > length(aClass)){

    toAdd <- data.frame(0)
    names(toAdd) <- uClass[!uClass %in% aClass]
    availValues_DF <- cbind(availValues_DF, toAdd)

  }

  # so because the difference is use the available, we can do III design with the
  # II set up just with different availabilities. Is in a try() function because
  # of the instances where habitats are used, but not available bceause of the
  # random sampling of available points
  wiOUT <- try(
    adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
  )
  # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)

  return(wiOUT)

}
