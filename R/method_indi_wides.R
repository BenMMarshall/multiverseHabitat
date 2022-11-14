#' Run wides based analyses
#'
#' @name method_indi_wides
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param availableArea
#' @param availabelContour
#' @param availabelPoints
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
  availabelPoints){

  # generate points based on the availableArea and the number of points
  ### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
  availPoints <- spsample(availableArea, n = availabelPoints, type = "random")
  # extract the habitat types each point is located within
  availValues <- raster::extract(landscape, availPoints)
  # convert to dataframe for easier use in WIDES
  availValues_DF <- data.frame(rbind(table(availValues)))
  names(availValues_DF) <- c("c0", "c1", "c2")

  usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                               sp::CRS(SRS_string = "EPSG:32601")))
  usedValues_DF <- data.frame(rbind(table(usedValues)))
  names(usedValues_DF) <- c("c0", "c1", "c2")

  # so because the difference is use the available, we can do III design with
  # the II set up just with different availabilities
  wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)

  return(wiOUT)

}
