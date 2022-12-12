#' Run RSF based analyses
#'
#' @name method_indi_rsf
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param availableArea
#' @param availablePoints
#' @param weighting
#' @return a
#'
#' @export
method_indi_rsf <- function(
    # first two can be for individuals, as they will be provided by previous nodes
  movementData,
  landscape,
  # below can all be programmed as single values as the
  # targets workflow will be used to feed multiple values
  # in
  availableArea,
  availablePoints,
  weighting){

  # generate points based on the availableArea and the number of points
  ### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
  availPoints <- sp::spsample(availableArea[[2]], n = availablePoints, type = "random")

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

  availValues_DF <- as.data.frame(availPoints@coords)
  availValues_DF$values <- as.factor(availValues)
  # case_ == false cos not used
  availValues_DF$case_ <- FALSE
  # assign the weighting
  availValues_DF$weights <- weighting

  usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                               sp::CRS(SRS_string = "EPSG:32601")))
  movementData$values <- as.factor(usedValues)
  modelData <- movementData[,c("x", "y", "values")]
  # used gets case_ == TRUE, and weights == 1
  modelData$case_ <- TRUE
  modelData$weights <- 1

  modelData <- rbind(modelData, availValues_DF)

  # fit the model using base R glm()
  rsfOUT <- glm(case_ ~ values,
                family = binomial(),
                data = modelData,
                weights = weights)

  return(rsfOUT)

}
