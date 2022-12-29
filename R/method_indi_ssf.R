#' Run SSD based analyses
#'
#' @name method_indi_ssf
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param methodForm "mf.is", "mf.ss"
#' @param covExtract "start", "end", "both"
#' @param availableSteps
#' @param weighting
#' @return a
#'
#' @export
method_indi_ssf <- function(
    # first two can be for individuals, as they will be provided by previous nodes
  movementData,
  landscape,
  # below can all be programmed as single values as the
  # targets workflow will be used to feed multiple values
  # in
  methodForm,
  covExtract,
  availableSteps){

  if(!require(amt)){
    stop("amt not installed")
  }

  movementData$t <- as.POSIXct(movementData$datetime)
  movementTrack <- amt::make_track(tbl = movementData, .x = x, .y = y, .t = t, crs = 32601)
  movementSteps <- amt::steps(movementTrack)

  set.seed(2022)
  modelData <- amt::random_steps(movementSteps,
                                 n_control = availableSteps,
                                 sl_distr = amt::fit_distr(movementSteps$sl_, "gamma"),
                                 ta_distr = amt::fit_distr(movementSteps$ta_, "vonmises"))

  classRaster <- raster::raster(nrows = nrow(landscape$classified),
                                ncols = ncol(landscape$classified),
                                xmn = 0, xmx = nrow(landscape$classified),
                                ymn = 0, ymx = ncol(landscape$classified),
                                crs = CRS(SRS_string = "EPSG:32601"),
                                # need to transpose cos matrix and raster deal with rows and col differently
                                vals = t(landscape$classified))
  # and flip to full match the raster with the matrix used in the sims
  classRaster <- raster::flip(classRaster)

  modelData <- amt::extract_covariates(modelData,
                                       classRaster,
                                       where = covExtract)

  modelData$values <- paste0("c", modelData$layer)
  modelData$values <- factor(modelData$values)

  if(methodForm == "mf.is"){
    mFormFull <- case_ ~
      values +
      sl_ + log(sl_) + cos(ta_) +
      strata(step_id_)

  } else if(methodForm == "mf.ss"){
    mFormFull <- case_ ~
      values +
      strata(step_id_)

  }

  ssfOUT <- amt::fit_issf(data = modelData,
                          formula = mFormFull,
                          model = TRUE)

  ssfDF <- as.data.frame(summary(ssfOUT)$coef)
  method <- rep("ssf", nrow(ssfDF))
  ssfDF <- cbind(ssfDF, method)

  return(extract_estimate(ssfDF))
}
