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

  movementData$t <- as.POSIXct(movementData$datetime)
  movementTrack <- make_track(tbl = movementData, .x = x, .y = y, .t = t, crs = 32601)
  movementSteps <- steps(movementTrack)

  set.seed(2022)
  modelData <- random_steps(movementSteps,
                            n_control = availableSteps,
                            sl_distr = fit_distr(movementSteps$sl_, "gamma"),
                            ta_distr = fit_distr(movementSteps$ta_, "vonmises"))

  modelData <- amt::extract_covariates(modelData,
                                       landscape,
                                       where = covExtract)

  modelData$values <- factor(modelData$layer)

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

  return(ssfOUT)
}
