testLand <- multiverseHabitat::simulate_landscape(species = "VULTURE", seed = 2022)

testData <- multiverseHabitat::simulate_individual(
  individualNum = 2,
  species = "VULTURE",
  simSteps = 24*60 *365,
  desOptions = 10,
  options = 12,
  landscapeList = testLand,
  seed = 2022)

# plot(testData$locations$x, testData$locations$y)

# As our most infrequent tracking is 168 hours (1 week), we will set the
# window to the number of data points collected over 168 hours, and a margin
# of 48 hours.

# windowSize <- nrow(testData$locations[testData$locations$timestep <= 168*60,])
# if(windowSize %% 2 == 0){
#   windowSize <- windowSize - 1
# }
#
# marginSize <- nrow(testData$locations[testData$locations$timestep <= 48*60,])
# if(marginSize %% 2 == 0){
#   marginSize <- marginSize - 1
# }


sampDuraData <- multiverseHabitat::subset_duration(movementData = testData$locations,
                                                   daysDuration = 15)
sampDuraFreqData <- multiverseHabitat::subset_frequency(movementData = sampDuraData,
                                                        freqPreset = 0.5)

# akdeOUT <- multiverseHabitat::build_available_area(movementData = sampDuraFreqData,
#                                         "AKDE", 95)
# methOUT_method_indi_wides_10_1_90_dBBMM_0.5_7_4_badger
targets::tar_load("landscape_badger")
targets::tar_load("sampDuraFreqData_0.5_15_1_badger")
targets::tar_load("polygon_90_MCP_0.5_15_1_badger")

# methOUT_method_indi_wides_10_1_95_MCP_0.5_7_1_badger
# Last error: 'names' attribute [2] must be the same length as the vector [1]
landscape <- landscape_badger
movementData <- sampDuraFreqData_0.5_15_1_badger
availableArea <- polygon_90_MCP_0.5_15_1_badger
availablePointsPer <- 1

sp::plot(availableArea)

# wides ---------------------------------------------------------------
multiverseHabitat::method_indi_wides(
  movementData,
  landscape_badger,
  availableArea,
  availablePointsPer
)

if(!require(sp)){
  stop("sp not installed")
}

# generate points based on the availableArea and the number of points
### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
suppressWarnings({
  availPoints <- sp::spsample(x = availableArea,
                              n = nrow(movementData) * availablePointsPer,
                              type = "random")
})

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

suppressWarnings({
  usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                               sp::CRS(SRS_string = "EPSG:32601")))
})
usedValues_DF <- data.frame(rbind(table(usedValues)))
names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))

aClass <- names(availValues_DF)
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

usedValues_DF <- usedValues_DF[,sort(names(usedValues_DF))]
availValues_DF <- availValues_DF[,sort(names(availValues_DF))]
# so because the difference is use the available, we can do III design with the
# II set up just with different availabilities. Is in a try() function because
# of the instances where habitats are used, but not available bceause of the
# random sampling of available points
wiOUT <- try(
  adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
)
if(class(wiOUT)[1] == "try-error"){
  wiOUT <- wiOUT[1]
}
# wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
wiOUT

# rsf ---------------------------------------------------------------------

availPoints <- sp::spsample(availableArea[[1]], n = 10, type = "random")
plot(availPoints)
classRaster <- raster(nrows = nrow(landscape$classified),
                      ncols = ncol(landscape$classified),
                      xmn = 0, xmx = nrow(landscape$classified),
                      ymn = 0, ymx = ncol(landscape$classified),
                      crs = CRS(SRS_string = "EPSG:32601"),
                      # need to transpose cos matrix and raster deal with rows and col differently
                      vals = t(landscape$classified))
# and flip to full match the raster with the matrix used in the sims
classRaster <- raster::flip(classRaster)

availValues <- raster::extract(classRaster, availPoints)

availValues_DF <- as.data.frame(availPoints@coords)
availValues_DF$values <- as.factor(availValues)
availValues_DF$case_ <- FALSE
availValues_DF$weights <- 1

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

method_indi_rsf(sampDuraFreqData_0.5_7_2_badger,
                landscape_badger,
                area_MCP_90_0.5_7_2_badger,
                availablePoints = 10,
                weighting = 1)



# ssf testing -------------------------------------------------------------

targets::tar_load("landscape_badger")
targets::tar_load("sampDuraFreqData_1_15_4_badger")
landscape <- landscape_badger
movementData <- sampDuraFreqData_1_15_4_badger
availableSteps <- 10
covExtract <- "end"

modout <- multiverseHabitat::method_indi_ssf(movementData = movementData,
                                             landscape = landscape_badger,
                                             methodForm = "mf.ss",
                                             covExtract = "end",
                                             availableSteps = 10)

targets::tar_load("ssfOUT_mf.is_end_10_0.5_7_2_badger")
summary(ssfOUT_mf.is_end_10_0.5_7_2_badger)$coef
summary(modout)$coef

movementData$t <- as.POSIXct(movementData$datetime)
movementTrack <- amt::make_track(tbl = movementData, .x = x, .y = y, .t = t, crs = 32601)
movementSteps <- amt::steps(movementTrack)

set.seed(2022)
modelData <- amt::random_steps(movementSteps,
                               n_control = availableSteps,
                               sl_distr = amt::fit_distr(movementSteps$sl_, "gamma"),
                               ta_distr = amt::fit_distr(movementSteps$ta_, "vonmises"))

classRaster <- raster(nrows = nrow(landscape$classified),
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
ssfOUT



# r <- raster(x = testLand$classified,
#   xmn = 1, xmx = 2000,
#   ymn = 1, ymx = 2000,
# crs = CRS(SRS_string = "EPSG:32601"))
#
# r <-raster(
#   dat1$z,
#   xmn=range(dat1$x)[1], xmx=range(dat1$x)[2],
#   ymn=range(dat1$y)[1], ymx=range(dat1$y)[2],
#   crs=CRS("+proj=utm +zone=11 +datum=NAD83")
# )
# plot(r)

modelData$layer[1:20]
modelData$x1_[1]
modelData$y1_[1]

extract(
  classRaster, data.frame(
    modelData$x1_[1:20],
    modelData$y1_[1:20])
)


plot(classRaster)
points(400,1750)

testLand$forage[1600,1750]
extract(
  classRaster, data.frame(x = 1600, y = 1750)
)

# for(rowcell in 1:20){
#   print(testLand$classified[modelData$x1_[rowcell], modelData$y1_[rowcell]])
# }
library(reshape2)
library(ggplot2)
layerDF <- melt(testLand$forage, c("col", "row"))

ggplot(layerDF) +
  geom_raster(aes(x = col, y = row, fill = value)) +
  annotate("point", x = 400, y = 1750)





classRaster <- raster(nrows = 2000, ncols = 2000,
                      xmn = 0, xmx = 2000, ymn = 0, ymx = 2000,
                      crs = CRS(SRS_string = "EPSG:32601"),
                      # need to transpose cos matrix and raster deal with rows and col differently
                      vals = t(testLand$classified))

r <- raster(testLand$classified,
            crs = CRS(SRS_string = "EPSG:32601"))
plot(r)
