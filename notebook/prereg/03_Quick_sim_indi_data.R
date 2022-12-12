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

targets::tar_load("landscape_badger")
targets::tar_load("sampDuraFreqData_0.5_15_3_badger")
targets::tar_load("area_dBBMM_90_0.5_15_3_badger")

movementData <- sampDuraFreqData_0.5_15_3_badger
availableArea <- area_dBBMM_90_0.5_15_3_badger
landscape <- landscape_badger
availablePoints <- 10

plot(area_dBBMM_90_0.5_15_3_badger[[1]])

multiverseHabitat::method_indi_wides(
  movementData,
  landscape_badger,
  availableArea,
  availablePoints
)
# wides ---------------------------------------------------------------

availPoints <- sp::spsample(x = availableArea[[1]],
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

allClasses <- c("c0", "c1", "c2")
# convert to dataframe for easier use in WIDES
availValues_DF <- data.frame(rbind(table(availValues)))

names(availValues_DF) <- sub("X", "c", names(availValues_DF))
# need to run a check in case not all classes appear in the avail or used
missingClass <- allClasses[!allClasses %in% names(availValues_DF)]
if(length(missingClass) >= 1){
  toAdd <- data.frame(0)
  names(toAdd) <- missingClass
  availValues_DF <- cbind(availValues_DF, toAdd)
}
availValues_DF <- availValues_DF[,allClasses]

usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                             sp::CRS(SRS_string = "EPSG:32601")))
usedValues_DF <- data.frame(rbind(table(usedValues)))
names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))

missingClass <- allClasses[!allClasses %in% names(usedValues_DF)]
if(length(missingClass) >= 1){
  toAdd <- data.frame(0)
  names(toAdd) <- missingClass
  usedValues_DF <- cbind(usedValues_DF, toAdd)
}
usedValues_DF <- usedValues_DF[,allClasses]

# However, if both used and avail have zeroes in the same colunm wides breaks,
# so we check for that here and if true we remove both the check, goes column
# wise for zeroes in both dataframes, combines them, then checks for columns
# that have two instances of zeroes
columnsWithBothZeroes <- apply(rbind(
  apply(availValues_DF, 2, function(x){all(x == 0)}),
  apply(usedValues_DF, 2, function(x){all(x == 0)})), 2, function(x){
    all(x)
  })

if(any(columnsWithBothZeroes)){
  # have to remove instances where both avail and used are zeroes as that causes errors
  availValues_DF <- availValues_DF[,!apply(availValues_DF, 2, function(x){
    all(x == 0)
  })]
  usedValues_DF <- usedValues_DF[,!apply(usedValues_DF, 2, function(x){
    all(x == 0)
  })]
}

# so because the difference is use the available, we can do III design with the
# II set up just with different availabilities. Is in a try() function because
# of the instances where habitats are used, but not available bceause of the
# random sampling of available points
wiOUT <- try(
  adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
)
# wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)

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



modout <- multiverseHabitat::method_indi_ssf(movementData = sampDuraFreqData,
                                   landscape = testLand,
                                   methodForm = "mf.is",
                                   covExtract = "end",
                                   availableSteps = 10)

summary(modout)

sampDuraFreqData$t <- as.POSIXct(sampDuraFreqData$datetime)
movementTrack <- amt::make_track(tbl = sampDuraFreqData, .x = x, .y = y, .t = t, crs = 32601)
movementSteps <- amt::steps(movementTrack)

set.seed(2022)
modelData <- amt::random_steps(movementSteps,
                               n_control = 12,
                               sl_distr = amt::fit_distr(movementSteps$sl_, "gamma"),
                               ta_distr = amt::fit_distr(movementSteps$ta_, "vonmises"))

classRaster <- raster(nrows = 2000, ncols = 2000,
                      xmn = 0, xmx = 2000, ymn = 0, ymx = 2000,
                      crs = CRS(SRS_string = "EPSG:32601"),
                      # need to transpose cos matrix and raster deal with rows and col differently
                      vals = t(testLand$classified))
# and flip to full match the raster with the matrix used in the sims
classRaster <- raster::flip(classRaster)
modelData <- amt::extract_covariates(modelData,
                                     classRaster,
                                     where = "start")

modelData$values <- factor(modelData$layer)

if(methodForm == "mf.is"){
  mFormFull <- case_ ~
    values +
    sl_ + log(sl_) + cos(ta_) +
    amt::strata(step_id_)

} else if(methodForm == "mf.ss"){
  mFormFull <- case_ ~
    values +
    amt::strata(step_id_)

}

ssfOUT <- amt::fit_issf(data = modelData,
                        formula = mFormFull,
                        model = TRUE)



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
