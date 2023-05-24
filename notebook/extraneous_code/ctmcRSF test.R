
testLand <- multiverseHabitat::simulate_landscape(species = "VULTURE", seed = 2022)

testData <- multiverseHabitat::simulate_individual(
  individualNum = 2,
  species = "VULTURE",
  simSteps = 24*60 *365,
  desOptions = 10,
  options = 12,
  landscapeList = testLand,
  seed = 2022)

sampDuraData <- multiverseHabitat::subset_duration(movementData = testData$locations,
                                                   daysDuration = 15)
sampDuraFreqData <- multiverseHabitat::subset_frequency(movementData = sampDuraData,
                                                        freqPreset = 0.5)

movementData <- sampDuraFreqData

library(ctmm)

plot(movementData$x,
     movementData$y)
spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
sp::plot(spPoints)
spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
sp::plot(spLL)
movementData$lon <- spLL@coords[,1]
movementData$lat <- spLL@coords[,2]
plot(movementData$lon,
     movementData$lat)


area_OUT <- vector("list", 2)

teleObj <- ctmm::as.telemetry(movementData,
                              timeformat = "%Y-%m-%d %H:%M:%S",
                              timezone="UTC",
                              projection = sp::CRS(SRS_string = "EPSG:32601"))

# can do the slower one for the real deal
# varioDataVar <- variogram(teleObj, fast = FALSE, CI = "Gauss")
varioDataVar <- ctmm::variogram(teleObj, fast = TRUE)
guess <- ctmm::ctmm.guess(teleObj, interactive = FALSE)
# need to specify more cores???
fits <- ctmm::ctmm.select(teleObj, guess, verbose = TRUE, cores = 1, method = "pHREML")

akdeRes <- ctmm::akde(teleObj, fits[[1]],
                      weights = TRUE)

# SVF <- variogram(pepper)
# GUESS <- ctmm.guess(pepper, variogram=SVF, interactive=FALSE)
# FIT <- ctmm.select(pepper, GUESS, trace=2)
# ud <- akde(pepper, FIT, weights=TRUE)

library(raster)

# Create a habitat raster
# r1 <- raster(nrows = 100, ncols = 100,
#              xmn = 31, xmx = 32.5, ymn = -25, ymx = -23.5,
#              vals = c(rep(seq(1, 100,1),100)))
# projection(r1) <- "+proj=longlat +datum=WGS84 +no_defs"

# Plot raster to see where it is in relation to the buffalo
# raster::plot(r1)
# points(pepper$latitude~pepper$longitude)
raster::plot(testLand$classRaster)
points(teleObj$y~teleObj$x)

rBase <- testLand$classRaster
rAll <- raster::projectRaster(from = rBase,
                              to = projectExtent(rBase, crs = sp::CRS(SRS_string = "EPSG:4326")))

raster::plot(rAll)
# points(movementData$lat~movementData$lon)
points(teleObj$latitude~teleObj$longitude)

# Weighted resource selection function with one habitat covariate

rAll[] <- as.factor(paste0("c", round(rAll[], digits = 0)))

raster::plot(rAll)
points(teleObj$latitude~teleObj$longitude)

wRSF <- ctmm:::rsf.fit(teleObj,
                       UD = akdeRes,
                       R = list(c = rAll),
                       # R = list(
                       #   c0 = r0,
                       #   c1 = r1,
                       #   c2 = r2),
                       error = 0.01,
                       reference = 1,
                       max.mem = "4 GB")

# wRSF <- ctmm:::rsf.fit(pepper, UD=ud, R=list(habitat1=r1), error=0.1)
summary(wRSF)
summary(wRSF)$CI[1,]
