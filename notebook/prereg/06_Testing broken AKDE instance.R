# areaMethodsOUT_240_168_3_VULTURE
# Last error: missing value where TRUE/FALSE needed

targets::tar_make("areaMethodsOUT_240_168_3_VULTURE")
targets::tar_load("sampDuraFreqData_240_168_3_VULTURE")

targets::tar_load("simData_3_VULTURE")

library(ggplot2)

simData_3_VULTURE$locations %>%
  ggplot() +
  geom_point(aes(x = hour, y = behave))

simData_3_VULTURE$locations %>%
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  geom_point(data = sampDuraFreqData_240_168_3_VULTURE,
             aes(x = x, y = y), colour = "red")

targets::tar_load("areaMethodsOUT_240_168_3_VULTURE")
areaMethodsOUT_240_168_3_VULTURE %>%
  filter(area == "AKDE")


targets::tar_load("sampDuraFreqData_240_168_3_VULTURE")
targets::tar_load("landscape_VULTURE")

movementData <- sampDuraFreqData_240_168_3_VULTURE
landscape <- landscape_VULTURE

spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
plot(spPoints)
spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
plot(spLL)
movementData$lon <- spLL@coords[,1]
movementData$lat <- spLL@coords[,2]
plot(movementData$lon, movementData$lat)

area_OUT <- vector("list", 2)

teleObj <- ctmm::as.telemetry(movementData,
                              timeformat = "%Y-%m-%d %H:%M:%S",
                              timezone="UTC",
                              projection = sp::CRS(SRS_string = "EPSG:32601"))
plot(teleObj)
print(attributes(teleObj)$info$identity)
print("teleObj")
# can do the slower one for the real deal
# varioDataVar <- variogram(teleObj, fast = FALSE, CI = "Gauss")
varioDataVar <- ctmm::variogram(teleObj, fast = TRUE)
print("vario")
guess <- ctmm::ctmm.guess(teleObj, interactive = FALSE)
print("guess")
# need to specify more cores???
fits <- try(
  ctmm::ctmm.select(teleObj, guess, verbose = FALSE,
                    cores = 4, method = "pHREML")
)
print("fit")
