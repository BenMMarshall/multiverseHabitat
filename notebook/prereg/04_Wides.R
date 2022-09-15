library(here)
library(multiverseHabitat)

movementData <- read.csv(here("notebook", "prereg", "prelimData.csv"))
movementData <- movementData[seq(0, 100000, by = 100),]
movementData$datetime <- as.POSIXct(movementData$timestep * 60,
           origin = "2022-01-01")

library(raster)
load(file = here("notebook", "prereg", "landscapePrelim.RData"))
classRaster <- raster(classLandscapeList$classified, crs = sp::CRS(SRS_string = "EPSG:32601"),
                      xmn = 0, xmx = ncol(classLandscapeList$classified),
                      ymn = 0, ymx = nrow(classLandscapeList$classified))

out <- build_available_area(movementData = movementData,
                     method = "dBBMM",
                     contour = 90,
                     dBBMMsettings = c(25, 5))
sp::plot(out)

### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
availPoints <- spsample(out, n = 1000, type = "random")

availValues <- raster::extract(classRaster, availPoints)
availValues_DF <- data.frame(rbind(table(availValues)))
names(availValues_DF) <- c("c0", "c1", "c2")

availVector <- as.vector(table(availValues))
names(availVector) <- c("c0", "c1", "c2")

library(adehabitatHS)

usedValues <- raster::extract(classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                             sp::CRS(SRS_string = "EPSG:32601")))
usedValues <- data.frame(rbind(table(usedValues)))
names(usedValues) <- c("c0", "c1", "c2")

# for widesI and widesII, a vector with named elements describing the sample or
# the proportion of available resource units. For widesIII a matrix or a data
# frame giving the number or the proportion of available resource units for each
# animal (in rows) in each resource category (in columns)

# so because the
# difference is use the available, we can do III design with the II set up just
# with different availabilities
wiOUT <- widesIII(u = usedValues, a = availValues_DF)

wiOUT$wi[3]

# adehabitatHS::compana(used = usedValues, avail = availValues_DF)
#
# data(squirrel)
#
# ## Second order habitat selection
# ## Selection of home range within the
# ## study area
# squiana2 <- compana(squirrel$mcp, squirrel$studyarea)
# squiana2
#
# ## The ranking matrix:
# print(squiana2$rm, quote = FALSE)
#
# compOUT <- adehabitatHS::eisera(used = usedValues, available = availValues_DF,
#                                 nf = 4, scannf = FALSE)
#
# compOUT
#
# data(squirrel)
#
# ## computation of the number of relocations in each habitat type
# ## from the data given by Aebischer et al. (1993).
# ## squirrel$locs give the percentage of relocations in each habitat
# ## type, and Aebischer et al. (1993) indicate that there are 30
# ## relocations per animal.
# ## We therefore compute the number of relocations in each habitat type
# ## using:
# us <- round(30 * squirrel$locs / 100)
#
# ## Habitat availability
# av <- squirrel$studyarea
#
# ## Eigenanalysis of selection ratios
# ii <- eisera(us, av, scannf = FALSE)
