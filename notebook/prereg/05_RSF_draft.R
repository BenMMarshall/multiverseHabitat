library(here)
library(multiverseHabitat)


# Load movement data ------------------------------------------------------

# species
sam_01_sp <- c("sp.B", "sp.V", "sp.K")
# tracking frequency hours
sam_02_tf <- c("tf.0.5", "tf.01", "tf.02", "tf.06", "tf.12", "tf.24", "tf.48", "tf.168")
# tracking duration days
sam_03_td <- c("td.007", "td.015", "td.030", "td.060", "td.120", "td.240", "td.365")

dataLocation <- list.files(here("notebook", "prereg", "prelimMultiData", "dataSubset"),
                           full.names = TRUE)[1]

movementData <- read.csv(dataLocation)

# Load landscape ----------------------------------------------------------

library(raster)
load(file = here("notebook", "prereg", "sp.B_landscapePrelim.RData"))
classRaster <- raster(classLandscapeList$classified, crs = sp::CRS(SRS_string = "EPSG:32601"),
                      xmn = 0, xmx = ncol(classLandscapeList$classified),
                      ymn = 0, ymx = nrow(classLandscapeList$classified))

# Load polygon data -------------------------------------------------------

load(paste0(here("notebook", "prereg", "prelimMultiData", "polyData"),
            "/sp.B__i001__tf.0.5__td.007__aa.ak__ac.90.RData"))


# RSF ---------------------------------------------------------------------

# options for RSF
# area, points, weighting

weighting <- 1000

library(amt)

### POSSIBLE NEW NODE, RANDOM VERSUS SYSTEMATIC???
availPoints <- spsample(p90, n = 1000, type = "random")

availValues <- raster::extract(classRaster, availPoints)

availValues_DF <- as.data.frame(availPoints@coords)
availValues_DF$values <- as.factor(availValues)
availValues_DF$case_ <- FALSE

usedValues <- raster::extract(classRaster,
                              sp::SpatialPoints(movementData[,c("x", "y")],
                                                sp::CRS(SRS_string = "EPSG:32601")))
movementData$values <- as.factor(usedValues)

modelData <- movementData[,c("x", "y", "values")]
modelData$case_ <- TRUE

modelData <- rbind(modelData, availValues_DF)


## add weighting to the dataset
modelData$weights <- ifelse(modelData$case_, 1, weighting)

# We can now fit the model using base R's 'glm()'
rsfOUT <- glm(case_ ~ values,
           family = binomial(),
           data = modelData,
           weights = weights)

# Check the summary
summary(rsfOUT)

library(multiverseHabitat)

method_indi_rsf(
  movementData = movementData,
  landscape = classRaster,
  availableArea = p90,
  availabelPoints = 1000,
  weighting = 1000
)


# rsfOUT <- fit_rsf(data = modelData,
#                   formula = case_ ~ values, weights = weights)
#
# summary(rsfOUT)

library(amt)
data("deer")
hr <- hr_mcp(deer)
r1 <- random_points(hr, n = 500)
rsf1 <- deer %>% random_points() %>%
  extract_covariates(sh_forest) %>%
  mutate(forest = sh.forest == 1)
sum(rsf1$case_)
