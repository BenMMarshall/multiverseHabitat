# iSSFs in 'amt' ----

# Load packages ----
library(amt)
library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

# Load data ----
# Load GPS data (Demo data included in 'amt')
# gps <- uhc_issf_locs

# Load habitat data (Demo data included in 'amt')
# hab <- uhc_hab

# Load movement data ------------------------------------------------------
library(here)
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

# draft code --------------------------------------------------------------

# Quick look
plot(hab)

movementData$t <- as.POSIXct(movementData$datetime)
movementTrack <- make_track(tbl = movementData, .x = x, .y = y, .t = t, crs = 32601)
movementSteps <- steps(movementTrack)

# stp <- gps %>%
#   make_track(x, y, t, crs = 32612) %>%
#   steps()

# We can use the function 'random_steps()' to generate our random steps.
# When we pass a 'steps_xyt' object, the default is for it to fit the
# gamma and von Mises distributions, just as we did above.
set.seed(2022)
modelData <- random_steps(movementSteps,
                          n_control = 10,
                          sl_distr = fit_distr(movementSteps$sl_, "gamma"),
                          ta_distr = fit_distr(movementSteps$ta_, "vonmises"))
# set.seed(2022)
# obs_avail <- random_steps(stp,
#                           n_control = 10,
#                           sl_distr = fit_distr(stp$sl_, "gamma"),
#                           ta_distr = fit_distr(stp$ta_, "vonmises"))
# View(obs_avail)

# Now that we have observed and available steps, we need to attach our
# environmental covariates to each one. We can use the functions
# 'extract_covariates()' to attach the raster values to our steps.
modelData <- amt::extract_covariates(modelData, classRaster)

# Format "cover" as factor
modelData$values <- factor(modelData$layer)

# The argument 'where = "both"' tells the function to extract the time
# of day for *both* the start of the step and the end of the step.
print(covs2, n = 3, width = 200)

# Now we're ready to fit a model!

# ... fitting a model ----
# Let's begin with a simple iSSF. We'll model our movement-free habitat
# selection kernel as a function of forage and predation risk, and we'll
# include all the movement parameters to update the selection-free movement
# kernel.

# Note that the 'amt' function 'fit_issf()' is just a wrapper for
# survival::clogit(). For much more information on the implementation,
# see the help file.
?survival::clogit

m1 <- fit_issf(modelData,
               # Response
               case_ ~
                 # Habitat
                 values +
                 # Movement
                 sl_ + log(sl_) + cos(ta_) +
                 # Stratum
                 strata(step_id_),
               # Need this later for model predictions
               model = TRUE)

# m1 <- fit_issf(covs2,
#                # Response
#                case_ ~
#                  # Habitat
#                  cover + pred +
#                  # Movement
#                  sl_ + log(sl_) + cos(ta_) +
#                  # Stratum
#                  strata(step_id_),
#                # Need this later for model predictions
#                model = TRUE)

# Let's have a look at the structure of our object.
str(m1, 1)
summary(m1$model)
# Notice that it is a list with 4 elements at the top level.
#   - $model: the actual fitted model
#   - $sl_: the tentative step-length distribution
#   - $ta_: the tentative turn-angle distribution
#   - $more: (currently empty) a placeholder for additional information

library(multiverseHabitat)

method_indi_ssf(
  movementData = movementData,
  landscape = classRaster,
  methodForm = "mf.is",
  covExtract = "end",
  availableSteps = 10
)
