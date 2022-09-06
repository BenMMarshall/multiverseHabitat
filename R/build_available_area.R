#' Generate a polygon for use as an available area
#'
#' @name build_available_area
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param method "MCP", "KDE_LSCV", "KDE_href", "AKDE", "dBBMM"
#' @param contour Numeric, > 0 and < 100.
#' @param SRS_string = "EPSG:32601"
#' @return A spatialpolygondataframe
#'
#' @export
build_available_area <- function(movementData,
                                 method = c("MCP", "KDE_LSCV", "KDE_href", "AKDE", "dBBMM"),
                                 contour,
                                 SRS_string = "EPSG:32601"){


  if(method == "MCP"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
    poly_OUT <- adehabitatHR::mcp(spPoints, percent = contour, unin = "m",
                                  unout = "m2")

  } else if(method == "KDE_LSCV"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))

    kdeLSCV_UD <- adehabitatHR::kernelUD(spPoints,
                                         h = "LSCV",
                                         grid = 120,
                                         same4all = FALSE,
                                         hlim = c(0.001, 2000), # might need to play with the limits to help convergence
                                         kern = "bivnorm",
                                         extent = 2,
                                         boundary = NULL)

    poly_OUT <- adehabitatHR::getverticeshr(kdeLSCV_UD, contour)

  } else if(method == "KDE_href"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))

    kdehref_UD <- adehabitatHR::kernelUD(spPoints,
                                         h = "href",
                                         grid = 120, # needs to be large enough to be smooth-ish
                                         same4all = FALSE,
                                         hlim = c(0.1, 1.5),
                                         kern = "bivnorm",
                                         extent = 2,
                                         boundary = NULL)

    poly_OUT <- adehabitatHR::getverticeshr(kdehref_UD, contour)

  } else if(method == "AKDE"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
    spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
    movementData$lon <- spLL@coords[,1]
    movementData$lat <- spLL@coords[,2]

    teleObj <- ctmm::as.telemetry(movementData,
                                  timeformat = "%Y-%m-%d %H:%M:%S",
                                  timezone="UTC",
                                  projection = sp::CRS(SRS_string = "EPSG:32601"))

    # can do the slower one for the real deal
    # varioDataVar <- variogram(teleObj, fast = FALSE, CI = "Gauss")
    varioDataVar <- ctmm::variogram(teleObj, fast = TRUE)
    guess <- ctmm::ctmm.guess(teleObj, interactive = FALSE)
    # need to specify more cores???
    fits <- ctmm::ctmm.select(teleObj, guess, verbose = TRUE, cores = 2, method = "pHREML")

    akdeRes <- ctmm::akde(teleObj, fits[[1]],
                          weights = TRUE)

    ## as.sf() might be a better way of doing this???
    akdePoly <- ctmm::SpatialPolygonsDataFrame.UD(akdeRes, level.UD = contour/100)
    poly_OUT <- akdePoly[akdePoly$name == akdePoly$name[2],] # just get the point estimate

  } else if(method == "dBBMM"){

    moveObj <- move::move(x = movementData$x, y = movementData$y,
                    time = movementData$datetime,
                    proj = sp::CRS(SRS_string = "EPSG:32601"))

    ws <- 25
    mrg <- 5
    set_grid.ext <- 2
    set_dimsize <- 400
    dbbmm <- move::brownian.bridge.dyn(object = moveObj,
                                 location.error = 5,
                                 margin = mrg,
                                 window.size = ws,
                                 ext = set_grid.ext,
                                 dimSize = set_dimsize,
                                 verbose = FALSE)
    # library(rgeos)

    dbbmmSP <- as(dbbmm, "SpatialPixelsDataFrame")
    dbbmmSP_UD <- new(getClass("estUD", where = "adehabitatHR"), dbbmmSP)
    dbbmmSP_UD@vol = FALSE
    dbbmmSP_UD@h$meth = "dBBMM"
    dbbmm_UD <- adehabitatHR::getvolumeUD(dbbmmSP_UD, standardize = TRUE)
    poly_OUT <- adehabitatHR::getverticeshr(dbbmm_UD, percent = 95)

  }
  return(poly_OUT)

}

# library(here)
#
# movementData <- read.csv(here("notebook", "prereg", "prelimData.csv"))
# movementData <- movementData[seq(0, 100000, by = 200),]
# movementData$datetime <- as.POSIXct(movementData$timestep * 60,
#            origin = "2022-01-01")



# # MCP ---------------------------------------------------------------------
#
# library(adehabitatHR)
#
# spPoints <- SpatialPoints(movementData[,c("x", "y")], CRS(SRS_string = "EPSG:32601"))
#
# mcp_OUT <- mcp(spPoints, percent = 100, unin = "m",
#                unout = "m2")
#
# plot(mcp_OUT)
# points(spPoints)
#
# # KDE LSCV ----------------------------------------------------------------
#
# spPoints <- SpatialPoints(movementData[,c("x", "y")], CRS(SRS_string = "EPSG:32601"))
#
# kdeLSCV_UD <- kernelUD(spPoints,
#                        h = "LSCV",
#                        grid = 120,
#                        same4all = FALSE,
#                        hlim = c(0.001, 2000), # might need to play with the limits to help convergence
#                        kern = "bivnorm",
#                        extent = 2,
#                        boundary = NULL)
#
# kdeLSCV_OUT <- getverticeshr(kdeLSCV_UD, 95)
#
# plot(kdeLSCV_OUT, add = TRUE)
#
# # KDE href ----------------------------------------------------------------
#
# kdehref_UD <- kernelUD(spPoints,
#                        h = "href",
#                        grid = 120, # needs to be large enough to be smooth-ish
#                        same4all = FALSE,
#                        hlim = c(0.1, 1.5),
#                        kern = "bivnorm",
#                        extent = 2,
#                        boundary = NULL)
#
# kdehref_OUT <- getverticeshr(kdehref_UD, 95)
#
# plot(kdehref_OUT, add = TRUE)
#
# # AKDE --------------------------------------------------------------------
#
# library(sp)
#
# spPoints <- SpatialPoints(movementData[,c("x", "y")], CRS(SRS_string = "EPSG:32601"))
# spLL <- spTransform(spPoints, CRS(SRS_string = "EPSG:4326"))
# movementData$lon <- spLL@coords[,1]
# movementData$lat <- spLL@coords[,2]
#
# library(ctmm)
#
# teleObj <- as.telemetry(movementData,
#                         timeformat = "%Y-%m-%d %H:%M:%S",
#                         timezone="UTC",
#                         projection = CRS(SRS_string = "EPSG:32601"))
#
# # can do the slower one for the real deal
# # varioDataVar <- variogram(teleObj, fast = FALSE, CI = "Gauss")
# varioDataVar <- variogram(teleObj, fast = TRUE)
# guess <- ctmm.guess(teleObj, interactive = FALSE)
# # need to specify more cores???
# fits <- ctmm.select(teleObj, guess, verbose = TRUE, cores = 2, method = "pHREML")
#
# akdeRes <- akde(teleObj, fits[[1]],
#                 weights = TRUE)
#
# ## as.sf() might be a better way of doing this???
# akde_OUT <- SpatialPolygonsDataFrame.UD(akdeRes, level.UD = 0.95)
#
# plot(akde_OUT, add = TRUE)
#
# # dBBMM -------------------------------------------------------------------
#
# library(move)
#
# moveObj <- move(x = movementData$x, y = movementData$y,
#                 time = movementData$datetime,
#                 proj = CRS(SRS_string = "EPSG:32601"))
#
# ws <- 25
# mrg <- 5
# set_grid.ext <- 2
# set_dimsize <- 400
# dbbmm <- brownian.bridge.dyn(object = moveObj,
#                              location.error = 5,
#                              margin = mrg,
#                              window.size = ws,
#                              ext = set_grid.ext,
#                              dimSize = set_dimsize,
#                              verbose = FALSE)
# library(rgeos)
#
# dbbmmSP <- as(dbbmm, "SpatialPixelsDataFrame")
# dbbmmSP_UD <- new("estUD", dbbmmSP)
# dbbmmSP_UD@vol = FALSE
# dbbmmSP_UD@h$meth = "dBBMM"
# dbbmm_UD <- getvolumeUD(dbbmmSP_UD, standardize = TRUE)
# dbbmm_OUT <- getverticeshr(dbbmm_UD, percent = 95)
#
# plot(dbbmm_OUT, add = TRUE)



