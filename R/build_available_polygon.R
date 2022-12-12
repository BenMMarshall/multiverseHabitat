#' Generate a polygon for use as an available area
#'
#' @name build_available_polygon
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime
#'   column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param method "MCP", "KDE_LSCV", "KDE_href", "AKDE", "dBBMM"
#' @param contour Numeric, > 0 and < 100. Can be a vector.
#' @param SRS_string = "EPSG:32601"
#' @param dBBMMsettings time duration of window and margin in hours, function
#'   translate duration to datapoints
#' @return A list, where the first items is the fit or UD that the polygon is
#'   extracted from, and the second item is the polygon ready for further
#'   analysis
#'
#' @export
build_available_polygon <- function(areaResource,
                                    method = c("MCP", "KDE_LSCV", "KDE_href", "AKDE", "dBBMM"),
                                    contour,
                                    SRS_string = "EPSG:32601"){


  if(method == "MCP"){

    spPoints <- areaResource

    poly_OUT <- vector("list", 2)
    poly_OUT[[1]] <- "MCP"
    poly_OUT[[2]] <- adehabitatHR::mcp(spPoints, percent = contour, unin = "m",
                                       unout = "m2")

    # poly_List <- lapply(contour, function(x){
    #   poly_OUT <- adehabitatHR::mcp(spPoints, percent = x, unin = "m",
    #                                 unout = "m2")
    return(poly_OUT)
    # })

  } else if(method == "KDE_LSCV"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))

    poly_OUT <- vector("list", 2)
    kdeLSCV_UD <- adehabitatHR::kernelUD(spPoints,
                                         h = "LSCV",
                                         grid = 240,
                                         same4all = FALSE,
                                         hlim = c(0.001, 2000), # might need to play with the limits to help convergence
                                         kern = "bivnorm",
                                         extent = 4,
                                         boundary = NULL)
    poly_OUT[[1]] <- kdeLSCV_UD
    poly_OUT[[2]] <- adehabitatHR::getverticeshr(kdeLSCV_UD, contour)

    # poly_List <- lapply(contour, function(x){
    #   poly_OUT <- adehabitatHR::getverticeshr(kdeLSCV_UD, x)
    return(poly_OUT)
    # })

  } else if(method == "KDE_href"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
    poly_OUT <- vector("list", 2)
    kdehref_UD <- adehabitatHR::kernelUD(spPoints,
                                         h = "href",
                                         grid = 240, # needs to be large enough to be smooth-ish
                                         same4all = FALSE,
                                         hlim = c(0.1, 1.5),
                                         kern = "bivnorm",
                                         extent = 4,
                                         boundary = NULL)
    poly_OUT[[1]] <- kdehref_UD
    poly_OUT[[2]] <- adehabitatHR::getverticeshr(kdehref_UD, contour)

    # poly_List <- lapply(contour, function(x){
    #   poly_OUT <- adehabitatHR::getverticeshr(kdehref_UD, x)
    return(poly_OUT)
    # })

  } else if(method == "AKDE"){

    spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
    spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
    movementData$lon <- spLL@coords[,1]
    movementData$lat <- spLL@coords[,2]

    poly_OUT <- vector("list", 2)

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

    poly_OUT[[1]] <- akdeRes

    akdePoly <- ctmm::SpatialPolygonsDataFrame.UD(akdeRes, level.UD = contour/100)
    poly_OUT[[2]] <- akdePoly[akdePoly$name == akdePoly$name[2],] # just get the point estimate

    ## as.sf() might be a better way of doing this???
    # poly_List <- lapply(contour, function(x){
    #   akdePoly <- ctmm::SpatialPolygonsDataFrame.UD(akdeRes, level.UD = x/100)
    #   poly_OUT <- akdePoly[akdePoly$name == akdePoly$name[2],] # just get the point estimate
    return(poly_OUT)
    # })


  } else if(method == "dBBMM"){

    if(is.null(dBBMMsettings)){
      stop("dBBMMsettings required for dBBMM running, 2 length vector of time duration of ws and mrg (in hours)")
    }

    # As our most infrequent tracking is 168 hours (1 week), we will set the
    # window to the number of data points collected over 168 hours, and a margin
    # of 48 hours.
    windowSize <- nrow(movementData[movementData$timestep <= dBBMMsettings[1]*60,])
    if(windowSize %% 2 == 0){
      windowSize <- windowSize - 1
    }

    marginSize <- nrow(movementData[movementData$timestep <= dBBMMsettings[2]*60,])
    if(marginSize %% 2 == 0){
      marginSize <- marginSize - 1
    }

    poly_OUT <- vector("list", 2)

    moveObj <- move::move(x = movementData$x, y = movementData$y,
                          time = movementData$datetime,
                          proj = sp::CRS(SRS_string = "EPSG:32601"))

    # ws <- 25
    # mrg <- 5
    set_grid.ext <- 4
    set_dimsize <- 400
    dbbmm <- move::brownian.bridge.dyn(object = moveObj,
                                       location.error = 5,
                                       window.size = windowSize,
                                       margin = marginSize,
                                       ext = set_grid.ext,
                                       dimSize = set_dimsize,
                                       verbose = FALSE)

    poly_OUT[[1]] <- dbbmm
    # library(rgeos)

    dbbmmSP <- as(dbbmm, "SpatialPixelsDataFrame")
    dbbmmSP_UD <- new(getClass("estUD", where = "adehabitatHR"), dbbmmSP)
    dbbmmSP_UD@vol = FALSE
    dbbmmSP_UD@h$meth = "dBBMM"
    dbbmm_UD <- adehabitatHR::getvolumeUD(dbbmmSP_UD, standardize = TRUE)

    poly_OUT[[2]] <- adehabitatHR::getverticeshr(dbbmm_UD, percent = contour)
    # poly_List <- lapply(contour, function(x){
    # poly_OUT <- adehabitatHR::getverticeshr(dbbmm_UD, percent = x)
    return(poly_OUT)
    # })

  }
  return(poly_List)

}

