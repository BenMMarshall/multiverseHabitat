#' Run all area based methods - wides and rsf
#'
#' @name wrapper_indi_area
#' @description A
#' @param movementData must have a x and y column for locations, and a datetime
#'   column for timestamps ("%Y-%m-%d %H:%M:%S")
#' @param landscape
#' @param optionsList Must have the following items: Method_method areaMethod
#'   areaContour Method_ap Method_sp Method_we
#' @return a
#'
#' @export
wrapper_indi_area <- function(
    movementData,
    landscape,
    optionsList
){

  Method_method <- optionsList$Method_method
  areaMethod <- optionsList$areaMethod
  areaContour <- optionsList$areaContour
  Method_ap <- optionsList$Method_ap
  Method_sp <- optionsList$Method_sp
  Method_we <- optionsList$Method_we

  # wides places
  listSize <-
    length(areaMethod) *
    length(areaContour) *
    length(Method_ap) *
    length(Method_sp) +
    # rsf places
    length(areaMethod) *
    length(areaContour) *
    length(Method_ap) *
    length(Method_sp) *
    length(Method_we) +
    # wRSF place
    1

  ## loop is better than an apply function as the for loop reduces the number of
  ## times the AKDE fit is ran
  listOUT <- vector("list",
                    length = listSize)
  i <- 0
  for(am in areaMethod){

    print(am)

    areaOUT <- multiverseHabitat::build_available_area(
      movementData = movementData,
      method = am,
      SRS_string = "EPSG:32601",
      dBBMMsettings = c(168, 48)
    )

    for(me in Method_method){

      if(!me == "wRSF"){

        for(ac in areaContour){

          print(ac)
          print(nrow(movementData))

          if(class(areaOUT)[1] == "try-error"){

            print(areaOUT)

            polyOUT <- NA

          } else {

            print(ac)
            print(nrow(movementData))

            polyOUT <- multiverseHabitat::build_available_polygon(
              areaResource = areaOUT,
              method = am,
              contour = ac,
              SRS_string = "EPSG:32601")

            print("polyOUT")

          }

          for(ap in Method_ap){

            for(sp in Method_sp){

              if(me == "wides"){

                i <- i+1

                # this tackles the instances where area estimates fail
                if(is.na(polyOUT)){

                  listOUT[[i]] <- data.frame(
                    Estimate = NA,
                    Lower = NA,
                    Upper = NA,
                    analysis = me,
                    area = am,
                    contour = ac,
                    availPointsPer = ap,
                    samplingPattern = sp,
                    weighting = NA
                  )

                } else {

                  wiOUT <- multiverseHabitat::method_indi_wides(
                    movementData = movementData,
                    landscape = landscape,
                    spSamp = sp,
                    availableArea = polyOUT,
                    availablePointsPer = ap)

                  listOUT[[i]] <- data.frame(
                    Estimate = wiOUT$Estimate,
                    Lower = wiOUT$Estimate - wiOUT$SE,
                    Upper = wiOUT$Estimate + wiOUT$SE,
                    analysis = me,
                    area = am,
                    contour = ac,
                    availPointsPer = ap,
                    samplingPattern = sp,
                    weighting = NA
                  )
                } # if poly NA
                # print(me)

              } else if(me == "rsf"){

                for(we in Method_we){

                  i <- i+1
                  if(is.na(polyOUT)){

                    listOUT[[i]] <- data.frame(
                      Estimate = NA,
                      Lower = NA,
                      Upper = NA,
                      analysis = me,
                      area = am,
                      contour = ac,
                      availPointsPer = ap,
                      samplingPattern = sp,
                      weighting = we
                    )

                  } else {

                    rsfOUT <- multiverseHabitat::method_indi_rsf(
                      movementData = movementData,
                      landscape = landscape,
                      spSamp = sp,
                      availableArea = polyOUT,
                      availablePointsPer = ap,
                      weighting = we
                    )


                    listOUT[[i]] <- data.frame(
                      Estimate = rsfOUT$Estimate,
                      Lower = rsfOUT$Estimate - rsfOUT$SE,
                      Upper = rsfOUT$Estimate + rsfOUT$SE,
                      analysis = me,
                      area = am,
                      contour = ac,
                      availPointsPer = ap,
                      samplingPattern = sp,
                      weighting = we
                    )
                  } # if poly NA

                  # print(me)

                } # we
              } # if method rsf
            } # sp
          } # ap
        } # ac

      } else if(me == "wRSF" & am == "AKDE"){

        print(me)

        i <- i+1
        if(class(areaOUT)[1] == "try-error"){

          print(areaOUT)

          listOUT[[i]] <- data.frame(
            Estimate = NA,
            Lower = NA,
            Upper = NA,
            analysis = "wRSF",
            area = NA,
            contour = NA,
            availPointsPer = NA,
            samplingPattern = NA,
            weighting = NA
          )

        } else {

          spPoints <- sp::SpatialPoints(movementData[,c("x", "y")], sp::CRS(SRS_string = "EPSG:32601"))
          spLL <- sp::spTransform(spPoints, sp::CRS(SRS_string = "EPSG:4326"))
          movementData$lon <- spLL@coords[,1]
          movementData$lat <- spLL@coords[,2]
          teleObj <- ctmm::as.telemetry(movementData,
                                        timeformat = "%Y-%m-%d %H:%M:%S",
                                        timezone="UTC",
                                        projection = sp::CRS(SRS_string = "EPSG:32601"))

          wRSF <- ctmm:::rsf.fit(teleObj,
                                 UD = areaOUT,
                                 R = list(c = landscape$classRasterLatLon),
                                 # R = list(
                                 #   c0 = r0,
                                 #   c1 = r1,
                                 #   c2 = r2),
                                 error = 0.01,
                                 reference = 1,
                                 max.mem = "1 Gb")

          # summary(wRSF)
          wRSFOUT <- summary(wRSF)$CI[1,]
          rm(wRSF)

          listOUT[[i]] <- data.frame(
            Estimate = wRSFOUT["est"],
            Lower = wRSFOUT["low"],
            Upper = wRSFOUT["high"],
            analysis = "wRSF",
            area = NA,
            contour = NA,
            availPointsPer = NA,
            samplingPattern = NA,
            weighting = NA
          )
        } # if error in akde area method
      } # if wRSF

    } # me
  } # am

  return(do.call(rbind, listOUT))
} # function end
