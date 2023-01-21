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
  listSize <- length(areaMethod) *
    length(areaContour) *
    length(Method_ap) *
    length(Method_sp) +
    # rsf places
    length(areaMethod) *
    length(areaContour) *
    length(Method_ap) *
    length(Method_sp) *
    length(Method_we)

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

    for(ac in areaContour){

      print(ac)
      print(nrow(movementData))

      polyOUT <- multiverseHabitat::build_available_polygon(
        areaResource = areaOUT,
        method = am,
        contour = ac,
        SRS_string = "EPSG:32601")

      print("polyOUT")

      for(ap in Method_ap){

        for(sp in Method_sp){

          for(me in Method_method){

            if(me == "wides"){

              wiOUT <- multiverseHabitat::method_indi_wides(
                movementData = movementData,
                landscape = landscape,
                spSamp = sp,
                availableArea = polyOUT,
                availablePointsPer = ap)

              i <- i+1

              listOUT[[i]] <- data.frame(
                Estimate = wiOUT$Estimate,
                SE = wiOUT$SE,
                analysis = me,
                area = am,
                contour = ac,
                availPointsPer = ap,
                samplingPattern = sp,
                weighting = NA
              )
              print(me)

            } else if(me == "rsf"){

              for(we in Method_we){

                rsfOUT <- multiverseHabitat::method_indi_rsf(
                  movementData = movementData,
                  landscape = landscape,
                  spSamp = sp,
                  availableArea = polyOUT,
                  availablePointsPer = ap,
                  weighting = we
                )

                i <- i+1

                listOUT[[i]] <- data.frame(
                  Estimate = rsfOUT$Estimate,
                  SE = rsfOUT$SE,
                  analysis = me,
                  area = am,
                  contour = ac,
                  availPointsPer = ap,
                  samplingPattern = sp,
                  weighting = we
                )
                print(me)

              } # we
            } # if method
          } # me
        } # sp
      } # ap
    } # ac
  } # am

  return(do.call(rbind, listOUT))
} # function end
