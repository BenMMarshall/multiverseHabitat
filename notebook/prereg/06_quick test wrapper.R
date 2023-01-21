
optionsList_area <- list(
  Method_method = c("wides", "rsf"),
  areaMethod = c("MCP", "KDEhref", "AKDE", "dBBMM"),
  areaContour = c(90, 95, 99),
  Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 4)), digits = 1)),
  # Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 2)), digits = 1)),
  Method_sp = c("rd", "st"),
  # Method_ap = 100,
  Method_we = exp(seq(log(1), log(10000000), length.out = 3))
  # Method_we = 1
)
# • start target sampDuraFreqData_30_0.5_2_VULTURE
# • start target sampDuraFreqData_60_1_2_VULTURE
# • start target sampDuraFreqData_120_0.5_1_VULTURE
# • start target sampDuraFreqData_30_1_2_VULTURE
# • start target sampDuraFreqData_30_48_2_VULTURE good
# • start target sampDuraFreqData_60_48_3_VULTURE good - but meant to be the issue
# • start target sampDuraFreqData_7_1_2_VULTURE good
# • start target sampDuraFreqData_7_2_2_VULTURE good
targets::tar_load("sampDuraFreqData_30_1_2_VULTURE")
targets::tar_load("landscape_VULTURE")

movementData <- sampDuraFreqData_30_1_2_VULTURE
landscape <- landscape_VULTURE

Method_method <- optionsList_area$Method_method
areaMethod <- optionsList_area$areaMethod
areaContour <- optionsList_area$areaContour
Method_ap <- optionsList_area$Method_ap
Method_sp <- optionsList_area$Method_sp
Method_we <- optionsList_area$Method_we

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

    polyOUT <- multiverseHabitat::build_available_polygon(
      areaResource = areaOUT,
      method = am,
      contour = ac,
      SRS_string = "EPSG:32601")

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

do.call(rbind, listOUT)
} # function end
