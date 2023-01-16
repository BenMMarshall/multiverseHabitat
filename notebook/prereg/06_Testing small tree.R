
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


landscape <- testLand
movementData <- sampDuraFreqData

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

optionsList_sff <- list(
  Method_method = c("ssf"),
  MethodSSF_mf = c("mf.is", "mf.ss"),
  MethodSSF_sd = c("gamma", "exp"),
  MethodSSF_td = c("vonmises", "unif"),
  MethodSSF_as = as.integer(round(exp(seq(log(5), log(500), length.out = 5)), digits = 1))
  # MethodSSF_as = c(5, 50)
)

targets::tar_load("sampDuraFreqData_7_6_1_BADGER")
targets::tar_load("landscape_BADGER")

multiverseHabitat::wrapper_indi_area(
  movementData = sampDuraFreqData_7_6_1_BADGER,
  landscape = landscape_BADGER,
  optionsList = optionsList_area
)


testOUT <- multiverseHabitat::wrapper_indi_area(
  movementData = movementData,
  landscape = landscape,
  optionsList = optionsList_area
)
testOUT <- multiverseHabitat::wrapper_indi_ssf(
  movementData = movementData,
  landscape = landscape,
  optionsList = optionsList_ssf
)

# function start here -----------------------------------------------------

Method_method <- optionsList_area$Method_method
areaMethod <- optionsList_area$areaMethod
areaContour <- optionsList_area$areaContour
Method_ap <- optionsList_area$Method_ap
Method_sp <- optionsList_area$Method_sp
Method_we <- optiooptionsList_areansList$Method_we

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
              samplingPattern = sp
            )
            print(i)

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
                samplingPattern = sp
              )
              print(i)

            }

          }

          # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
        }

      }
    }
  }
}

egOUT <- do.call(rbind, listOUT)
egOUT[egOUT$analysis == "rsf",]





# ssf fix function --------------------------------------------------------
# wrapper_indi_ssf

Method_method <- optionsList_sff$Method_method
MethodSSF_mf <- optionsList_sff$MethodSSF_mf
MethodSSF_sd <- optionsList_sff$MethodSSF_sd
MethodSSF_td <- optionsList_sff$MethodSSF_td
MethodSSF_as <- optionsList_sff$MethodSSF_as

# ssf places
listSize <- length(MethodSSF_mf) *
  length(MethodSSF_ce) *
  length(MethodSSF_as)

## loop is better than an apply function as the for loop reduces the number of
## times the AKDE fit is ran
listOUT <- vector("list",
                  length = listSize)
i <- 0
for(mf in MethodSSF_mf){

  for(sd in MethodSSF_sd){
    for(td in MethodSSF_td){

      for(as in MethodSSF_as){

        ssfOUT <- multiverseHabitat::method_indi_ssf(
          movementData = movementData,
          landscape = landscape,
          methodForm = mf,
          stepDist = sd,
          turnDist = td,
          availableSteps = as
        )

        i <- i+1

        listOUT[[i]] <- data.frame(
          Estimate = ssfOUT$Estimate,
          SE = ssfOUT$SE,
          analysis = "ssf",
          modelForm = mf,
          stepDist = sd,
          turnDist = td,
          availablePerStep = as
        )
        print(i)
      } # as
    } # sd
  } # td
} # mf

do.call(rbind, listOUT)




movementData = movementData
landscape = landscape
methodForm = mf
availableSteps = as

movementData$t <- as.POSIXct(movementData$datetime)
movementTrack <- amt::make_track(tbl = movementData, .x = x, .y = y, .t = t, crs = 32601)
movementSteps <- amt::steps(movementTrack)

set.seed(2022)
modelData <- amt::random_steps(movementSteps,
                               n_control = availableSteps,
                               sl_distr = amt::fit_distr(movementSteps$sl_, MethodSSF_sd),
                               ta_distr = amt::fit_distr(movementSteps$ta_, MethodSSF_td))

modelData <- amt::extract_covariates(modelData,
                                     landscape$classRaster,
                                     where = "end")

modelData$values <- paste0("c", modelData$layer)
modelData$values <- factor(modelData$values)

if(methodForm == "mf.is"){
  mFormFull <- case_ ~
    values +
    sl_ + log(sl_) + cos(ta_) +
    strata(step_id_)

} else if(methodForm == "mf.ss"){
  mFormFull <- case_ ~
    values +
    strata(step_id_)

}

ssfOUT <- amt::fit_issf(data = modelData,
                        formula = mFormFull,
                        model = TRUE)

ssfDF <- as.data.frame(summary(ssfOUT)$coef)
method <- rep("ssf", nrow(ssfDF))
ssfDF <- cbind(ssfDF, method)
