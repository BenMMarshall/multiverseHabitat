
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
)

targets::tar_load("sampDuraFreqData_60_48_3_VULTURE")
targets::tar_load("landscape_VULTURE")

movementData <- sampDuraFreqData_60_48_3_VULTURE
landscape <- landscape_VULTURE

multiverseHabitat::wrapper_indi_area(
  movementData = sampDuraFreqData_60_48_3_VULTURE,
  landscape = landscape_VULTURE,
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
  # am <- areaMethod[3]
  cat(am)

  areaOUT <- multiverseHabitat::build_available_area(
    movementData = movementData,
    method = am,
    SRS_string = "EPSG:32601",
    dBBMMsettings = c(168, 48)
  )


  for(ac in areaContour){
    # ac <- areaContour[2]

    polyOUT <- multiverseHabitat::build_available_polygon(
      areaResource = areaOUT,
      method = am,
      contour = ac,
      SRS_string = "EPSG:32601")


    for(ap in Method_ap){
      # ap <- Method_ap[1]
      for(sp in Method_sp){
        # sp <- Method_sp[1]
        for(me in Method_method){
          # me <- Method_method[1]
          if(me == "wides"){

            # wiOUT <- multiverseHabitat::method_indi_wides(
            #   movementData = movementData,
            #   landscape = landscape,
            #   spSamp = sp,
            #   availableArea = polyOUT,
            #   availablePointsPer = ap)

            spSamp = sp
            availableArea = polyOUT
            availablePointsPer = ap

            suppressWarnings({
              availPoints <- sp::spsample(availableArea,
                                          n = nrow(movementData) * availablePointsPer,
                                          type = ifelse(spSamp == "rd", "random", "stratified"))
            })

            # extract the habitat types each point is located within
            availValues <- raster::extract(landscape$classRaster, availPoints)

            availValues_DF <- data.frame(rbind(table(availValues)))
            names(availValues_DF) <- sub("X", "c", names(availValues_DF))

            suppressWarnings({
              usedValues <- raster::extract(landscape$classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                                                     sp::CRS(SRS_string = "EPSG:32601")))
            })
            usedValues_DF <- data.frame(rbind(table(usedValues)))
            names(usedValues_DF) <- sub("X", "c", names(usedValues_DF))

            aClass <- names(availValues_DF)
            uClass <- names(usedValues_DF)

            print(6)

            if(length(aClass) > length(uClass)){

              toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(aClass[!aClass %in% uClass])))
              names(toAdd) <- aClass[!aClass %in% uClass]
              usedValues_DF <- cbind(usedValues_DF, toAdd)

            } else if(length(uClass) > length(aClass)){

              toAdd <- as.data.frame(matrix(0, nrow = 1, ncol = length(uClass[!uClass %in% aClass])))
              names(toAdd) <- uClass[!uClass %in% aClass]
              availValues_DF <- cbind(availValues_DF, toAdd)

            }


            usedValues_DF <- usedValues_DF[,sort(names(usedValues_DF))]
            availValues_DF <- availValues_DF[,sort(names(availValues_DF))]
            # so because the difference is use the available, we can do III design with the
            # II set up just with different availabilities. Is in a try() function because
            # of the instances where habitats are used, but not available bceause of the
            # random sampling of available points
            wiOUT <- try(
              adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)
            )
            if(class(wiOUT)[1] == "try-error"){
              wiOUT <- wiOUT[1]
            }
            # wiOUT <- adehabitatHS::widesIII(u = usedValues_DF, a = availValues_DF)

            wiOUT <- multiverseHabitat::extract_estimate(wiOUT)

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
            cat(i)

          } else if(me == "rsf"){

            for(we in Method_we){
              # we <- Method_we[1]

              # rsfOUT <- multiverseHabitat::method_indi_rsf(
              #   movementData = movementData,
              #   landscape = landscape,
              #   spSamp = sp,
              #   availableArea = polyOUT,
              #   availablePointsPer = ap,
              #   weighting = we
              # )
                spSamp = sp
                availableArea = polyOUT
                availablePointsPer = ap
                weighting = we

                suppressWarnings({
                  availPoints <- sp::spsample(availableArea,
                                              n = nrow(movementData) * availablePointsPer,
                                              type = ifelse(spSamp == "rd", "random", "stratified"))
                })

                # extract the habitat types each point is located within
                availValues <- raster::extract(landscape$classRaster, availPoints)

                availValues_DF <- as.data.frame(availPoints@coords)
                availValues_DF$values <- as.factor(availValues)
                # case_ == false cos not used
                availValues_DF$case_ <- FALSE
                # assign the weighting
                availValues_DF$weights <- we
                names(availValues_DF)[1:2] <- c("x", "y")

                suppressWarnings({
                  usedValues <- raster::extract(landscape$classRaster, sp::SpatialPoints(movementData[,c("x", "y")],
                                                                                         sp::CRS(SRS_string = "EPSG:32601")))
                })
                movementData$values <- as.factor(usedValues)
                modelData <- movementData[,c("x", "y", "values")]
                # used gets case_ == TRUE, and weights == 1
                modelData$case_ <- TRUE
                modelData$weights <- 1

                modelData <- rbind(modelData, availValues_DF)
                modelData$values <- paste0("c", modelData$values)

                # fit the model using base R glm()
                rsfOUT <- glm(case_ ~ values,
                              family = binomial(),
                              data = modelData,
                              weights = weights)

                rsfDF <- as.data.frame(summary(rsfOUT)$coef)
                method <- rep("rsf", nrow(rsfDF))
                rsfDF <- cbind(rsfDF, method)

                rsfOUT <- multiverseHabitat::extract_estimate(rsfDF)

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
              cat(i)

            } # we
          } # if method
        } # me
      } # sp
    } # ap
  } # ac
} # am

do.call(rbind, listOUT)



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
