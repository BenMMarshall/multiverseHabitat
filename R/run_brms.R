#' Run all brm models
#'
#' @name run_brms
#' @description A
#' @param compiledResults output tar_combine resulting in ssfResults or areaResults
#' @return a
#'
#' @export
run_brms <- function(compiledResults, method, response,
                     iter, warmup){

  palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
  names(palette) <- c("KINGCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

  # targets::tar_load(areaResults)
  # targets::tar_load(ssfResults)

  modelName <- paste0("modOUT_", method, "_", response)

  save <- here::here("notebook", "modelOutput", paste0(modelName, ".txt"))
  file <- here::here("notebook", "modelOutput", modelName)

  # RSF ---------------------------------------------------------------------

  if(method == "rsf"){


    # parse combined results converts tf to points/hour to help interpretation
    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)
    areaResults$tf <- round(areaResults$tf, digits = 2)

    modelData <- areaResults %>%
      dplyr::group_by(classLandscape) %>%
      dplyr::filter(analysis == "rsf") %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    rawDeltaEst = Estimate - medEst,
                    absDeltaEst = abs(rawDeltaEst),
                    area = factor(area,
                                  levels = c("MCP", "dBBMM",
                                             "KDEhref", "AKDE"))
      ) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
                    weightingScaled = (weighting-mean(weighting))/sd(weighting),
                    contourScaled = (contour-mean(contour))/sd(contour))

    # hist(modelDataRSF$absDeltaEst, breaks = 20000)


    # RESPONSE ----------------------------------------------------------------

    if(response == "abs"){
      # rsf
      form <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                         area + area:tdScaled + area:tfScaled +
                         contourScaled + availPointsPerScaled + samplingPattern +
                         weightingScaled +
                         (1|classLandscape) +
                         (1|species/indi))

    } else {

      form <- brms::bf(rawDeltaEst ~ 1 + tdScaled + tfScaled +
                         area + area:tdScaled + area:tfScaled +
                         contourScaled + availPointsPerScaled + samplingPattern +
                         weightingScaled +
                         (1|classLandscape) +
                         (1|species/indi))

    }

    # brms::get_prior(formRSF_absDeltaEst, data = modelDataRSF)
    # priors
    brmprior <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "samplingPatternst"),
      brms::set_prior("cauchy(0.1, 3)", coef = "availPointsPerScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "weightingScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areaAKDE")
    )

    # WIDES -------------------------------------------------------------------

  } else if(method == "wides"){
    # parse combined results converts tf to points/hour to help interpretation
    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)
    areaResults$tf <- round(areaResults$tf, digits = 2)

    modelData <- areaResults %>%
      dplyr::group_by(classLandscape) %>%
      dplyr::filter(analysis == "wides") %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    rawDeltaEst = Estimate - medEst,
                    absDeltaEst = abs(rawDeltaEst),
                    area = factor(area,
                                  levels = c("MCP", "dBBMM",
                                             "KDEhref", "AKDE"))
      ) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
                    contourScaled = (contour-mean(contour))/sd(contour))

    # RESPONSE ----------------------------------------------------------------

    if(response == "abs"){
      # wides
      form <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                         area  + area:tdScaled + area:tfScaled + contourScaled + availPointsPerScaled + samplingPattern +
                         (1|classLandscape) +
                         (1|species/indi))

    } else {

      form <- brms::bf(rawDeltaEst ~ 1 + tdScaled + tfScaled +
                         area + area:tdScaled + area:tfScaled +
                         contourScaled + availPointsPerScaled + samplingPattern +
                         (1|classLandscape) +
                         (1|species/indi))
    }

    brmprior <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "samplingPatternst"),
      brms::set_prior("cauchy(0.1, 3)", coef = "availPointsPerScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled:areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled:areaAKDE")
    )

    # SSF ---------------------------------------------------------------------

  } else if(method == "ssf"){

    ssfResults <- multiverseHabitat::parse_combined_results(compiledResults)
    ssfResults$tf <- round(ssfResults$tf, digits = 2)

    modelData <- ssfResults %>%
      dplyr::group_by(classLandscape) %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    rawDeltaEst = Estimate - medEst,
                    absDeltaEst = abs(rawDeltaEst)) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availablePerStepScaled  = (availablePerStep-mean(availablePerStep))/sd(availablePerStep))

    # REPONSE -----------------------------------------------------------------
    if(response == "abs"){
      # ssf
      form <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                         modelForm + stepDist + turnDist + availablePerStepScaled +
                         (1|classLandscape) +
                         (1|species/indi))

    } else{

      form <- brms::bf(rawDeltaEst ~ 1 + tdScaled + tfScaled +
                         modelForm + stepDist + turnDist + availablePerStepScaled +
                         (1|classLandscape) +
                         (1|species/indi))

    }

    brmprior <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "availablePerStepScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormmf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled")
    )


    # WRSF --------------------------------------------------------------------

  } else if(method == "wrsf"){

    wrsfResults <- multiverseHabitat::parse_combined_results(compiledResults[!is.na(compiledResults$Estimate),])
    wrsfResults$tf <- round(wrsfResults$tf, digits = 2)

    modelData <- wrsfResults %>%
      dplyr::group_by(classLandscape) %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    rawDeltaEst = Estimate - medEst,
                    absDeltaEst = abs(rawDeltaEst)) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td))

    # REPONSE -----------------------------------------------------------------
    if(response == "abs"){
      form <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                         (1|classLandscape) +
                         (1|species/indi))

    } else {

      form <- brms::bf(rawDeltaEst ~ 1 + tdScaled + tfScaled +
                         (1|classLandscape) +
                         (1|species/indi))
    }

    brmprior <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled")
    )

  }

  modOUT <- brms::brm(formula = form,
                      data = modelData,
                      family = gaussian,
                      prior = brmprior,
                      # warmup = 100, iter = 300, chains = 4,
                      warmup = warmup, iter = iter, chains = 4,
                      cores = 4,
                      thin = 2,
                      # control = list(adapt_delta = 0.90,
                      #                max_treedepth = 15),
                      seed = 1,
                      save_pars = brms::save_pars(all = TRUE),
                      save_model = save,
                      file = file)

  modOUT_r2 <- performance::r2_bayes(modOUT)

  modelList <- list(modOUT,
                    modOUT_r2)

  names(modelList) <- c(modelName,
                        paste0(modelName, "_r2"))

  return(
    modelList
  )

}
