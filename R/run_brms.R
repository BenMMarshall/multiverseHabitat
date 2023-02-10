#' Run all brm models
#'
#' @name run_brms
#' @description A
#' @param compiledResults output tar_combine resulting in ssfResults or areaResults
#' @return a
#'
#' @export
run_brms <- function(compiledResults, method){

  palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
  names(palette) <- c("KINGCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

  # targets::tar_load(areaResults)
  # targets::tar_load(ssfResults)

  if(method == "rsf"){


    # parse combined results converts tf to points/hour to help interpretation
    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)
    areaResults$tf <- round(areaResults$tf, digits = 2)

    modelDataRSF <- areaResults %>%
      dplyr::filter(analysis == "rsf") %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    absDeltaEst = abs(Estimate - medEst),
                    positive = ifelse(Estimate > 0, 1, 0),
                    area = factor(area,
                                  levels = c("MCP", "dBBMM",
                                             "KDEhref", "AKDE"))
                    ) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
                    weightingScaled = (weighting-mean(weighting))/sd(weighting),
                    contourScaled = (contour-mean(contour))/sd(contour))

    hist(modelDataRSF$absDeltaEst, breaks = 20000)
    # model formula
    # rsf
    formRSF_absDeltaEst <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                                      area + area:tdScaled + area:tfScaled +
                                      contourScaled + availPointsPerScaled + samplingPattern +
                                      weightingScaled +
                                      (1|species/indi))

    formRSF_binPositive <- brms::bf(positive ~ 1 + tdScaled + tfScaled +
                                      area + area:tdScaled + area:tfScaled +
                                      contourScaled + availPointsPerScaled + samplingPattern +
                                      weightingScaled +
                                      (1|species/indi))

    # brms::get_prior(formRSF_absDeltaEst, data = modelDataRSF)
    # priors
    brmpriorRSF <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
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

    modOUT_dEstRSF <- brms::brm(formula = formRSF_absDeltaEst,
                                data = modelDataRSF,
                                family = gaussian,
                                prior = brmpriorRSF,
                                warmup = 200, iter = 1000, chains = 4,
                                # warmup = 2000, iter = 5000, chains = 4,
                                cores = 4,
                                thin = 2,
                                # control = list(adapt_delta = 0.90,
                                #                max_treedepth = 15),
                                seed = 1,
                                save_pars = brms::save_pars(all = TRUE),
                                save_model = here::here("notebook", "modelOutput", "absDeltaEstModel_RSF.txt"),
                                file = here::here("notebook", "modelOutput", "absDeltaEstModel_RSF"))

    # modOUT_bPosRSF <- brms::brm(formula = formRSF_binPositive,
    #                             data = modelDataRSF,
    #                             family = brms::bernoulli,
    #                             prior = brmpriorRSF,
    #                             warmup = 200, iter = 1000, chains = 4,
    #                             # warmup = 2000, iter = 5000, chains = 4,
    #                             cores = 4,
    #                             thin = 2,
    #                             # control = list(adapt_delta = 0.90,
    #                             #                max_treedepth = 15),
    #                             seed = 1,
    #                             save_pars = brms::save_pars(all = TRUE),
    #                             save_model = here::here("notebook", "modelOutput", "binPositiveModel_RSF.txt"),
    #                             file = here::here("notebook", "modelOutput", "binPositiveModel_RSF"))

    # return(list(method = "rsf",
    #             modOUT_dEst = modOUT_dEstRSF,
    #             modOUT_bPos = modOUT_bPosRSF))
    return(list(method = "rsf",
                modOUT_dEst = modOUT_dEstRSF))

  } else if(method == "wides"){
    # parse combined results converts tf to points/hour to help interpretation
    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)
    areaResults$tf <- round(areaResults$tf, digits = 2)

    modelDataWides <- areaResults %>%
      dplyr::filter(analysis == "wides") %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    absDeltaEst = abs(Estimate - medEst),
                    positive = ifelse(Estimate > 0, 1, 0),
                    area = factor(area,
                                  levels = c("MCP", "dBBMM",
                                             "KDEhref", "AKDE"))
      ) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
                    contourScaled = (contour-mean(contour))/sd(contour))


    hist(modelDataWides$absDeltaEst, breaks = 20000)
    # 2 % of the wides results are NA
    sum(is.na(modelDataWides$Estimate))/
      nrow(modelDataWides) *100
    # wides
    formWides_absDeltaEst <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                                        area  + area:tdScaled + area:tfScaled + contourScaled + availPointsPerScaled + samplingPattern +
                                        (1|species/indi))

    formWides_binPositive <- brms::bf(positive ~ 1 + tdScaled + tfScaled +
                                        area + area:tdScaled + area:tfScaled +
                                        contourScaled + availPointsPerScaled + samplingPattern +
                                        (1|species/indi))

    # brms::get_prior(formWides_absDeltaEst, data = modelDataWides)

    brmpriorWides <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
      brms::set_prior("cauchy(0.1, 3)", coef = "areaAKDE"),
      brms::set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
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

    modOUT_dEstWides <- brms::brm(formula = formWides_absDeltaEst,
                                  data = modelDataWides,
                                  family = gaussian,
                                  prior = brmpriorWides,
                                  warmup = 200, iter = 1000, chains = 4,
                                  # warmup = 2000, iter = 5000, chains = 4,
                                  cores = 4,
                                  thin = 2,
                                  # control = list(adapt_delta = 0.90,
                                  #                max_treedepth = 15),
                                  seed = 1,
                                  save_pars = brms::save_pars(all = TRUE),
                                  save_model = here::here("notebook", "modelOutput", "absDeltaEstModel_Wides.txt"),
                                  file = here::here("notebook", "modelOutput", "absDeltaEstModel_Wides"))

    # modOUT_bPosWides <- brms::brm(formula = formWides_binPositive,
    #                               data = modelDataWides,
    #                               family = brms::bernoulli,
    #                               prior = brmpriorWides,
    #                               warmup = 200, iter = 1000, chains = 4,
    #                               # warmup = 2000, iter = 5000, chains = 4,
    #                               cores = 4,
    #                               thin = 2,
    #                               # control = list(adapt_delta = 0.90,
    #                               #                max_treedepth = 15),
    #                               seed = 1,
    #                               save_pars = brms::save_pars(all = TRUE),
    #                               save_model = here::here("notebook", "modelOutput", "binPositiveModel_Wides.txt"),
    #                               file = here::here("notebook", "modelOutput", "binPositiveModel_Wides"))

    # return(list(method = "wides",
    #             modOUT_dEst = modOUT_dEstWides,
    #             modOUT_bPos = modOUT_bPosWides))
    return(list(method = "wides",
                modOUT_dEst = modOUT_dEstWides))


  } else if(method == "ssf"){

    ssfResults <- multiverseHabitat::parse_combined_results(compiledResults)
    ssfResults$tf <- round(ssfResults$tf, digits = 2)

    modelDataSSF <- ssfResults %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE),
                    absDeltaEst = abs(Estimate - medEst),
                    positive = ifelse(Estimate > 0, 1, 0)) %>%
      dplyr::mutate(tfScaled = (tf-mean(tf))/sd(tf),
                    tdScaled = (td-mean(td))/sd(td),
                    availablePerStepScaled  = (availablePerStep-mean(availablePerStep))/sd(availablePerStep))

    hist(modelDataSSF$absDeltaEst, breaks = 20000)
    # ssf
    formSSF_absDeltaEst <- brms::bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                                      modelForm + stepDist + turnDist + availablePerStepScaled +
                                      (1|species/indi))

    formSSF_binPositive <- brms::bf(positive ~ 1 + tdScaled + tfScaled +
                                      modelForm + stepDist + turnDist + availablePerStepScaled +
                                      (1|species/indi))
    # AKA (1|species) + (1|species:indi) for a nested group effect intercept
    # brms::get_prior(formSSF_absDeltaEst, data = modelDataSSF)
    # brms::get_prior(formSSF_binPositive, data = modelDataSSF)
    brmpriorSSF <- c(
      brms::set_prior("cauchy(0.1, 3)", coef = "availablePerStepScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "modelFormmf.ss"),
      brms::set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
      brms::set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
      brms::set_prior("cauchy(0.1, 3)", coef = "tfScaled")
    )
    # ggplot(data.frame("x" = rcauchy(2000, location = 0.1, scale = 1))) +
    #   geom_density(aes(x = x)) +
    #   coord_cartesian(xlim = c(-100, 100))

    modOUT_dEstSSF <- brms::brm(formula = formSSF_absDeltaEst,
                                data = modelDataSSF,
                                family = gaussian,
                                prior = brmpriorSSF,
                                warmup = 200, iter = 1000, chains = 4,
                                # warmup = 2000, iter = 5000, chains = 4,
                                cores = 4,
                                thin = 2,
                                # control = list(adapt_delta = 0.90,
                                #                max_treedepth = 15),
                                seed = 1,
                                save_pars = brms::save_pars(all = TRUE),
                                save_model = here::here("notebook", "modelOutput", "absDeltaEstModel_SSF.txt"),
                                file = here::here("notebook", "modelOutput", "absDeltaEstModel_SSF"))

    # modOUT_bPosSSF <- brms::brm(formula = formSSF_binPositive,
    #                             data = modelDataSSF,
    #                             family = brms::bernoulli,
    #                             prior = brmpriorSSF,
    #                             warmup = 200, iter = 1000, chains = 4,
    #                             # warmup = 2000, iter = 5000, chains = 4,
    #                             cores = 4,
    #                             thin = 2,
    #                             # control = list(adapt_delta = 0.90,
    #                             #                max_treedepth = 15),
    #                             seed = 1,
    #                             save_pars = brms::save_pars(all = TRUE),
    #                             save_model = here::here("notebook", "modelOutput", "binPositiveModel_SSF.txt"),
    #                             file = here::here("notebook", "modelOutput", "binPositiveModel_SSF"))

    # return(list(method = "ssf",
    #             modOUT_dEst = modOUT_dEstSSF,
    #             modOUT_bPos = modOUT_bPosSSF))
    return(list(method = "ssf",
                modOUT_dEst = modOUT_dEstSSF))
  }
}
