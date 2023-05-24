#' Generate files needed for RMD manuscript
#'
#' @name extract_values_rmd
#' @importFrom magrittr %>%
#' @description Extract the required values to report in the RMD file.
#' @return Will save a series of csv files containing the raw estimates and
#'   model betas that are needed to knit the manuscript.
#'
#' @export
extract_values_rmd <- function(){

  # write csv files of the raw estimate data
  areaResults <- qs::qread(here::here("_targets", "objects", "areaResults"))
  ssfResults <- qs::qread(here::here("_targets", "objects", "ssfResults"))
  wrsfResults <- qs::qread(here::here("_targets", "objects", "wrsfResults"))

  areaResults <- multiverseHabitat::parse_combined_results(areaResults)
  ssfResults <- multiverseHabitat::parse_combined_results(ssfResults)
  wrsfResults <- multiverseHabitat::parse_combined_results(wrsfResults)

  write.csv(areaResults, file = here::here("data", "areaResults.csv"))
  write.csv(ssfResults, file = here::here("data", "ssfResults.csv"))
  write.csv(wrsfResults, file = here::here("data", "wrsfResults.csv"))

  areaBrms_wides <- qs::qread(here::here("_targets", "objects", "areaBrms_wides"))
  areaBrms_rsf <- qs::qread(here::here("_targets", "objects", "areaBrms_rsf"))
  ssfBrms <- qs::qread(here::here("_targets", "objects", "ssfBrms"))
  wrsfBrms <- qs::qread(here::here("_targets", "objects", "wrsfBrms"))


# compile all R2 from all models ------------------------------------------
  # library(performance)
  # as.data.frame(wrsfBrms$modOUT_dEst_r2)
  # data.frame(
  #   R2 = c(unlist(
  #     wrsfBrms$modOUT_dEst_r2[1]),
  #     unlist(wrsfBrms$modOUT_dEst_r2[2])
  #     ),
  # SE = c(attr(wrsfBrms$modOUT_dEst_r2,"SE")$R2_Bayes,
  #        attr(wrsfBrms$modOUT_dEst_r2,"SE")$R2_Bayes_marginal),
  # rbind(attr(wrsfBrms$modOUT_dEst_r2,"CI")$R2_Bayes,
  #       attr(wrsfBrms$modOUT_dEst_r2,"CI")$R2_Bayes_marginal),
  # Component = c("conditional", "marginal")
  # )

  r2_allModels <- do.call(rbind, lapply(list(areaBrms_wides, areaBrms_rsf, ssfBrms, wrsfBrms),
                                        function(x){
    models <- paste0(sub("_.*$", "_", x[[1]][1]), x[[1]][3:4])
    r2df_1 <- data.frame(
      R2 = c(unlist(
        x[[4]][1]),
        unlist(x[[4]][2])
      ),
      SE = c(attr(x[[4]],"SE")$R2_Bayes,
             attr(x[[4]],"SE")$R2_Bayes_marginal),
      rbind(attr(x[[4]],"CI")$R2_Bayes,
            attr(x[[4]],"CI")$R2_Bayes_marginal),
      Component = c("conditional", "marginal")
    )
    # r2df_1 <- data.frame(x[[4]])
    r2df_1$model <- models[1]
    # r2df_2 <- data.frame(x[[5]])
    r2df_2 <- data.frame(
      R2 = c(unlist(
        x[[5]][1]),
        unlist(x[[5]][2])
      ),
      SE = c(attr(x[[5]],"SE")$R2_Bayes,
             attr(x[[5]],"SE")$R2_Bayes_marginal),
      rbind(attr(x[[5]],"CI")$R2_Bayes,
            attr(x[[5]],"CI")$R2_Bayes_marginal),
      Component = c("conditional", "marginal")
    )
    r2df_2$model <- models[2]
    r2_all <- rbind(r2df_1, r2df_2)
    return(r2_all)
  }))

  write.csv(r2_allModels, file = here::here("data", "brmsR2Results.csv"),
            row.names = FALSE)


# get all betas -----------------------------------------------------------

  modelBetas <- lapply(areaBrms_wides, function(x){
    if(class(x) == "brmsfit"){
      out <- ggdist::median_hdci(tidybayes::gather_draws(x,
                                                         `b_.*`, regex = TRUE),
                                 .width = c(0.95))
      return(out)
    } else (
      return(NULL)
    )
  })
  modelBetas$modOUT_dEst$model <- areaBrms_wides$modelNames[1]
  modelBetas$modOUT_rEst$model <- areaBrms_wides$modelNames[2]
  modelBetas[sapply(modelBetas, is.null)] <- NULL
  widesBetas <- do.call(rbind, modelBetas)

  modelBetas <- lapply(areaBrms_rsf, function(x){
    if(class(x) == "brmsfit"){
      out <- ggdist::median_hdci(tidybayes::gather_draws(x,
                                                         `b_.*`, regex = TRUE),
                                 .width = c(0.95))
      return(out)
    } else (
      return(NULL)
    )
  })
  modelBetas$modOUT_dEst$model <- areaBrms_rsf$modelNames[1]
  modelBetas$modOUT_rEst$model <- areaBrms_rsf$modelNames[2]
  modelBetas[sapply(modelBetas, is.null)] <- NULL
  rsfBetas <- do.call(rbind, modelBetas)

  modelBetas <- lapply(ssfBrms, function(x){
    if(class(x) == "brmsfit"){
      out <- ggdist::median_hdci(tidybayes::gather_draws(x,
                                                         `b_.*`, regex = TRUE),
                                 .width = c(0.95))
      return(out)
    } else (
      return(NULL)
    )
  })
  modelBetas$modOUT_dEst$model <- ssfBrms$modelNames[1]
  modelBetas$modOUT_rEst$model <- ssfBrms$modelNames[2]
  modelBetas[sapply(modelBetas, is.null)] <- NULL
  ssfBetas <- do.call(rbind, modelBetas)

  modelBetas <- lapply(wrsfBrms, function(x){
    if(class(x) == "brmsfit"){
      out <- ggdist::median_hdci(tidybayes::gather_draws(x,
                                                         `b_.*`, regex = TRUE),
                                 .width = c(0.95))
      return(out)
    } else (
      return(NULL)
    )
  })
  modelBetas$modOUT_dEst$model <- wrsfBrms$modelNames[1]
  modelBetas$modOUT_rEst$model <- wrsfBrms$modelNames[2]
  modelBetas[sapply(modelBetas, is.null)] <- NULL
  wrsfBrms <- do.call(rbind, modelBetas)

  brmEst_ext <- do.call(rbind, list(widesBetas, rsfBetas, ssfBetas, wrsfBrms))

  write.csv(brmEst_ext, file = here::here("data", "brmsEstResults.csv"),
            row.names = FALSE)

}

