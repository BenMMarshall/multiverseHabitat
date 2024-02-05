#' Generate files needed for RMD manuscript
#'
#' @name extract_values_rmd
#' @importFrom magrittr %>%
#' @description Extract the required values to report in the RMD file.
#' @return Will save a series of csv files containing the raw estimates and
#'   model betas that are needed to knit the manuscript.
#'
#' @export
extract_values_rmd <- function(areaResults, ssfResults, wrsfResults,
                               brmModels){

  # write csv files of the raw estimate data
  # areaResults <- qs::qread(here::here("_targets", "objects", "areaResults"))
  # ssfResults <- qs::qread(here::here("_targets", "objects", "ssfResults"))
  # wrsfResults <- qs::qread(here::here("_targets", "objects", "wrsfResults"))

  areaResults <- multiverseHabitat::parse_combined_results(areaResults)
  ssfResults <- multiverseHabitat::parse_combined_results(ssfResults)
  wrsfResults <- multiverseHabitat::parse_combined_results(wrsfResults)

  write_csv(areaResults, file = here::here("data", "areaResults.csv.gz"))
  write_csv(ssfResults, file = here::here("data", "ssfResults.csv.gz"))
  write_csv(wrsfResults, file = here::here("data", "wrsfResults.csv.gz"))

  write_csv(areaResults[1:2,1:2], file = here::here("data", "areaResults.csv"))
  write_csv(ssfResults[1:2,1:2], file = here::here("data", "ssfResults.csv"))
  write_csv(wrsfResults[1:2,1:2], file = here::here("data", "wrsfResults.csv"))

  # areaBrms_wides <- qs::qread(here::here("_targets", "objects", "areaBrms_wides"))
  # areaBrms_rsf <- qs::qread(here::here("_targets", "objects", "areaBrms_rsf"))
  # ssfBrms <- qs::qread(here::here("_targets", "objects", "ssfBrms"))
  # wrsfBrms <- qs::qread(here::here("_targets", "objects", "wrsfBrms"))


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
  r2_allModels <- do.call(rbind, lapply(brmModels, function(x){

    model <- names(x)[1]
    r2df_1 <- data.frame(
      model = model,
      R2 = unlist(
        x[[2]]),
      SE = c(attr(x[[2]],"SE")$R2_Bayes,
             attr(x[[2]],"SE")$R2_Bayes_marginal),
      rbind(attr(x[[2]],"CI")$R2_Bayes,
            attr(x[[2]],"CI")$R2_Bayes_marginal),
      Component = c("conditional", "marginal")
    )
    return(r2df_1)
  }))

  write.csv(r2_allModels, file = here::here("data", "brmsR2Results.csv"),
            row.names = FALSE)


  # get all betas -----------------------------------------------------------

  brmEst_ext <- do.call(rbind, lapply(brmModels, function(x){
    for(i in 1:length(x)){
      if(class(x[[i]]) == "brmsfit"){
        out <- ggdist::median_hdci(tidybayes::gather_draws(x[[i]],
                                                           `b_.*`, regex = TRUE),
                                   .width = c(0.95))
        out$model <- names(x)[1]
        return(out)
      } else (
        return(NULL)
      )
    }
  })
  )

  write.csv(brmEst_ext, file = here::here("data", "brmsEstResults.csv"),
            row.names = FALSE)

  extractedList <- list(
    "r2Outputs" = r2_allModels,
    "betasOutputs" = brmEst_ext)

  return(extractedList)

}

