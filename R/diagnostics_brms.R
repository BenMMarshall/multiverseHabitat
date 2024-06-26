#' Generate plots to assess brms
#'
#' @name diagnostics_brms
#' @description A
#' @param brmsResults output tar_targets resulting in areaBrms or ssfBrms
#' @return a
#'
#' @export
diagnostics_brms <- function(brmsResults, ...){

  for(n in 1:length(brmsResults)){

    name <- names(brmsResults)[1]
    currMod <- brmsResults[[n]]

    if(class(currMod) == "brmsfit"){

      vars <- get_variables(currMod)
      varsToPlot <- vars[stringr::str_detect(vars, "b_")]

      traceplot <- mcmc_trace(currMod, pars = varsToPlot)
      ggsave(traceplot,
             filename = here("notebook", "modelOutput", paste0(name, "_traceplot.png")),
             dpi = 300, width = 210, height = 140,
             units = "mm")

      acfplot <- mcmc_acf(currMod, pars = varsToPlot)
      ggsave(acfplot,
             filename = here("notebook", "modelOutput", paste0(name, "_acfplot.png")),
             dpi = 300, width = 210, height = 140,
             units = "mm")
    }
  }

}
