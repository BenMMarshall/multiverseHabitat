# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed. # nolint
library(tibble)

# Set target options:
tar_option_set(
  packages = c("qs", "here", "raster", "NLMR", "tibble", "dplyr", "stringr",
               "multiverseHabitat",
               "amt", "adehabitatHR", "move",
               "brms", "bayesplot", "tidybayes", "performance",
               "ggplot2", "ggridges", "reshape2", "patchwork", "ggtext",
               "kableExtra", "readr"), # packages that your targets need to run
  garbage_collection = TRUE,
  format = "qs", # storage format
  storage = "worker",
  retrieval = "worker",
  memory = "transient" # avoid ram issues
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
# options(clustermq.scheduler = "multiprocess") # recommended
options(clustermq.scheduler = "multicore")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

## every split requires it's own tibble, otherwise it's applied to all of them
values_SimSpecies <- tibble(
  species = c("BADGER", "VULTURE", "KINGCOBRA")
  # species = c("BADGER")
)
values_SimIndi <- tibble(
  # individual = 1
  individual = 1:4
  # individual = seq_len(30)
)

values_Sampling <- tidyr::expand_grid(
  # td = c(7, 15, 30, 60),
  # tf = c(0.5, 1, 2, 6)
  # td = c(7, 15, 30, 60, 120, 240, 365),
  td = c(7, 15, 30, 60, 120, 240),
  tf = c(0.5, 1.0, 2.0, 6.0, 12.0, 24.0, 48.0, 168.0)
  # td = c(7, 15),
  # tf = c(0.5, 1)
)
# have to filter out certain combos that have too little data to work with
values_Sampling <- values_Sampling %>%
  dplyr::mutate(datapoints = td*24 * (1/tf)) %>%
  dplyr::filter(datapoints > 30) %>%
  dplyr::select(td, tf)

optionsList_area <- list(
  # Method_method = c("wides", "rsf", "wRSF"),
  Method_method = c("wides", "rsf"),
  Method_lc = c("classRaster", "classRasterScram"),
  areaMethod = c("MCP", "KDEhref", "AKDE", "dBBMM"),
  areaContour = c(90, 95, 99),
  Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 4)), digits = 1)),
  # Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 2)), digits = 1)),
  Method_sp = c("rd", "st"),
  # Method_ap = 100,
  Method_we = exp(seq(log(1), log(10000000), length.out = 6))
  # Method_we = 1
)

optionsList_area_wRSFSplit <-
  list(
    Method_method = c("wRSF"),
    Method_lc = c("classRaster", "classRasterScram"),
    areaMethod = c("AKDE"),
    areaContour = c(95),
    Method_ap = 999,
    Method_sp = "rd",
    Method_we = 999
  )

optionsList_sff <- list(
  Method_method = c("ssf"),
  Method_lc = c("classRaster", "classRasterScram"),
  MethodSSF_mf = c("mf.is", "mf.ss"),
  MethodSSF_sd = c("gamma", "exp"),
  MethodSSF_td = c("vonmises", "unif"),
  MethodSSF_as = as.integer(round(exp(seq(log(5), log(500), length.out = 5)), digits = 1))
)


# values_MethodCTM <- tidyr::expand_grid(
#   # Methodctm_ks = round(c(1, 1/2, 1/4, 1/16, 1/32), digits = 2),
#   # Methodctm_it = c(24/60, 24/30, 24, 1),
#   # Methodctm_pm = c("pm.c1", "pm.c2"),
#   # Methodctm_im = c("im.li", "im.sp"),
#   # Methodctm_di = c("di.ro", "di.ki")
#   Methodctm_ks = 1,
#   Methodctm_it = 24/60,
#   Methodctm_pm = "pm.c1",
#   Methodctm_im = "im.li",
#   Methodctm_di = "di.ro"
# )

optionsCompleteList <- list(
  "species" = values_SimSpecies,
  "individuals" = values_SimIndi,
  "regime" = values_Sampling,
  "area" = optionsList_area,
  "wrsf" = optionsList_area_wRSFSplit,
  "ssf" = optionsList_sff
)

saveRDS(optionsCompleteList, file = here::here("data", "optionsCompleteList.rds"))

set.seed(2022)

allIndividualEstimatesList <- list(
  ## LANDSCAPE SIMULATION
  tar_map(
    values = values_SimSpecies,
    tar_target(landscape, simulate_landscape(species, seed = 2022)), # FUNCTION simulate_landscape

    ## INDIDIVUAL SIMULATION
    tar_map(
      values = values_SimIndi,
      tar_target(simData, simulate_individual(
        individualNum = individual,
        species = species,
        simSteps = 24*60 *365,
        desOptions = 12,
        options = 15,
        landscapeList = landscape,
        seed = 2022),
        priority = 0.93), # FUNCTION simulate_individual

      ## DURATION + FREQUENCY MAP
      tar_map(
        values = values_Sampling,
        tar_target(sampDuraFreqData,
                   subset_duration(
                     movementData = subset_frequency(movementData = simData$locations,
                                                     freqPreset = tf),
                     daysDuration = td),
                   priority = 0.92),
        ## AREA-BASED METHODS
        tar_target(areaMethodsOUT,
                   wrapper_indi_area(
                     movementData = sampDuraFreqData,
                     landscape = landscape,
                     optionsList = optionsList_area
                   ),
                   priority = 0.91),
        ## WRSF
        tar_target(wrsfMethodsOUT,
                   wrapper_indi_area(
                     movementData = sampDuraFreqData,
                     landscape = landscape,
                     optionsList = optionsList_area_wRSFSplit
                   ),
                   priority = 0.01),
        ## SSF
        tar_target(ssfOUT,
                   wrapper_indi_ssf(
                     movementData = sampDuraFreqData,
                     landscape = landscape,
                     optionsList = optionsList_sff
                   ),
                   priority = 0.9)
      ) # duration map
    ) # individual map
  ) # species map
) # list

areaCompiled <- tar_combine(
  areaResults,
  allIndividualEstimatesList[[1]][grep("areaMethodsOUT", names(allIndividualEstimatesList[[1]]))],
  # command = list(!!!.x),
  command = rbind(!!!.x),
  priority = 0.8
)
wrsfCompiled <- tar_combine(
  wrsfResults,
  allIndividualEstimatesList[[1]][grep("wrsfMethodsOUT", names(allIndividualEstimatesList[[1]]))],
  # command = list(!!!.x),
  command = rbind(!!!.x),
  priority = 0.1
)
ssfCompiled <- tar_combine(
  ssfResults,
  allIndividualEstimatesList[[1]][grep("ssfOUT", names(allIndividualEstimatesList[[1]]))],
  # command = list(!!!.x),
  command = rbind(!!!.x),
  priority = 0.8
)

simsCompiled <- tar_combine(
  simResults,
  allIndividualEstimatesList[[1]][grep("simData", names(allIndividualEstimatesList[[1]]))],
  command = list(!!!.x),
  # command = rbind(!!!.x),
  priority = 0.8
)


directCompiled <- list(
  tar_target(directEstimates, direct_estimates(simResults),
             priority = 0.99)
)

method_BRMS <- tibble(
  # method = c("rsf", "wides"),
  resp = c("abs", "raw")
)

brmsCompiled <- list(
  # area method models
  tar_map(
    values = method_BRMS,
    # RSF
    tar_target(rsfBrmsModel,
               run_brms(
                 compiledResults = areaResults,
                 method = "rsf",
                 response = resp,
                 iter = 500,
                 warmup = 150),
               priority = 0.71
    ),
    # WIDES
    tar_target(widesBrmsModel,
               run_brms(
                 compiledResults = areaResults,
                 method = "wides",
                 response = resp,
                 iter = 1500,
                 warmup = 500),
               priority = 0.71
    ),
    # SSF
    tar_target(ssfBrmsModel,
               run_brms(
                 compiledResults = ssfResults,
                 method = "ssf",
                 response = resp,
                 iter = 1500,
                 warmup = 250),
               priority = 0.72
    ),
    # WRSF
    tar_target(wrsfBrmsModel,
               run_brms(
                 compiledResults = wrsfResults,
                 method = "wrsf",
                 response = resp,
                 iter = 2000,
                 warmup = 500),
               priority = 0.71
    ),
    # EFFECT SIZES
    tar_target(effectSizeBrms_rsf,
               generate_effect_plots(
                 brmsResults = rsfBrmsModel,
                 method = "rsf",
                 response = resp
               ),
               priority = 0.74
    ),
    tar_target(effectSizeBrms_wides,
               generate_effect_plots(
                 brmsResults = widesBrmsModel,
                 method = "wides",
                 response = resp
               ),
               priority = 0.74
    ),
    tar_target(effectSizeBrms_ssf,
               generate_effect_plots(
                 brmsResults = ssfBrmsModel,
                 method = "ssf",
                 response = resp
               ),
               priority = 0.74
    ),
    tar_target(effectSizeBrms_wrsf,
               generate_effect_plots(
                 brmsResults = wrsfBrmsModel,
                 method = "wrsf",
                 response = resp
               ),
               priority = 0.70
    ),
    # SUMMARY DIAGNOSTICS
    tar_target(summaryBrms_rsf,
               diagnostics_brms(
                 brmsResults = rsfBrmsModel,
                 response = resp
               ),
               priority = 0.71
    ),
    tar_target(summaryBrms_wides,
               diagnostics_brms(
                 brmsResults = widesBrmsModel,
                 response = resp
               ),
               priority = 0.71
    ),
    tar_target(summaryBrms_ssf, ## added _ssf to match name style of the area methods
               diagnostics_brms(
                 brmsResults = ssfBrmsModel,
                 response = resp
               ),
               priority = 0.73
    ),
    tar_target(summaryBrms_wrsf, ## added _ssf to match name style of the area methods
               diagnostics_brms(
                 brmsResults = wrsfBrmsModel,
                 response = resp
               ),
               priority = 0.70
    )
  ),
  tar_target(rsfSpecCurves,
             generate_spec_curves(
               compiledResults = areaResults,
               method = "rsf"),
             priority = 0.71
  ),
  tar_target(widesSpecCurves,
             generate_spec_curves(
               compiledResults = areaResults,
               method = "wides"),
             priority = 0.71
  ),
  tar_target(ssfSpecCurves,
             generate_spec_curves(
               compiledResults = ssfResults,
               method = "ssf"
             ),
             priority = 0.72
  ),
  tar_target(wrsfpecCurves,
             generate_spec_curves(
               compiledResults = wrsfResults,
               method = "wrsf"
             ),
             priority = 0.71
  ),
  tar_target(uncertaintyPlot,
             uncertainty_vs_estimate(
               aResults = areaResults,
               sResults = ssfResults,
               wResults = wrsfResults
             ),
             priority = 0.74
  )
)

brmAllModels <- tar_combine(
  brmModels,
  brmsCompiled[[1]][grep("BrmsModel", names(brmsCompiled[[1]]))],
  command = list(!!!.x),
  # command = rbind(!!!.x),
  priority = 0.8
)

extrasForRMD <- list(
  tar_target(
    extractedValues,
    multiverseHabitat::extract_values_rmd(areaResults, ssfResults, wrsfResults,
                                          brmModels)
  ),
  tar_target(
    allEffectPlot,
    generate_allEffect_plots(modelExtracts = extractedValues)
  ),
  tar_target(
    iteractionPlot,
    multiverseHabitat::generate_iteraction_plot()
  ),
  tar_target(
    landscapeExamplePlot,
    multiverseHabitat::plot_landscape_example()
  ),
  tar_target(
    rmdRender,
    render_rmd(extractedValues,
               effectPlots,
               allEffectPlot,
               iteractionPlot,
               landscapeExamplePlot,
               uncertaintyPlot,
               areaSpecCurves_wides,
               areaSpecCurves_rsf,
               ssfSpecCurves),
    cue = tar_cue(mode = "always"),
    priority = 0.1
  )
)

list(allIndividualEstimatesList,
     areaCompiled,
     wrsfCompiled,
     ssfCompiled,
     simsCompiled,
     brmsCompiled,
     brmAllModels,
     directCompiled,
     extrasForRMD)


# Launch the app in a background process.
# tar_watch(seconds = 60, outdated = FALSE, targets_only = TRUE)

# preview one species
# targets::tar_visnetwork(allow = contains("BADGER"))
# targets::tar_visnetwork(allow = contains("7_6_3"))
# targets::tar_visnetwork()
# targets::tar_manifest()
# targets::tar_make()
# targets::tar_make("effectSizeBrms_ssf")
# watch out too many workers can hit ram limits
# targets::tar_make_clustermq(workers = 2, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 8, log_worker = TRUE)
# targets::tar_make_clustermq("wrsfResults", workers = 6, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 12, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 12, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 18, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 20, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 20, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 12, reporter = "summary", log_worker = TRUE)

# warnings <- targets::tar_meta(fields = warnings, complete_only = TRUE)

# endPointsAndNodes <- targets::tar_network(targets_only = TRUE)
