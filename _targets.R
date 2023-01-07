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
  packages = c("qs", "here", "raster", "NLMR", "tibble",
               "multiverseHabitat",
               "amt", "adehabitatHR", "move"), # packages that your targets need to run
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
  # species = c("BADGER", "VULTURE", "KINGCOBRA")
  species = c("BADGER")
)
values_SimIndi <- tibble(
  individual = 1
  # individual = 1:4
  # individual = seq_len(30)
)

# values_SampDuration <- tibble(
#   # td = c(7)
#   td = c(7, 15, 30, 60)
#   # td = c(7, 15, 30, 60, 120, 240, 365)
# )
# values_SampFrequency <- tibble(
#   # tf = c(0.5)
#   tf = c(0.5, 1, 2, 6)
# )
values_Sampling <- tidyr::expand_grid(
  # td = c(7, 15, 30, 60),
  # tf = c(0.5, 1, 2, 6)
  # tf = c(0.5, 1.0, 2.0, 6.0, 12.0, 24.0, 48.0, 168.0),
  # td = c(7, 15, 30, 60, 120, 240, 365)
  td = c(7, 15),
  tf = c(0.5, 1)
)

# values_MethodArea <- tidyr::expand_grid(
#   areaMethod = c("MCP", "dBBMM"),
#   # areaMethod = c("MCP", "KDE_href", "AKDE", "dBBMM")
#   # areaContour = c(90)
#   areaContour = c(90, 95, 99)
# )
values_MethodArea <- tibble(
  areaMethod = c("MCP", "dBBMM"),
  # areaMethod = c("MCP", "KDEhref", "AKDE", "dBBMM")
)
values_MethodContour <- tidyr::expand_grid(
  areaContour = c(90)
  # areaContour = c(90, 95, 99)
)

values_MethodSSF <- tidyr::expand_grid(
  MethodSSF_mf = c("mf.is", "mf.ss"),
  # MethodSSF_ce = c("start", "end"),
  MethodSSF_ce = c("end"),
  # MethodSSF_as = as.integer(round(exp(seq(log(5), log(500), length.out = 5)), digits = 1))
  MethodSSF_as = c(5, 50)
)

values_MethodMethod <- tidyr::expand_grid( # Use all possible combinations of input settings.
  Method_function = rlang::syms(c("method_indi_wides", "method_indi_rsf")),
  Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 4)), digits = 1)),
  # Method_ap = as.integer(round(exp(seq(log(1), log(10), length.out = 2)), digits = 1)),
  Method_sp = c("rd", "st"),
  # Method_ap = 100,
  # Method_we = exp(seq(log(100), log(10000000), length.out = 3))
  Method_we = 1
)
# trim out the wieghting variation for when it is wides as that doesn't apply
values_MethodMethod <- values_MethodMethod[
  values_MethodMethod$Method_function == "method_indi_rsf" |
    (values_MethodMethod$Method_function == "method_indi_wides" &
       values_MethodMethod$Method_we == 1),]

values_MethodCTM <- tidyr::expand_grid(
  # Methodctm_ks = round(c(1, 1/2, 1/4, 1/16, 1/32), digits = 2),
  # Methodctm_it = c(24/60, 24/30, 24, 1),
  # Methodctm_pm = c("pm.c1", "pm.c2"),
  # Methodctm_im = c("im.li", "im.sp"),
  # Methodctm_di = c("di.ro", "di.ki")
  Methodctm_ks = 1,
  Methodctm_it = 24/60,
  Methodctm_pm = "pm.c1",
  Methodctm_im = "im.li",
  Methodctm_di = "di.ro"
)

targetsList <- list(
  ## LANDSCAPE SIMULATION
  tar_map(
    values = values_SimSpecies,
    tar_target(landscape, simulate_landscape(species, 2022)), # FUNCTION simulate_landscape

    ## INDIDIVUAL SIMULATION
    tar_map(
      values = values_SimIndi,
      tar_target(simData, simulate_individual(
        individualNum = individual,
        species = species,
        simSteps = 24*60 *365,
        desOptions = 10,
        options = 12,
        landscape,
        seed = 2022)), # FUNCTION simulate_individual

      ## DURATION MAP
      tar_map(
        values = values_Sampling,
        tar_target(sampDuraFreqData,
                   subset_duration(
                     movementData = subset_frequency(movementData = simData$locations,
                                                     freqPreset = tf),
                     daysDuration = td),
                   priority = 0.92), # FUNCTION subset_duration
        ## FREQUENCY MAP
        # tar_map(
        #   values = values_SampFrequency,
        #   tar_target(sampDuraFreqData,
        #              subset_frequency(movementData = sampDuraData,
        #                               freqPreset = tf)), # FUNCTION subset_frequency
        ## AREA + CONTOUR MAP
        tar_map(
          values = values_MethodArea,
          tar_target(area,
                     build_available_area(movementData = sampDuraFreqData,
                                          method = areaMethod,
                                          SRS_string = "EPSG:32601",
                                          dBBMMsettings = c(168, 48)),
                     priority = 0.9), # FUNCTION build_available_area
          tar_map(
            values = values_MethodContour,
            tar_target(polygon,
                       build_available_polygon(areaResource = area,
                                               method = areaMethod,
                                               contour = areaContour,
                                               SRS_string = "EPSG:32601"),
                       priority = 0.91), # FUNCTION build_available_area
            ## AREA-BASED METHODS MAP
            tar_map(
              values = values_MethodMethod,
              # names = "tNames", # Select columns from `values` for target names.
              tar_target(methOUT, Method_function(movementData = sampDuraFreqData,
                                                  landscape = landscape,
                                                  spSamp = Method_sp,
                                                  availableArea = polygon,
                                                  availablePoints = Method_ap,
                                                  weighting = Method_we)) # FUNCTION "method_indi_wides", "method_indi_rsf"
              # tar_target(methEstimate, extract_estimate(methOUT))

              # next level goes here using tar_map() again
            ) # area methods map
          ) # contour map
        ), # area creation map
        ## SSF MAP
        tar_map(
          values = values_MethodSSF,
          tar_target(ssfOUT, method_indi_ssf(movementData = sampDuraFreqData,
                                             landscape = landscape,
                                             methodForm = MethodSSF_mf,
                                             covExtract = MethodSSF_ce,
                                             availableSteps = MethodSSF_as)) # FUNCTION method_ssf
          # tar_target(ssfEstimate, extract_estimate(ssfOUT))
        ) # ssf map
        # ## CTMC MAP
        # tar_map(
        #   values = values_MethodCTM,
        #   tar_target(ctmcOUT, paste0(sampDuraFreqData, "_-_ctm1_", Methodctm_ks,
        #                           Methodctm_it, Methodctm_pm,
        #                           Methodctm_im, Methodctm_di))
        # ) # ctmc map
        # ) # frequency map
      ) # duration map
    ) # individual map
  ) # species map
)

# tar_combine(
#   name = resultsList,
#   methOUT,
#   values_MethodSSF,
#   command = list(!!!.x)
# )

# targetsList[[1]][grep("OUT", names(targetsList[[1]]))]
resultsCompiled <- tar_combine(
  combinedResults,
  targetsList[[1]][grep("OUT", names(targetsList[[1]]))],
  # command = list(!!!.x)
  command = rbind(!!!.x),
  priority = 0
)
list(targetsList, resultsCompiled)

# Launch the app in a background process.
# tar_watch(seconds = 60, outdated = FALSE, targets_only = TRUE)

# preview one species
# targets::tar_visnetwork(allow = contains("badger"))
# targets::tar_visnetwork()
# targets::tar_manifest()
# targets::tar_make()
# watch out too many workers can hit ram limits
# targets::tar_make_clustermq(workers = 12, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 12, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 18, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 18, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 20, log_worker = TRUE)
# targets::tar_make_clustermq(workers = 20, reporter = "verbose_positives", log_worker = TRUE)
# targets::tar_make_clustermq(workers = 12, reporter = "summary", log_worker = TRUE)


# endPointsAndNodes <- targets::tar_network(targets_only = TRUE)
