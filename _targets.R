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
  packages = c("here", "raster", "NLMR", "tibble",
               "multiverseHabitat"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multiprocess")

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

## every split requires it's own tibble, otherwise it's applied to all of them
values <- tibble(
  species = c("badger", "vulture", "king cobra")
)
values2 <- tibble(
  td = c("td1", "td2")
)
values3 <- tibble(
  tf = c("td0.1", "td0.2")
)
values4 <- tibble(
  areaMethod = c("dBBMM", "AKDE")
)
# values5 <- tibble(
#   ssfMethod = c("mf.is", "mf.ss")
# )
values5 <- tidyr::expand_grid(
  ssfMethodmf = c("mf.is", "mf.ss"),
  ssfMethodce = c("ce.st", "ce.mi", "ce.ed"),
)

methodsValues <- tidyr::expand_grid( # Use all possible combinations of input settings.
  method_function = rlang::syms(c("ade.wid", "amt.rsf"))
)

list(
  tar_map(
    values = values,
    # in the final sim function wrapper make sure there is an if statement to
    # get all the values per species, easier there than here, can have extra
    # landscape target that is indepedent?
    tar_target(landscape, paste0("l_", species)),
    tar_target(simdata, paste0("d_", species, "__", landscape)),
    # tar_target(sampdata, paste0(simdata, "_-_samp_", td))
    tar_map(
      values = values2,
      tar_target(sampdata, paste0(simdata, "_-_samp_", td)),
      tar_map(
        values = values3,
        tar_target(sampdata2, paste0(sampdata, "_-_samp2_", tf)),
        tar_target(outputctmc, paste0(sampdata2, "_-_ctmc")), # quick to output function cos doesnt need areas
        # tar_target(outputssf, paste0(sampdata2, "_-_ssf")), # quick to output function cos doesnt need areas
        ## can we swap the above for tar_maps can create sub trees for these
        ## routes? do we need to? or do we bake the method nodes into the
        ## functions. If not we need "values" for every node in teh targets.R
        ## file here
        ## area based branching
        tar_map(
          values = values4,
          tar_target(area, paste0(sampdata2, "_-_area_", areaMethod)),
          tar_map(
            values = methodsValues,
            # names = "tNames", # Select columns from `values` for target names.
            tar_target(output, method_function(sampdata2, area))
            # next level goes here using tar_map() again
          )),
        ## ssf branching
        tar_map(
          values = values5,
          tar_target(ssf1, paste0(sampdata2, "_-_ssf1_", ssfMethodmf,
                                  "_ssf2_", ssfMethodce))
        )
      )
    )
  )
)

# preview one species
# targets::tar_visnetwork(allow = contains("badger"))
