# multiverseHabitat

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.6951937.svg)](https://doi.org/10.5281/zenodo.12169335)

--------------------------------------------------------------------------------

The files included in this *multiverseHabitat* repository were used to generate the results for a manuscript entitled: "A Habitat Selection Multiverse Reveals Largely Consistent Results Despite a Multitude of Analysis Options" by Benjamin Michael Marshall and Alexander Brad Duthie.
The code and targets workflow include the necessary components to simulate the data and run all analyses described in the manuscript.

--------------------------------------------------------------------------------

To re-run this project's workflow, ensure all packages listed in the `_targets.R` file are installed.
Then use targets package functions `tar_make()` or `tar_make_clustermq()`, as seen in the `run.R` file.
The workflow is slow to complete and at times can be memory intensive.

The `R` folder contains all custom functions used in the generation of data and the multiverse analysis.
The `_targets` folder stores all intermediate objects that are created during the running of the workflow, as described by the `_targets.R` file.
The `data` folder contains outputted habitat preference estimates, as well as the results from the Bayesian regression models.
The `notebook` folder contains external images, figures, and model outputs used in the manuscript describing the results along with the manuscript drafts.
`notebook` also contains notes and miscellaneous drafts that is not used in the final workflow.
