### CODE STRUCTURE / WORKFLOW

# Simulate landscapes - store outputs, 3, 1 for each species

# Simulate population - 200? individuals, store all as separate RData files

# Sampling - each individual gets all sampling nodes completed on them, so we
# have 200 x node1 x node2 data files

# Generate areas - as the areas will be reused and are likely a more
# computationally intensive bit we can run 90,95,99 contours or each method for
# each data file. store all outputs

# Note - do we need to explore buffers on each area method? see amt vignette

# Individual analyses - run SSF RSF and Wides designs

# Population analyses - summarise SSF RSF and WIDES to pop level, then introduce
# popSSF, Compana and Eisera. The summaries and pop analyses will need varying
# sample sizes but that should be easy because the outputs will be stored.
# Questions over repeats? but will be computationally low cos the areas etc are
# already calculated and tracking data already sub sampled.

## Overall this plan will use up loads of harddrive space, but will save
## computational time. Maximise the data shared at the end, perhaps, polygons
## and original sim data as those will likely take the longest to run.
## Subsampling is more a convenience for later summaries and makes things nicely
## clear for running analysis functions, it won't save much time besides not
## needing to load in the largest tracking dataset files.

### To do

# - draft individual preference methods
#   - wides - draft done
#   - ssf - draft done
#   - rsf - draft done - need to add new ctmm implementation
#   - ctmc
# - function-ise individual preference methods
#   - wides - done, pending documentation
#   - ssf - done, pending documentation
#   - rsf - done, pending documentation
#   - ctmc
# - draft population preference methods
#   - compana
#   - eisera
#   - pop-ssf via INLA
#   - ??? rsf w/ random effect
# - function-ise population preference methods
#   - compana
#   - eisera
#   - pop-ssf via INLA
#   - ??? rsf w/ random effect

# - formally construct Targets tree(s) - mostly done bar ctmc, but needs testing

# - create stable data objects that contain the species sim data - done
#   - include landscape sim values in data object - done
