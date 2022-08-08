# install.packages("data.tree")
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#tree-creation
library(data.tree)

multiverseTree <- Node$new("multiverseTree")
species1 <- multiverseTree$AddChild("species1")
species2 <- multiverseTree$AddChild("species2")
species3 <- multiverseTree$AddChild("species3")

print(multiverseTree)

## decision vectors

### Sampling
samp_01_species <- c("species1_Badger", "species1_Vulture", "species1_Kingcobra")
samp_02_trackingFreq <- c("0.5hrs", "1hrs", "2hrs", "6hrs", "12hrs", "24hrs", "168hrs")
samp_03_trackingDura <- c("7days", "15days", "30days", "60days", "120days", "240days", "365days")

# make sure all combinations are in the tree
samp_nodeDF <- expand.grid(sapply(
  ls(pattern = "samp_[0-9]{2}"),
  FUN = get,
  simplify = FALSE, USE.NAMES = TRUE),
  stringsAsFactors = FALSE)
cols <- names(samp_nodeDF)
samp_nodeDF$pathString <- do.call(paste, c("multiverseTree", samp_nodeDF[cols], sep="/"))
samp_tree <- as.Node(samp_nodeDF)

# plot(as.dendrogram(samp_tree), center = TRUE)

### Analysis

habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera", "amt_HSF", "amt_SSF", "ctmc_ctmc")

designType <- c("II", "III") # applies to first four methods
# for first four methods
avaiableAreas <- c("MCP", "KDEhref", "KDElscv", "AKDEbest", "dBBMM")
# for first four methods
availablePoints <- exp(seq(log(1), log(1000), length.out = 4)) # high enough would approximate true available %

# hsf only
hsf_01_Weighting <- exp(seq(log(1), log(10000), length.out = 5))

# ssf only
ssf_01_modelForm <- c("ISSF", "SSF")
ssf_02_covariateExtract <- c("start", "middle", "end")
ssf_03_availableSteps <- exp(seq(log(1), log(500), length.out = 5))

# ctmcmove only
# varying the spacing of the knots
ctmc_01_knotSpacing <- c(1, 1/2, 1/4, 1/16, 1/32)
# define the sequence of times on which to sample the imputed path
ctmc_02_imputeTimes <- c(1/24/60, 1/24/30, 1/24, 1)

ctmc_03_interpMethod <- c("LinearInterp", "ShortestPath")

ctmc_04_precisionMat <- c("CAR1", "CAR2")

# make sure all combinations are in the tree
ctmc_nodeDF <- expand.grid(sapply(
  c(ls(pattern = "samp_[0-9]{2}"),
    ls(pattern = "ctmc_[0-9]{2}")),
  FUN = get,
  simplify = FALSE, USE.NAMES = TRUE),
  stringsAsFactors = FALSE)

ctmc_nodeDF$habitatMethod <- "ctmc_ctmc"

cols <- names(ctmc_nodeDF)
## reorder to ensure that the method choice is between sampl and method specific choices
cols <- c(cols[grep(cols, pattern = "samp_[0-9]{2}")],
          "habitatMethod",
          cols[grep(cols, pattern = "ctmc_[0-9]{2}")])

ctmc_nodeDF$pathString <- do.call(paste, c("multiverseTree", ctmc_nodeDF[cols], sep="/"))

ctmc_tree <- as.Node(ctmc_nodeDF)
print(ctmc_tree)

ctmc_tree$species1_Badger$`0.5hrs`$`7days`$AddChild("ade_wides")

# plot(as.dendrogram(samp_tree), center = TRUE)

# list attempts -----------------------------------------------------------

vec2NamedList <- function(INVECTOR){
  OUTLIST <- vector("list", length = length(INVECTOR))
  names(OUTLIST) <- paste0(deparse(substitute(INVECTOR)), "___", INVECTOR)
  return(OUTLIST)
}

### Sampling
samp_01_species <- c("species1_Badger", "species1_Vulture", "species1_Kingcobra")
samp_01list_species <- vec2NamedList(samp_01_species)
samp_02_trackingFreq <- c("0.5hrs", "1hrs", "2hrs", "6hrs", "12hrs", "24hrs", "168hrs")
samp_02list_trackingFreq <- vec2NamedList(samp_02_trackingFreq)
samp_03_trackingDura <- c("7days", "15days", "30days", "60days", "120days", "240days", "365days")
samp_03list_trackingDura <- vec2NamedList(samp_03_trackingDura)

method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera",
                                "amt_HSF", "amt_SSF",
                                "ctmc_ctmc")
method_01list_habitatMethod <- vec2NamedList(method_01_habitatMethod)

# samp_list <-
#   lapply(samp_01list_species, function(x){
#     lapply(samp_02list_trackingFreq, function(x){
#       lapply(samp_03list_trackingDura, function(x){
#         lapply(method_01list_habitatMethod, function(x){
#           NA
#         })
#       })
#     })
#   })
# str(samp_list)
# samp_tree <- as.Node(samp_list)
# plot(as.dendrogram(samp_tree), center = TRUE)


method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera")
method_01list_habitatMethod <- vec2NamedList(method_01_habitatMethod)

avail_02_designType <- c("II", "III") # applies to first four methods
avail_02list_designType <- vec2NamedList(avail_02_designType)
avail_03_avaiableAreas <- c("MCP", "KDEhref", "KDElscv", "AKDEbest", "dBBMM") # for first four methods
avail_03list_avaiableAreas <- vec2NamedList(avail_03_avaiableAreas)
avail_04_availablePointsMultiplier <- exp(seq(log(1), log(1000), length.out = 4)) # high enough would approximate true available %
avail_04list_availablePointsMultiplier <- vec2NamedList(avail_04_availablePointsMultiplier)

adeMethods_list <-
  lapply(method_01list_habitatMethod, function(x){
    lapply(avail_02list_designType, function(x){
      lapply(avail_03list_avaiableAreas, function(x){
        lapply(avail_04list_availablePointsMultiplier, function(x){
          "Result"
        })
      })
    })
  })

# hsf only
hsf_01_weighting <- exp(seq(log(1), log(10000), length.out = 5))
hsf_01list_weighting <- vec2NamedList(hsf_01_weighting)

hsfMethods_list <-
  list("method_01list_habitatMethod___amt_HSF" =
         lapply(avail_02list_designType, function(x){
           lapply(avail_03list_avaiableAreas, function(x){
             lapply(avail_04list_availablePointsMultiplier, function(x){
               lapply(hsf_01list_weighting, function(x){
                 "Result"
               })
             })
           })
         })
  )
str(hsfMethods_list)

# ssf only
ssf_01_modelForm <- c("ISSF", "SSF")
ssf_01list_modelForm <- vec2NamedList(avail_02_designType)
ssf_02_covariateExtract <- c("start", "middle", "end")
ssf_02list_covariateExtract <- vec2NamedList(ssf_02_covariateExtract)
ssf_03_availableSteps <- exp(seq(log(1), log(500), length.out = 5))
ssf_03list_availableSteps <- vec2NamedList(ssf_03_availableSteps)

ssfMethods_list <-
  list("method_01list_habitatMethod___amt_SSF" =
         lapply(ssf_01list_modelForm, function(x){
           lapply(ssf_02list_covariateExtract, function(x){
             lapply(ssf_03list_availableSteps, function(x){
               "Result"
             })
           })
         })
  )
str(ssfMethods_list)

# ctmcmove only
# varying the spacing of the knots
ctmc_01_knotSpacing <- c(1, 1/2, 1/4, 1/16, 1/32)
ctmc_01list_knotSpacing <- vec2NamedList(ctmc_01_knotSpacing)
# define the sequence of times on which to sample the imputed path
ctmc_02_imputeTimes <- c(1/24/60, 1/24/30, 1/24, 1)
ctmc_02list_imputeTimes <- vec2NamedList(ctmc_02_imputeTimes)

ctmc_03_interpMethod <- c("LinearInterp", "ShortestPath")
ctmc_03list_interpMethod <- vec2NamedList(ctmc_03_interpMethod)

ctmc_04_precisionMat <- c("CAR1", "CAR2")
ctmc_04list_precisionMat <- vec2NamedList(ctmc_04_precisionMat)

ctmcMethods_list <-
  list("method_01list_habitatMethod___ctmc_ctmc" =
         lapply(ctmc_01list_knotSpacing, function(x){
           lapply(ctmc_02list_imputeTimes, function(x){
             lapply(ctmc_03list_interpMethod, function(x){
               lapply(ctmc_04list_precisionMat, function(x){
                 "Result"
               })
             })
           })
         })
  )
str(ctmcMethods_list)


# Combine all to the sample list ------------------------------------------

method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera",
                                "amt_HSF", "amt_SSF",
                                "ctmc_ctmc")
method_01list_habitatMethod <- vec2NamedList(method_01_habitatMethod)
method_01list_habitatMethod <- mapply(function(x, y){x = paste0("method_01list_habitatMethod___", y)},
                                      method_01list_habitatMethod, method_01_habitatMethod)

fullMultiverse_list <-
  lapply(samp_01list_species, function(x){
    lapply(samp_02list_trackingFreq, function(x){
      lapply(samp_03list_trackingDura, function(x){
        lapply(method_01list_habitatMethod, function(x){
          if(x %in% c(
            "method_01list_habitatMethod___ade_Wides",
            "method_01list_habitatMethod___ade_Compana",
            "method_01list_habitatMethod___ade_Eisera")
          ){
            adeMethods_list[[sub("list", "", x)]]
          } else if(x == "method_01list_habitatMethod___amt_HSF"){
            hsfMethods_list$method_01list_habitatMethod___amt_HSF
          } else if(x == "method_01list_habitatMethod___amt_SSF"){
            ssfMethods_list$method_01list_habitatMethod___amt_SSF
          } else if(x == "method_01list_habitatMethod___ctmc_ctmc"){
            ctmcMethods_list$method_01list_habitatMethod___ctmc_ctmc
          }
        })
      })
    })
  })
# str(fullMultiverse_list)

fullMultiverse_tree <- as.Node(fullMultiverse_list)
print(fullMultiverse_tree)

fullMultiverse_dendro <- as.dendrogram(fullMultiverse_tree$
                                         samp_01_species___species1_Badger$
                                         samp_02_trackingFreq___1hrs$
                                         samp_03_trackingDura___30days$
                                         method_01_habitatMethod___ade_Compana)

fullMultiverse_nwk <- ToNewick(fullMultiverse_tree)

# plot(fullMultiverse_dendro, center = TRUE,
#      leaflab = "none")

library(ggplot2)
library(ggtree)

nwkTree <- ToNewick(fullMultiverse_tree$
           samp_01_species___species1_Badger$
           samp_02_trackingFreq___1hrs$
           samp_03_trackingDura___30days$
           method_01_habitatMethod___ade_Compana)

tree <- read.tree(text = nwkTree)

ggplot(tree) +
  geom_tree()
