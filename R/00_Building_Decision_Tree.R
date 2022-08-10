# install.packages("data.tree")
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#tree-creation
library(data.tree)

# as a list -----------------------------------------------------------

vec2NamedList <- function(INVECTOR){
  OUTLIST <- vector("list", length = length(INVECTOR))
  names(OUTLIST) <- paste0(deparse(substitute(INVECTOR)), "___", INVECTOR)
  return(OUTLIST)
}

### Sampling
samp_01_species <- c("species1_Badger", "species1_Vulture", "species1_Kingcobra")
samp_01list_species <- vec2NamedList(samp_01_species)
samp_02_trackingFreq <- c("0.5hrs", "1hrs", "2hrs", "6hrs", "12hrs", "24hrs", "48hrs", "168hrs")
samp_02list_trackingFreq <- vec2NamedList(samp_02_trackingFreq)
samp_03_trackingDura <- c("7days", "15days", "30days", "60days", "120days", "240days", "365days")
samp_03list_trackingDura <- vec2NamedList(samp_03_trackingDura)

method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera",
                             "amt_RSF", "amt_SSF",
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
avail_03_avaiableAreas <- c("MCP", "KDEhref", "KDElscv", "AKDEbest", "dBBMM", "landscape") # for first four methods
avail_03list_avaiableAreas <- vec2NamedList(avail_03_avaiableAreas)
avail_04_avaiableContour <- c("90", "95", "99") # for first four methods
avail_04list_avaiableContour <- vec2NamedList(avail_04_avaiableContour)
avail_05_availablePointsMultiplier <- exp(seq(log(1), log(1000), length.out = 4)) # high enough would approximate true available %
avail_05list_availablePointsMultiplier <- vec2NamedList(avail_05_availablePointsMultiplier)

adeMethods_list <-
  lapply(method_01list_habitatMethod, function(x){
    lapply(avail_02list_designType, function(x){
      lapply(avail_03list_avaiableAreas, function(x){
        lapply(avail_04list_avaiableContour, function(x){
          lapply(avail_05list_availablePointsMultiplier, function(x){
            "Result"
          })
        })
      })
    })
  })

# remove the landscape option from type III design
adeMethods_list$method_01_habitatMethod___ade_Wides$avail_02_designType___III$avail_03_avaiableAreas___landscape <- NULL
adeMethods_list$method_01_habitatMethod___ade_Compana$avail_02_designType___III$avail_03_avaiableAreas___landscape <- NULL
adeMethods_list$method_01_habitatMethod___ade_Eisera$avail_02_designType___III$avail_03_avaiableAreas___landscape <- NULL

# rsf only
rsf_01_weighting <- exp(seq(log(1), log(10000), length.out = 5))
rsf_01list_weighting <- vec2NamedList(rsf_01_weighting)

rsfMethods_list <-
  list("method_01list_habitatMethod___amt_RSF" =
         lapply(avail_02list_designType, function(x){
           lapply(avail_03list_avaiableAreas, function(x){
             lapply(avail_04list_avaiableContour, function(x){
               lapply(avail_05list_availablePointsMultiplier, function(x){
                 lapply(rsf_01list_weighting, function(x){
                   "Result"
                 })
               })
             })
           })
         })
  )
str(rsfMethods_list)

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

ctmc_03_precisionMat <- c("CAR1", "CAR2")
ctmc_03list_precisionMat <- vec2NamedList(ctmc_04_precisionMat)

ctmc_04_interpMethod <- c("LinearInterp", "ShortestPath")
ctmc_04list_interpMethod <- vec2NamedList(ctmc_03_interpMethod)

ctmc_05_interpDirec <- c("LinearInterp", "ShortestPath")
ctmc_05list_interpDirec <- vec2NamedList(ctmc_05_interpDirec)

ctmcMethods_list <-
  list("method_01list_habitatMethod___ctmc_ctmc" =
         lapply(ctmc_01list_knotSpacing, function(x){
           lapply(ctmc_02list_imputeTimes, function(x){
             lapply(ctmc_03list_precisionMat, function(x){
               lapply(ctmc_04list_interpMethod, function(x){
                 lapply(ctmc_05list_interpDirec, function(x){
                   "Result"
                 })
               })
             })
           })
         })
  )
str(ctmcMethods_list)


# Combine all to the sample list ------------------------------------------

method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera",
                             "amt_RSF", "amt_SSF",
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
          } else if(x == "method_01list_habitatMethod___amt_RSF"){
            rsfMethods_list$method_01list_habitatMethod___amt_RSF
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
