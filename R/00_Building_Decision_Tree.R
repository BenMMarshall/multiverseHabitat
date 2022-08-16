# install.packages("data.tree")
# https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html#tree-creation
library(data.tree)

library(ape)
library(treeio)
library(tidytree)

library(here)
library(castor)# fast phylo calcs

library(ggplot2)
library(ggtree)

# Unique labelled nodes for sampling tree ----------------------

### Sampling
samp_01_sp <- c("sp_B", "sp_V", "sp_K")
samp_01_sp_list <- as.list(samp_01_sp)
names(samp_01_sp_list) <- samp_01_sp

samp_02_tf <- c("tf_0.5", "tf_01", "tf_02", "tf_06", "tf_12", "tf_24", "tf_48", "tf_168")
samp_02_tf_list <- as.list(samp_02_tf)
names(samp_02_tf_list) <- samp_02_tf

samp_03_td <- c("td_007", "td_015", "td_030", "td_060", "td_120", "td_240", "td_365")
samp_03_td_list <- as.list(samp_03_td)
names(samp_03_td_list) <- samp_03_td

anls_01_hm <- c("ade_wid", "ade_com", "ade_eis",
                "amt_rsf", "amt_ssf",
                "ctm_ctm")
anls_01_hm_list <- as.list(anls_01_hm)
names(anls_01_hm_list) <- anls_01_hm

# created function below is more elegant to be put into lapplys
# tempDf <- expand.grid(samp_01_sp, samp_02_tf)
# sprintf('%s.%s', tempDf[,1], tempDf[,2])

# a function to take all combos of any number of vectors and output a vector of each combo pasted together
allComboVector <- function(..., splitter = "."){
  argsL <- list(...)
  expanded <- do.call(expand.grid, argsL)
  OUT <- apply(expanded, 1, function(x){
    paste(x, collapse = splitter)
  })
  return(OUT)
}

# allComboVector(samp_01_sp, samp_02_tf)
# allComboVector(samp_01_sp, samp_02_tf, samp_03_td)

samplingList <- lapply(samp_01_sp_list, function(s01){

  temp_L1 <- lapply(samp_02_tf_list, function(s02){

    allComboVector(s01, s02)

    temp_L2 <- lapply(samp_03_td_list, function(s03){

      allComboVector(s01, s02, s03)

      temp_L3 <- lapply(anls_01_hm_list, function(a01){

        allComboVector(s01, s02, s03, a01)

      })
      names(temp_L3) <- allComboVector(s01, s02, s03, anls_01_hm)
      return(temp_L3)

    })
    names(temp_L2) <- allComboVector(s01, s02, samp_03_td)
    return(temp_L2)

  })
  names(temp_L1) <- allComboVector(s01, samp_02_tf)
  return(temp_L1)

})

samplingList

samplingDataTree <- as.Node(samplingList)

samplingPhylo <- as.phylo(samplingDataTree)

write_tree(samplingPhylo,
           file = here("notebook", "prereg", "decisionTrees", "samplingTree.txt"),
           include_edge_labels = TRUE,
           include_edge_numbers = TRUE)

samplingTreeData <- as.treedata(samplingPhylo)

tidytree::nodeid(samplingTreeData, samplingTreeData@phylo$tip.label)

data2add <- tibble::tibble(
  "node" = tidytree::nodeid(samplingTreeData, c(samplingTreeData@phylo$node.label, samplingTreeData@phylo$tip.label)),
  "nodelab" = c(samplingTreeData@phylo$node.label, samplingTreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(samplingTreeData))

data2add$species <- substr(data2add$nodelab, 1, 4)

samplingTreeData_joined <- full_join(samplingTreeData, data2add, by = "node")

ggplot(samplingTreeData_joined, branch.length = "none",
       aes(colour = species)) +
  geom_tree(size = 0.1) +
  # layout_fan(angle = 90) +
  geom_tiplab(aes(angle=angle), size = 2) +
  layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())


# Old attempts ------------------------------------------------------------


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
            list("Result" = "Result")
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
                   list("Result" = "Result")
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
               list("Result" = "Result")
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
ctmc_03list_precisionMat <- vec2NamedList(ctmc_03_precisionMat)

ctmc_04_interpMethod <- c("LinearInterp", "ShortestPath")
ctmc_04list_interpMethod <- vec2NamedList(ctmc_04_interpMethod)

ctmc_05_interpDirec <- c("LinearInterp", "ShortestPath")
ctmc_05list_interpDirec <- vec2NamedList(ctmc_05_interpDirec)

ctmcMethods_list <-
  list("method_01list_habitatMethod___ctmc_ctmc" =
         lapply(ctmc_01list_knotSpacing, function(x){
           lapply(ctmc_02list_imputeTimes, function(x){
             lapply(ctmc_03list_precisionMat, function(x){
               lapply(ctmc_04list_interpMethod, function(x){
                 lapply(ctmc_05list_interpDirec, function(x){
                   list("Result" = "Result")
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

# fullMultiverse_tree$samp_01_species___species1_Badger$leafCount
#
# fullMultiverse_dendro <- as.dendrogram(fullMultiverse_tree$
#                                          samp_01_species___species1_Badger$
#                                          samp_02_trackingFreq___1hrs$
#                                          samp_03_trackingDura___30days$
#                                          method_01_habitatMethod___ade_Compana)
#
# fullMultiverse_nwk <- ToNewick(fullMultiverse_tree)

# plot(fullMultiverse_dendro, center = TRUE,
#      leaflab = "none")

library(parallel)
detectCores()

fullMultiverse_tree$
  samp_01_species___species1_Badger$
  samp_02_trackingFreq___0.5hrs$
  samp_03_trackingDura___365days$method_01_habitatMethod___ade_Wides$leafCount

fullMultiverse_tree$
  samp_01_species___species1_Badger$
  samp_02_trackingFreq___0.5hrs$
  samp_03_trackingDura___365days$
  method_01_habitatMethod___ade_Wides$
  avail_02_designType___II$
  avail_03_avaiableAreas___dBBMM$
  avail_04_avaiableContour___90$avail_05_availablePointsMultiplier___1
fullMultiverse_list$
  samp_01_species___species1_Badger$
  samp_02_trackingFreq___0.5hrs$
  samp_03_trackingDura___365days$
  method_01_habitatMethod___ade_Wides$
  avail_02_designType___II$
  avail_03_avaiableAreas___dBBMM$
  avail_04_avaiableContour___90$avail_05_availablePointsMultiplier___1

analysisSubTreeNodes <- names(fullMultiverse_tree$
                                samp_01_species___species1_Badger$
                                samp_02_trackingFreq___0.5hrs$
                                samp_03_trackingDura___365days$children)

# analysisSubTreeNodes <- analysisSubTreeNodes[5:6]

# fullMultiverse_tree$
#   samp_01_species___species1_Badger$
#   samp_02_trackingFreq___0.5hrs$
#   samp_03_trackingDura___365days$method_01_habitatMethod___ade_Compana$leafCount
#
# fullMultiverse_tree$
#   samp_01_species___species1_Badger$
#   samp_02_trackingFreq___0.5hrs$
#   samp_03_trackingDura___365days$method_01_habitatMethod___ctmc_ctmc$leafCount

# subnode <- analysisSubTreeNodes[1]
# tictoc::tic()
# tempSubTree <- fullMultiverse_tree$
#   samp_01_species___species1_Badger$
#   samp_02_trackingFreq___0.5hrs$
#   samp_03_trackingDura___365days[[subnode]]
# analysisSubTreeList[[i]] <- as.phylo(tempSubTree)
# tictoc::toc()
#
# beepr::beep()
tictoc::tic()

analysisSubTreeList <- vector("list", length = length(analysisSubTreeNodes))
# names(analysisSubTreeList) <- analysisSubTreeNodes
i <- 0
for(subnode in analysisSubTreeNodes){
  i <- i+1
  print(subnode)

  tempSubTree <- fullMultiverse_tree$
    samp_01_species___species1_Badger$
    samp_02_trackingFreq___0.5hrs$
    samp_03_trackingDura___365days[[subnode]]

  analysisSubTreeList[[i]] <- as.phylo(tempSubTree)
  # analysisSubTreeList[[i]] <- ToNewick(tempSubTree)

  write_tree(analysisSubTreeList[[i]],
             file = here("notebook", "prereg", "decisionTrees", paste0(subnode, ".txt")),
             include_edge_labels = TRUE,
             include_edge_numbers = TRUE)
  # write.tree(analysisSubTreeList[[i]],
  #            file = here("notebook", "prereg", "decisionTrees", paste0(subnode, ".txt")))

}
# # comboTreeFull <- do.call(join_rooted_trees, analysisSubTreeList)
# analysisSubTreeList
# comboTreeFull <- do.call(join_rooted_trees, c(analysisSubTreeList, list(target_edge1 = 0,
#                                                  target_edge_length1 = 1, root_edge_length2 = 1)))

tictoc::toc()
# 8502.333 sec elapsed

analysisSubTreeList
save(analysisSubTreeList, file = here("notebook", "prereg", "decisionTrees", paste0("analysisSubTreeList.RData")))

# parrellel testing -------------------------------------------------------

# library(foreach)
library(doParallel)

# lapply(analysisSubTreeNodes,
#        FUN = function(subnode){
#          tempSubTree <- fullMultiverse_tree$
#            samp_01_species___species1_Badger$
#            samp_02_trackingFreq___0.5hrs$
#            samp_03_trackingDura___365days[[subnode]]
#          return(as.phylo(tempSubTree))
#        })

# analysisSubTreeNodes <- analysisSubTreeNodes[1:2]
tictoc::tic()

phyloList <- parallel::mclapply(analysisSubTreeNodes,
                                FUN = function(subnode){
                                  tempSubTree <- fullMultiverse_tree$
                                    samp_01_species___species1_Badger$
                                    samp_02_trackingFreq___0.5hrs$
                                    samp_03_trackingDura___365days[[subnode]]
                                  return(as.phylo(tempSubTree))
                                },
                                mc.cores = 62)
tictoc::toc()
beepr::beep()
# 4049.673 sec elapsed

save(phyloList, file = here("notebook", "prereg", "decisionTrees", paste0("phyloList.RData")))

join_rooted_trees(phyloList[[1]], phyloList[[2]], target_edge1 = 0,
                  target_edge_length1 = 15, root_edge_length2 = 15)

comboTreeFull <- do.call(join_rooted_trees, c(phyloList, list(target_edge1 = 0,
                                                              target_edge_length1 = 1, root_edge_length2 = 1)))
comboTreeFull <- do.call(join_rooted_trees, c(analysisSubTreeList, list(target_edge1 = 0,
                                                                        target_edge_length1 = 1, root_edge_length2 = 1)))

for(i in 1:length(phyloList)){
  if(i == 1){
    mainTree <- phyloList[[i]]
  } else if(i == 2){
    mainTree <- join_rooted_trees(mainTree, phyloList[[i]], target_edge1 = 0,
                                  target_edge_length1 = 15, root_edge_length2 = 15)
  } else {
    mainTree <- join_rooted_trees(mainTree$tree, phyloList[[i]], target_edge1 = 0,
                                  target_edge_length1 = 0, root_edge_length2 = 15)

  }
}
mainTree


beast_file <- system.file("examples/MCC_FluA_H3.tree",
                          package="ggtree")
beast_tree <- treeio::read.beast(beast_file)

mainTreeData <- as.treedata(mainTree$tree)
mainTreeData@data
Nnode2(mainTreeData)

class(mainTree)
class(beast_tree)
beast_tree@data$node
get.data(mainTreeData)
mainTreeData@phylo$node.label
mainTreeData@phylo$Nnode

# c(mainTreeData@phylo$node.label, mainTreeData@phylo$tip.label)

Nnode2(mainTreeData)
tidytree::nodeid(mainTreeData, mainTreeData@phylo$node.label)

tidytree::nodeid(mainTreeData, "avail_04_avaiableContour___90")

data2add <- tibble::tibble(
  "node" = tidytree::nodeid(mainTreeData, c(mainTreeData@phylo$node.label, mainTreeData@phylo$tip.label)),
  "nodelab" = c(mainTreeData@phylo$node.label, mainTreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(mainTreeData))

sum(duplicated(data2add$node))

data2add$externalVal[grepl("ssf", data2add$nodelab)] <- -999

mainTreeData <- full_join(mainTreeData, data2add, by = "node")

# mainTreeData@data <-
#   tibble::tibble("node" = 1:Nnode2(mainTreeData),
#                  "value" = 1:Nnode2(mainTreeData))

get.data(mainTreeData)

get.data(beast_tree)

mainTree$tree$value <- 100

ggplot(mainTreeData, branch.length = "none",
       aes(colour = externalVal)) +
  geom_tree(size = 0.1) +
  # layout_fan(angle = 90) +
  geom_tiplab(aes(angle=angle), size = 1) +
  layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())


list.files(here("notebook", "prereg", "decisionTrees"))

inTree <- read_tree(file = here("notebook", "prereg", "decisionTrees", paste0(subnode, ".txt")))

combotreeTest <- join_rooted_trees(analysisSubTreeList[[1]], analysisSubTreeList[[2]], target_edge1 = 0,
                                   target_edge_length1 = 15, root_edge_length2 = 15)

# read_tree(file = here("notebook", "prereg", "decisionTrees", paste0(subnode, ".txt")))


# nwkTree <- ToNewick(fullMultiverse_tree$
#                       samp_01_species___species1_Badger$
#                       samp_02_trackingFreq___0.5hrs$
#                       samp_03_trackingDura___365days$method_01_habitatMethod___amt_RSF)
# nwkTreeWides <- ToNewick(fullMultiverse_tree$
#                            samp_01_species___species1_Badger$
#                            samp_02_trackingFreq___0.5hrs$
#                            samp_03_trackingDura___365days$method_01_habitatMethod___ade_Wides)
# nwkTreeSSF <- ToNewick(fullMultiverse_tree$
#                            samp_01_species___species1_Badger$
#                            samp_02_trackingFreq___0.5hrs$
#                            samp_03_trackingDura___365days$method_01_habitatMethod___amt_SSF)
# nwkTreeCTMC <- ToNewick(fullMultiverse_tree$
#                            samp_01_species___species1_Badger$
#                            samp_02_trackingFreq___0.5hrs$
#                            samp_03_trackingDura___365days$method_01_habitatMethod___ctmc_ctmc)
# comboTree <- ape::bind.tree(read.tree(text = nwkTree), read.tree(text = nwkTreeWides))
# comboTree2 <- ape::bind.tree(read.tree(text = nwkTreeSSF), read.tree(text = nwkTreeCTMC))
# # comboTree3 <- ape::bind.tree(comboTree, comboTree2)
# comboTree3 <- do.call(bind.tree, list(comboTree, comboTree2))

ggplot(mainTree$tree) +
  geom_tree()

ggplot(analysisSubTreeList) +
  geom_tree()

# comboTree$node.label
