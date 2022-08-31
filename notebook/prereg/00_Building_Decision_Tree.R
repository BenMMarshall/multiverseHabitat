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

library(multiverseHabitat)
# Recursive function testing ---------------------------------------------

# species
sam_01_sp <- c("sp.B", "sp.V", "sp.K")
# tracking frequency hours
sam_02_tf <- c("tf.0.5", "tf.01", "tf.02", "tf.06", "tf.12", "tf.24", "tf.48", "tf.168")
# tracking duration days
sam_03_td <- c("td.007", "td.015", "td.030", "td.060", "td.120", "td.240", "td.365")

saList <- lapply(ls(pattern = "sam"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(saList) <- paste0(ls(pattern = "sam"), "_list")
list2env(saList, envir = .GlobalEnv)

sampling_List <- build_nested_list(sam_01_sp_list, sam_02_tf_list, sam_03_td_list,
                              pasteSep = "__")

# Save sampling tree ------------------------------------------------------

# save it via a phylo and data.tree to a newick output
write_tree(as.phylo(as.Node(sampling_List)),
           file = here("notebook", "prereg", "decisionTrees", "samplingTree.txt"),
           include_edge_labels = TRUE,
           include_edge_numbers = TRUE)

# Plot sampling tree ------------------------------------------------------

# to plot the correct looking tree we must double the final node
sampling_List_toPlot <- apply_to_leaves(sampling_List, sam_03_td_list,
                pasteSep = "__")
sampling_Phylo <- as.phylo(as.Node(sampling_List_toPlot))
sampling_TreeData <- as.treedata(sampling_Phylo)

# tidytree::nodeid(sampling_TreeData, sampling_TreeData@phylo$tip.label)
# add in a data.frame that we can base plotting modifcations on
data2add <- tibble::tibble(
  "node" = tidytree::nodeid(sampling_TreeData, c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label)),
  "nodelab" = c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(sampling_TreeData))

data2add$species <- substr(data2add$nodelab, 1, 4)

sampling_TreeData_joined <- full_join(sampling_TreeData, data2add, by = "node")

ggplot(sampling_TreeData_joined, branch.length = "none",
       aes(colour = species)) +
  geom_tree(size = 0.1) +
  # layout_fan(angle = 90) +
  geom_tiplab(aes(angle=angle), size = 2) +
  layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())

# Analysis Tree -----------------------------------------------------------

# method
ana_01_hm <- c("ade.wid", "ade.com", "ade.eis", "amt.rsf")
# design type II or III
ana_02_dt <- c("dt.t2", "dt.t3") # applies to first four methods
# available area
# MCP, href, LSCV, ADKE, dBBMM, landscape
ana_03_aa <- c("aa.mp", "aa.hr", "aa.ls", "aa.ak", "aa.db", "aa.lc") # for first four methods
# available contour
ana_04_ac <- c("ac.90", "ac.95", "ac.99") # for first four methods
# available points
ana_05_ap <- paste0("ap.", round(exp(seq(log(1), log(1000), length.out = 4)), digits = 1)) # high enough would approximate true available %

anList <- lapply(ls(pattern = "ana"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(anList) <- paste0(ls(pattern = "ana"), "_list")
list2env(anList, envir = .GlobalEnv)

analysis_List <- build_nested_list(ana_01_hm_list, ana_02_dt_list,
                                   ana_04_ac_list, ana_05_ap_list,
                                   pasteSep = "__")

# RSF addition ------------------------------------------------------------

# rsf only , point weighting
rsf_01_wt <- paste0("wt.", exp(seq(log(1), log(10000), length.out = 5)))
rsf_01_wt_list <- vec_to_list(rsf_01_wt)
# aaply the extra node to all the leaves in the rsf branch
analysis_List$amt.rsf <- apply_to_leaves(analysis_List$amt.rsf,
                                         rsf_01_wt_list, pasteSep = "__")


# SSF Tree ----------------------------------------------------------------

ssf_01_hm <- c("amt.ssf")
# issf or ssf
ssf_02_mf <- c("mf.is", "mf.ss") # applies to first four methods
# coveraite extract
ssf_03_ce <- c("ce.st", "ce.mi", "ce.ed") # for first four methods
# available steps
ssf_04_as <- paste0("as.", round(exp(seq(log(1), log(500), length.out = 5)), digits = 1)) # for first four methods

ssfList <- lapply(ls(pattern = "ssf"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(ssfList) <- paste0(ls(pattern = "ssf_"), "_list")
list2env(ssfList, envir = .GlobalEnv)

ssf_List <- build_nested_list(ssf_01_hm_list, ssf_02_mf_list, ssf_03_ce_list, ssf_04_as_list,
                  pasteSep = "__")
# add to main analysis list at the method node
analysis_List$amt.ssf <- ssf_List$amt.ssf

# CTMC Tree ---------------------------------------------------------------

ctm_01_hm <- c("ctm.ctm")
# varying the spacing of the knots, c(1, 1/2, 1/4, 1/16, 1/32)
ctm_02_ks <- paste0("ks.", round(c(1, 1/2, 1/4, 1/16, 1/32), digits = 2))
# define the sequence of times on which to sample the imputed path, c(1/24/60, 1/24/30, 1/24, 1)
ctm_03_it <- paste0("it.", c("24/60", "24/30", "24", "1"))
# precision matrix, CAR1 and CAR2
ctm_04_pm <- c("pm.c1", "pm.c2")
# intrepre method, "LinearInterp", "ShortestPath"
ctm_05_im <- c("im.li", "im.sp")

ctmList <- lapply(ls(pattern = "ctm"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(ctmList) <- paste0(ls(pattern = "ctm_"), "_list")
list2env(ctmList, envir = .GlobalEnv)

ctm_List <- build_nested_list(ctm_01_hm_list, ctm_02_ks_list, ctm_03_it_list,
                              ctm_04_pm_list, ctm_05_im_list,
                              pasteSep = "__")
# add to main analysis list at the method node
analysis_List$ctm.ctm <- ctm_List$ctm.ctm


# Save analysis tree ------------------------------------------------------

write_tree(as.phylo(as.Node(sampling_List)),
           file = here("notebook", "prereg", "decisionTrees", "samplingTree.txt"),
           include_edge_labels = TRUE,
           include_edge_numbers = TRUE)

# Plot analysis tree ------------------------------------------------------

# to plot the correct looking tree we must double the final node
analysis_List_toPlot <- apply_to_leaves(analysis_List, "leaf",
                                        pasteSep = "__")
analysis_Phylo <- as.phylo(as.Node(analysis_List_toPlot))
analysis_TreeData <- as.treedata(analysis_Phylo)

# tidytree::nodeid(sampling_TreeData, sampling_TreeData@phylo$tip.label)
# add in a data.frame that we can base plotting modifcations on
analysis_data2add <- tibble::tibble(
  "node" = tidytree::nodeid(analysis_TreeData, c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label)),
  "nodelab" = c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(analysis_TreeData))

analysis_data2add$method <- substr(analysis_data2add$nodelab, 1, 7)
analysis_TreeData_joined <- full_join(analysis_TreeData, analysis_data2add, by = "node")

ggplot(analysis_TreeData_joined, branch.length = "none",
       aes(colour = method)) +
  geom_tree(size = 0.5) +
  layout_fan(angle = 45) +
  geom_tiplab(aes(angle=angle), size = 2) +
  # layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())


# Combine to full tree ----------------------------------------------------
# TAKES LONG TIME TO CONERT TO PHYLO
multiverse_List <- apply_to_leaves(sampling_List, analysis_List,
                                   pasteSep = "__")

# to plot the correct looking tree we must double the final node
multiverse_List_toPlot <- apply_to_leaves(multiverse_List, "leaf",
                                          pasteSep = "__")
multiverse_Phylo <- as.phylo(as.Node(multiverse_List_toPlot))
multiverse_TreeData <- as.treedata(multiverse_Phylo)

# tidytree::nodeid(sampling_TreeData, sampling_TreeData@phylo$tip.label)
# add in a data.frame that we can base plotting modifcations on
multiverse_data2add <- tibble::tibble(
  "node" = tidytree::nodeid(multiverse_TreeData, c(multiverse_TreeData@phylo$node.label, multiverse_TreeData@phylo$tip.label)),
  "nodelab" = c(multiverse_TreeData@phylo$node.label, multiverse_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(multiverse_TreeData))

multiverse_data2add$method <- substr(multiverse_data2add$nodelab, 1, 7)
multiverse_TreeData_joined <- full_join(multiverse_TreeData, multiverse_data2add, by = "node")

ggplot(multiverse_TreeData_joined, branch.length = "none",
       aes(colour = method)) +
  geom_tree(size = 0.5) +
  layout_fan(angle = 45) +
  geom_tiplab(aes(angle=angle), size = 2) +
  # layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())

#
# test section ------------------------------------------------------------

vector1 <- c("a", "b", "c")
vector2 <- c("e", "f", "g", "h")
vector3 <- c("i", "j")

list1 <- vec_to_list(vector1)
list2 <- vec_to_list(vector2)
list3 <- vec_to_list(vector3)

testCombo <- nest_list(list1, list2)
lapply(list1, function(x){
  paste(x, "test")
})

testList <- build_nested_list(list1,
                             list2, pasteSep = "__")


testlistNamed <- prefix_all(testList, "abc", pasteSep = "__")

nestedListPrefix <- prefix_all(testList, "example", pasteSep = "__")

doublenest <- apply_to_leaves(nestedList = testList,
                              toNest = list3, pasteSep = "__")

is.list(sampling_List$sp.B$sp.B__tf.0.5$sp.B__tf.0.5__td.007)
x[1]

names(sampling_List["sp.B"])
names(sampling_List)

# apply_to_leaves <- function(nested_list){
#   ### names(nested_list)
#   # lapply(nested_list, function(x, names_above){
#   lapply(nested_list, function(x){
#     if (is.list(x)) {
#       apply_to_leaves(x)
#     } else {
#       # prefix_all(analysis_List, names_above)
#       prefix_all(analysis_List, x)
#     }
#   })
# }
# combined_List <- apply_to_leaves(sampling_List, pasteSep = "asffd")

# Unique labelled nodes for sampling tree ----------------------

# vec_to_list <- function(INVECTOR){
#   OUTLIST <- as.list(INVECTOR)
#   names(OUTLIST) <- INVECTOR
#   return(OUTLIST)
# }

### sampling
# species
sam_01_sp <- c("sp.B", "sp.V", "sp.K")
# tracking frequency hours
sam_02_tf <- c("tf.0.5", "tf.01", "tf.02", "tf.06", "tf.12", "tf.24", "tf.48", "tf.168")
# tracking duration days
sam_03_td <- c("td.007", "td.015", "td.030", "td.060", "td.120", "td.240", "td.365")

saList <- lapply(ls(pattern = "sam"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(saList) <- paste0(ls(pattern = "sam"), "_list")

list2env(saList, envir = .GlobalEnv)
# method, Wides, Compana, Eisera, RSF, SSF, ctmc
ana_01_hm <- c("ade.wid", "ade.com", "ade.eis",
                "amt.rsf", "amt.ssf",
                "ctm.ctm")
ana_01_hm_list <- vec_to_list(ana_01_hm)

# created function below is more elegant to be put into lapplys
# tempDf <- expand.grid(sam_01_sp, sam_02_tf)
# sprintf('%s.%s', tempDf[,1], tempDf[,2])

# a function to take all combos of any number of vectors and output a vector of each combo pasted together
allComboVector <- function(..., splitter = "__"){
  argsL <- list(...)
  expanded <- do.call(expand.grid, argsL)
  OUT <- apply(expanded, 1, function(x){
    paste(x, collapse = splitter)
  })
  return(OUT)
}

# allComboVector(sam_01_sp, sam_02_tf)
# allComboVector(sam_01_sp, sam_02_tf, sam_03_td)

sampling_List <- lapply(sam_01_sp_list, function(s01){

  temp_L1 <- lapply(sam_02_tf_list, function(s02){

    allComboVector(s01, s02)

    temp_L2 <- lapply(sam_03_td_list, function(s03){

      allComboVector(s01, s02, s03)

      # temp_L3 <- lapply(ana_01_hm_list, function(a01){
      #
      #   allComboVector(s01, s02, s03, a01)
      #
      # })
      # names(temp_L3) <- allComboVector(s01, s02, s03, ana_01_hm)
      # return(temp_L3)

    })
    names(temp_L2) <- allComboVector(s01, s02, sam_03_td)
    return(temp_L2)

  })
  names(temp_L1) <- allComboVector(s01, sam_02_tf)
  return(temp_L1)

})

sampling_List

sampling_DataTree <- as.Node(sampling_List)

sampling_Phylo <- as.phylo(sampling_DataTree)

write_tree(sampling_Phylo,
           file = here("notebook", "prereg", "decisionTrees", "samplingTree.txt"),
           include_edge_labels = TRUE,
           include_edge_numbers = TRUE)

sampling_TreeData <- as.treedata(sampling_Phylo)

tidytree::nodeid(sampling_TreeData, sampling_TreeData@phylo$tip.label)

data2add <- tibble::tibble(
  "node" = tidytree::nodeid(sampling_TreeData, c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label)),
  "nodelab" = c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(sampling_TreeData))

data2add$species <- substr(data2add$nodelab, 1, 4)

sampling_TreeData_joined <- full_join(sampling_TreeData, data2add, by = "node")

ggplot(sampling_TreeData_joined, branch.length = "none",
       aes(colour = species)) +
  geom_tree(size = 0.1) +
  # layout_fan(angle = 90) +
  geom_tiplab(aes(angle=angle), size = 2) +
  layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())


# Analysis trees ----------------------------------------------------------
rm(list = ls(pattern = "ana"))

# Core ade tree part ------------------------------------------------------
# method
ana_01_hm <- c("ade.wid", "ade.com", "ade.eis", "amt.rsf")
ana_01_hm_list <- vec_to_list(ana_01_hm)
# design type II or III
ana_02_dt <- c("dt.t2", "dt.t3") # applies to first four methods
# available area
# MCP, href, LSCV, ADKE, dBBMM, landscape
ana_03_aa <- c("aa.mp", "aa.hr", "aa.ls", "aa.ak", "aa.db", "aa.lc") # for first four methods
# available contour
ana_04_ac <- c("ac.90", "ac.95", "ac.99") # for first four methods
# available points
ana_05_ap <- paste0("ap.", round(exp(seq(log(1), log(1000), length.out = 4)), digits = 1)) # high enough would approximate true available %

anList <- lapply(ls(pattern = "ana"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(anList) <- paste0(ls(pattern = "ana"), "_list")

list2env(anList, envir = .GlobalEnv)

analysis_ade_List <- lapply(ana_01_hm_list, function(a01){

  temp_L1 <- lapply(ana_02_dt_list, function(a02){

    allComboVector(a01, a02)

    temp_L2 <- lapply(ana_03_aa_list, function(a03){

      allComboVector(a01, a02, a03)

      temp_L3 <- lapply(ana_04_ac_list, function(a04){

        allComboVector(a01, a02, a03, a04)

        temp_L4 <- lapply(ana_05_ap_list, function(a05){

          # allComboVector(a01, a02, a03, a04, a05)
          temp_Lfinal <- as.list(allComboVector(a01, a02, a03, a04, a05))
          names(temp_Lfinal) <- allComboVector(a01, a02, a03, a04, a05)
          temp_Lfinal

        })
        names(temp_L4) <- allComboVector(a01, a02, a03, a04, ana_05_ap)
        return(temp_L4)

      })
      names(temp_L3) <- allComboVector(a01, a02, a03, ana_04_ac)
      return(temp_L3)

    })
    names(temp_L2) <- allComboVector(a01, a02, ana_03_aa)
    return(temp_L2)

  })
  names(temp_L1) <- allComboVector(a01, ana_02_dt)
  return(temp_L1)

})

# analysisAdeDataTree <- as.Node(analysis_ade_List)
#
# analysisAdePhylo <- as.phylo(analysisAdeDataTree)

analysis_ade_List


# Adding RSF aspect to ade tree -------------------------------------------
# as they share all nodes bar one

# rsf only , point weighting
rsf_01_wt <- paste0("wt.", exp(seq(log(1), log(10000), length.out = 5)))
rsf_01_wt_list <- vec_to_list(rsf_01_wt)

#### making sure we get the correct point in the tree testing
# get_elements <- function(x, element) {
#   if(is.list(x))
#   {
#     if(element %in% names(x)) x[[element]]
#     else lapply(x, get_elements, element = element)
#   }
# }
#
# grepl("amt.*dt.*aa.*ac.*ap", "amt_rsf.dt_t3.aa_ls.ac_90.ap_1000")
# # grepl("amt.*dt.*aa.*ac.*ap", "amt_rsf")
#
#
# get_elements(analysis_ade_List, "amt_rsf.dt_t3.aa_ls.ac_90")
#
# append()

# analysis_ade_List[grepl("amt", names(analysis_ade_List))]
# # analysis_ade_List[grepl("amt", names(analysis_ade_List))][dt][aa][ac][ap]
# analysis_ade_List[grepl("amt", names(analysis_ade_List))][["amt_rsf"]][[1]][[1]][[1]]
#
# analysis_ade_List[grepl("amt", names(analysis_ade_List))][["amt_rsf"]][[paste(
#   "amt_rsf", ana_02_dt, sep = "__")]][[paste(
#     "amt_rsf", ana_02_dt, ana_03_aa, sep = "__")]][[paste(
#       "amt_rsf", ana_02_dt, ana_03_aa, ana_04_ac, sep = "__")]][[paste(
#         "amt_rsf", ana_02_dt, ana_03_aa, ana_04_ac, ana_05_ap, sep = "__")]]
#
# names(rsf_01_wt_list) <- paste(
#   "amt_rsf", ana_02_dt[1], ana_03_aa[1], ana_04_ac[1], ana_05_ap[1], sep = "__",
#   names(rsf_01_wt_list))
#
# analysis_ade_List[grepl("amt", names(analysis_ade_List))][["amt_rsf"]][[paste(
#   "amt_rsf", ana_02_dt[1], sep = "__")]][[paste(
#     "amt_rsf", ana_02_dt[1], ana_03_aa[1], sep = "__")]][[paste(
#       "amt_rsf", ana_02_dt[1], ana_03_aa[1], ana_04_ac[1], sep = "__")]][[paste(
#         "amt_rsf", ana_02_dt[1], ana_03_aa[1], ana_04_ac[1], ana_05_ap[1], sep = "__")]]


for(a02 in ana_02_dt){
  for(a03 in ana_03_aa){
    for(a04 in ana_04_ac){
      for(a05 in ana_05_ap){

        leaves <- paste(
          "amt.rsf", a02, a03, a04, a05, sep = "__",
          rsf_01_wt)

        leaves_list <- vec_to_list(leaves)

        names(leaves_list) <- leaves

        # temp_Lfinal <- as.list(allComboVector(a01, f01, f02, f03))
        # names(temp_Lfinal) <- allComboVector(a01, f01, f02, f03)
        # temp_Lfinal

        analysis_ade_List[grepl("amt", names(analysis_ade_List))][["amt.rsf"]][[paste(
          "amt.rsf", a02, sep = "__")]][[paste(
            "amt.rsf", a02, a03, sep = "__")]][[paste(
              "amt.rsf", a02, a03, a04, sep = "__")]][[paste(
                "amt.rsf", a02, a03, a04, a05, sep = "__")]] <- lapply(leaves_list, function(x){
                  leaves_list
                })# have the list in the list to make sure the nodes don't get removed/turned into leaves at the tree conversion step

      }
    }
  }
}

# SSF tree ----------------------------------------------------------------

ana_01_hm <- c("amt.ssf")
ana_01_hm_list <- vec_to_list(ana_01_hm)
# issf or ssf
ssf_01_mf <- c("mf.is", "mf.ss") # applies to first four methods
# coveraite extract
ssf_02_ce <- c("ce.st", "ce.mi", "ce.ed") # for first four methods
# available steps
ssf_03_as <- paste0("as.", round(exp(seq(log(1), log(500), length.out = 5)), digits = 1)) # for first four methods

ssfList <- lapply(ls(pattern = "ssf"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(ssfList) <- paste0(ls(pattern = "ssf_"), "_list")

list2env(ssfList, envir = .GlobalEnv)

analysis_ssf_List <- lapply(ana_01_hm_list, function(a01){

  temp_L1 <- lapply(ssf_01_mf_list, function(f01){

    allComboVector(a01, f01)

    temp_L2 <- lapply(ssf_02_ce_list, function(f02){

      allComboVector(a01, f01, f02)

      temp_L3 <- lapply(ssf_03_as_list, function(f03){

        # allComboVector(a01, f01, f02, f03)
        temp_Lfinal <- as.list(allComboVector(a01, f01, f02, f03))
        names(temp_Lfinal) <- allComboVector(a01, f01, f02, f03)
        temp_Lfinal

      })
      names(temp_L3) <- allComboVector(a01, f01, f02, ssf_03_as)
      return(temp_L3)

    })
    names(temp_L2) <- allComboVector(a01, f01, ssf_02_ce)
    return(temp_L2)

  })
  names(temp_L1) <- allComboVector(a01, ssf_01_mf)
  return(temp_L1)

})

# ctmc analysis -----------------------------------------------------------

ana_01_hm <- c("ctm.ctm")
ana_01_hm_list <- vec_to_list(ana_01_hm)
# ctmcmove only
# varying the spacing of the knots, c(1, 1/2, 1/4, 1/16, 1/32)
ctm_01_ks <- paste0("ks.", round(c(1, 1/2, 1/4, 1/16, 1/32), digits = 2))
# define the sequence of times on which to sample the imputed path, c(1/24/60, 1/24/30, 1/24, 1)
ctm_02_it <- paste0("it.", c("24/60", "24/30", "24", "1"))
# precision matrix, CAR1 and CAR2
ctm_03_pm <- c("pm.c1", "pm.c2")
# intrepre method, "LinearInterp", "ShortestPath"
ctm_04_im <- c("im.li", "im.sp")

ctmList <- lapply(ls(pattern = "ctm"), function(x){
  tempList <- vec_to_list(get(x))
  assign(paste0(x, "_list"), tempList)
})
names(ctmList) <- paste0(ls(pattern = "ctm_"), "_list")

list2env(ctmList, envir = .GlobalEnv)

analysis_ctm_List <- lapply(ana_01_hm_list, function(a01){

  temp_L1 <- lapply(ctm_01_ks_list, function(c01){

    allComboVector(a01, c01)

    temp_L2 <- lapply(ctm_02_it_list, function(c02){

      allComboVector(a01, c01, c02)

      temp_L3 <- lapply(ctm_03_pm_list, function(c03){

        allComboVector(a01, c01, c02, c03)

        temp_L4 <- lapply(ctm_04_im_list, function(c04){

          # allComboVector(a01, c01, c02, c03, c04)
          temp_Lfinal <- as.list(allComboVector(a01, c01, c02, c03, c04))
          names(temp_Lfinal) <- allComboVector(a01, c01, c02, c03, c04)
          temp_Lfinal

        })
        names(temp_L4) <- allComboVector(a01, c01, c02, c03, ctm_04_im)
        return(temp_L4)

      })
      names(temp_L3) <- allComboVector(a01, c01, c02, ctm_03_pm)
      return(temp_L3)

    })
    names(temp_L2) <- allComboVector(a01, c01, ctm_02_it)
    return(temp_L2)

  })
  names(temp_L1) <- allComboVector(a01, ctm_01_ks)
  return(temp_L1)

})


# Combine analysis lists --------------------------------------------------
# https://stackoverflow.com/questions/45091691/convert-a-nested-list-in-dendrogram-tree-with-r
# x <- analysis_List
#
# # flatten list
# x2 <- paste0(lapply(x, function(y) paste0("(", paste0(y, collapse = ","), ")")), collapse = ",")
#
# # remove unwanted characters
# x2 <- gsub('\"|c|list| ', "", x2)
# x2 <- paste0("(", x2, ");")
#
# # remove brackets from single term list object
# library(stringr)
# x3 <- str_replace_all(x2, "\\([a-z]*\\)", function(x) gsub("^\\(|\\)$", "", x))
#
# # plot
# library(ape)
# plot(read.tree(text = x3))
#
# testnew <- read.tree(text = x3)

##########

analysis_List <- append(analysis_ade_List, analysis_ssf_List)
analysis_List <- append(analysis_List, analysis_ctm_List)

analysis_DataTree <- as.Node(analysis_List, check = "check")

analysis_Phylo <- as.phylo(analysis_DataTree)

write_tree(analysis_Phylo,
           file = here("notebook", "prereg", "decisionTrees", "analysisTree.txt"),
           include_edge_labels = TRUE,
           include_edge_numbers = TRUE)

analysis_TreeData <- as.treedata(analysis_Phylo)

tidytree::nodeid(analysis_TreeData, analysis_TreeData@phylo$tip.label)

tibble::tibble(
  "node" = tidytree::nodeid(analysis_TreeData, c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label)),
  "nodelab" = c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(analysis_TreeData)) %>%
  print(n = 1000)

data2add <- tibble::tibble(
  "node" = tidytree::nodeid(analysis_TreeData, c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label)),
  "nodelab" = c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(analysis_TreeData))

data2add$package <- substr(data2add$nodelab, 1, 2)
data2add$method <- substr(data2add$nodelab, 1, 7)

analysis_TreeData_joined <- full_join(analysis_TreeData, data2add, by = "node")

ggplot(analysis_TreeData_joined, branch.length = "none",
       aes(colour = method)) +
  geom_tree(size = 0.1) +
  # layout_fan(angle = 90) +
  geom_tiplab(aes(angle=angle), size = 2) +
  layout_circular() +
  theme_bw() +
  theme(panel.grid = element_blank())


# Combining sampling and analysis trees -----------------------------------
# modifed from answer given at: https://stackoverflow.com/questions/63074814/recursively-change-names-in-nested-lists-in-r
# library(purrr) # not needed anymore
# prefix_all <- function(nested_list, prefix){
#   if(is.null(prefix)) stop("Prefix is null")
#   names(nested_list) <- paste0(prefix, "__", names(nested_list))
#
#   lapply(nested_list, function(x){
#     if (is.list(x)) {
#       prefix_all(x, prefix = prefix)
#     } else {
#       paste0(prefix, "__", x)
#     }
#   })
# }
# prefix_all(analysis_List, prefix = "sp.B__tf.0.5__td.007__")

sampling_List

# apply_to_leaves <- function(nested_list){
# ### names(nested_list)
#   # lapply(nested_list, function(x, names_above){
#   lapply(nested_list, function(x){
#     if (is.list(x)) {
#       apply_to_leaves(x)
#     } else {
#       # prefix_all(analysis_List, names_above)
#       prefix_all(analysis_List, x)
#     }
#   })
# }
combined_List <- apply_to_leaves(sampling_List)

combined_DataTree <- as.Node(combined_List, check = "check")
### BEWARE TAKES A LONG TIME TO COMPLETE
combined_Phylo <- as.phylo(combined_DataTree)

# Old attempts ------------------------------------------------------------


# as a list -----------------------------------------------------------



### Sampling
samp_01_species <- c("species1_Badger", "species1_Vulture", "species1_Kingcobra")
samp_01list_species <- vec_to_list(samp_01_species)
samp_02_trackingFreq <- c("0.5hrs", "1hrs", "2hrs", "6hrs", "12hrs", "24hrs", "48hrs", "168hrs")
samp_02list_trackingFreq <- vec_to_list(samp_02_trackingFreq)
samp_03_trackingDura <- c("7days", "15days", "30days", "60days", "120days", "240days", "365days")
samp_03list_trackingDura <- vec_to_list(samp_03_trackingDura)

method_01_habitatMethod <- c("ade_Wides", "ade_Compana", "ade_Eisera",
                             "amt_RSF", "amt_SSF",
                             "ctmc_ctmc")
method_01list_habitatMethod <- vec_to_list(method_01_habitatMethod)

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
method_01list_habitatMethod <- vec_to_list(method_01_habitatMethod)

avail_02_designType <- c("II", "III") # applies to first four methods
avail_02list_designType <- vec_to_list(avail_02_designType)
avail_03_avaiableAreas <- c("MCP", "KDEhref", "KDElscv", "AKDEbest", "dBBMM", "landscape") # for first four methods
avail_03list_avaiableAreas <- vec_to_list(avail_03_avaiableAreas)
avail_04_avaiableContour <- c("90", "95", "99") # for first four methods
avail_04list_avaiableContour <- vec_to_list(avail_04_avaiableContour)
avail_05_availablePointsMultiplier <- exp(seq(log(1), log(1000), length.out = 4)) # high enough would approximate true available %
avail_05list_availablePointsMultiplier <- vec_to_list(avail_05_availablePointsMultiplier)

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
rsf_01list_weighting <- vec_to_list(rsf_01_weighting)

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
ssf_01list_modelForm <- vec_to_list(avail_02_designType)
ssf_02_covariateExtract <- c("start", "middle", "end")
ssf_02list_covariateExtract <- vec_to_list(ssf_02_covariateExtract)
ssf_03_availableSteps <- exp(seq(log(1), log(500), length.out = 5))
ssf_03list_availableSteps <- vec_to_list(ssf_03_availableSteps)

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
ctmc_01list_knotSpacing <- vec_to_list(ctmc_01_knotSpacing)
# define the sequence of times on which to sample the imputed path
ctmc_02_imputeTimes <- c(1/24/60, 1/24/30, 1/24, 1)
ctmc_02list_imputeTimes <- vec_to_list(ctmc_02_imputeTimes)

ctmc_03_precisionMat <- c("CAR1", "CAR2")
ctmc_03list_precisionMat <- vec_to_list(ctmc_03_precisionMat)

ctmc_04_interpMethod <- c("LinearInterp", "ShortestPath")
ctmc_04list_interpMethod <- vec_to_list(ctmc_04_interpMethod)

ctmc_05_interpDirec <- c("LinearInterp", "ShortestPath")
ctmc_05list_interpDirec <- vec_to_list(ctmc_05_interpDirec)

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
method_01list_habitatMethod <- vec_to_list(method_01_habitatMethod)
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
