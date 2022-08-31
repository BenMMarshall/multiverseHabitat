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

save(sampling_List, file = here("notebook", "prereg", "decisionTrees",
                                "sampling_List.RData"))

# save it via a phylo and data.tree to a newick output
# write_tree(as.phylo(as.Node(sampling_List)),
#            file = here("notebook", "prereg", "decisionTrees", "samplingTree.txt"),
#            include_edge_labels = TRUE,
#            include_edge_numbers = TRUE)

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

analysis_List <- build_nested_list(ana_01_hm_list, ana_02_dt_list, ana_03_aa_list,
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
ctm_03_it <- paste0("it.", c("24o60", "24o30", "24", "1"))
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

ctm_List <- build_nested_list(ctm_01_hm_list,
                              ctm_02_ks_list,
                              ctm_03_it_list,
                              ctm_04_pm_list,
                              ctm_05_im_list,
                              pasteSep = "__")
# add to main analysis list at the method node
analysis_List$ctm.ctm <- ctm_List$ctm.ctm

# Save analysis tree ------------------------------------------------------

save(analysis_List, file = here("notebook", "prereg", "decisionTrees",
                                "analysis_List.RData"))

# write_tree(as.phylo(as.Node(analysis_List)),
#            file = here("notebook", "prereg", "decisionTrees", "analysisTree.txt"),
#            include_edge_labels = TRUE,
#            include_edge_numbers = TRUE)

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
