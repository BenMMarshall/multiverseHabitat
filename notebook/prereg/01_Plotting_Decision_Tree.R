library(ggplot2)
library(ggtree)
library(here)

load(here("notebook", "prereg", "decisionTrees", "sampling_List.RData"))

# to plot the correct looking tree we must double the final node
sampling_List_toPlot <- apply_to_leaves(sampling_List, "leaf",
                                        pasteSep = "__")
sampling_Phylo <- as.phylo(as.Node(sampling_List_toPlot))
sampling_TreeData <- as.treedata(sampling_Phylo)

# tidytree::nodeid(sampling_TreeData, sampling_TreeData@phylo$tip.label)
# add in a data.frame that we can base plotting modifcations on
sampling_data2add <- tibble::tibble(
  "node" = tidytree::nodeid(sampling_TreeData, c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label)),
  "nodeLab" = c(sampling_TreeData@phylo$node.label, sampling_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(sampling_TreeData))

sampling_data2add$nodeType <- substr(sub(".*\\__", "", sampling_data2add$nodeLab), 1, 2)
sampling_data2add$nodeType[sampling_data2add$nodeType == "Ro"] <- NA

sampling_data2add$nodeType <- factor(sampling_data2add$nodeType, levels = unique(sampling_data2add$nodeType))

sampling_data2add$species <- substr(sampling_data2add$nodeLab, 1, 4)
sampling_data2add$species[sampling_data2add$species == "sp.B"] <- "Badger"
sampling_data2add$species[sampling_data2add$species == "sp.V"] <- "Vulture"
sampling_data2add$species[sampling_data2add$species == "sp.K"] <- "King Cobra"
sampling_data2add$species[sampling_data2add$species == "Root"] <- NA

sampling_data2add$nodeLabShort <- sub(".*\\__", "", sampling_data2add$nodeLab)
sampling_data2add$nodeLabShort[grepl("tf", sampling_data2add$nodeLabShort)] <-
  substr(sampling_data2add$nodeLabShort[grepl("tf", sampling_data2add$nodeLabShort)],
         4,
         nchar(sampling_data2add$nodeLabShort[grepl("tf", sampling_data2add$nodeLabShort)]))

sampling_data2add$nodeLabShort[grepl("Root", sampling_data2add$nodeLabShort)] <- NA

sampling_data2add$nodeLabShort[sampling_data2add$nodeLabShort == "sp.B"] <- "Badger"
sampling_data2add$nodeLabShort[sampling_data2add$nodeLabShort == "sp.V"] <- "Vulture"
sampling_data2add$nodeLabShort[sampling_data2add$nodeLabShort == "sp.K"] <- "King Cobra"

sampling_TreeData_joined <- full_join(sampling_TreeData, sampling_data2add, by = "node")

ggplot(sampling_TreeData_joined, branch.length = "none") +
  geom_tree(aes(colour = nodeType), size = 0.5) +
  geom_label(aes(x = branch, y = y, colour = nodeType,
                 label = ifelse(grepl("td", nodeLabShort) | nodeLabShort == "Root",
                                NA, nodeLabShort)),
             fill = "white",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines"),
             label.size = 0) +
  geom_tiplab(aes(angle = angle,
                  colour = nodeType,
                  label = substr(sub(".*\\__", "", nodeLab), 4,
                                 nchar(sub(".*\\__", "", nodeLab)))),
              size = 2) +
  scale_colour_manual(values = c("grey5", "grey45", "grey65"),
                      labels = c("Species", "Tracking frequency", "Tracking duration"),
                      na.translate = FALSE) +
  layout_circular() +
  labs(colour = "Node") +
  theme_void() +
  theme(panel.grid = element_blank())


# Analysis Tree -----------------------------------------------------------

load(here("notebook", "prereg", "decisionTrees", "analysis_List.RData"))

# to plot the correct looking tree we must double the final node
analysis_List_toPlot <- apply_to_leaves(analysis_List, "leaf",
                                        pasteSep = "__")
analysis_Phylo <- as.phylo(as.Node(analysis_List_toPlot))
analysis_TreeData <- as.treedata(analysis_Phylo)

# tidytree::nodeid(analysis_TreeData, analysis_TreeData@phylo$tip.label)
# add in a data.frame that we can base plotting modifcations on
analysis_data2add <- tibble::tibble(
  "node" = tidytree::nodeid(analysis_TreeData, c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label)),
  "nodeLab" = c(analysis_TreeData@phylo$node.label, analysis_TreeData@phylo$tip.label),
  "externalVal" = 1:Nnode2(analysis_TreeData))

analysis_data2add$nodeType <- substr(sub(".*\\__", "", analysis_data2add$nodeLab), 1, 2)
analysis_data2add$nodeType[analysis_data2add$nodeType == "Ro"] <- NA

# analysis_data2add$nodeType <- factor(analysis_data2add$nodeType, levels = unique(analysis_data2add$nodeType))

library(dplyr)

analysis_data2add %>%
  mutate(nodeType =
           case_when(
             nodeType == "ad" ~ "adeHabitatHS",
             nodeType == "dt" ~ "",
             nodeType == "am" ~ "amt",
             nodeType == "ap" ~ "",
             nodeType == "mf" ~ "",
             nodeType == "ct" ~ "ctmcmove",
             nodeType == "ks" ~ "Knot spacing",
             nodeType == "wt" ~ "",
             nodeType == "as" ~ "",
             nodeType == "im" ~ "Imputed spacing",
             nodeType == "pc" ~ "Precision matrix",



           ))

analysis_data2add$nodeLabShort <- sub(".*\\__", "", analysis_data2add$nodeLab)
analysis_data2add$nodeLabShort[grepl("tf", analysis_data2add$nodeLabShort)] <-
  substr(analysis_data2add$nodeLabShort[grepl("tf", analysis_data2add$nodeLabShort)],
         4,
         nchar(analysis_data2add$nodeLabShort[grepl("tf", analysis_data2add$nodeLabShort)]))

analysis_data2add$nodeLabShort[grepl("Root", analysis_data2add$nodeLabShort)] <- NA
#
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.B"] <- "Badger"
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.V"] <- "Vulture"
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.K"] <- "King Cobra"

analysis_TreeData_joined <- full_join(analysis_TreeData, analysis_data2add, by = "node")

ggplot(analysis_TreeData_joined, branch.length = "none") +
  geom_tree(aes(colour = nodeType), size = 0.5) +
  geom_label(aes(x = branch, y = y, colour = nodeType,
                 label = ifelse(grepl("td", nodeLabShort) | nodeLabShort == "Root",
                                NA, nodeLabShort)),
             fill = "white",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines"),
             label.size = 0) +
  geom_tiplab(aes(angle = angle,
                  colour = nodeType,
                  label = substr(sub(".*\\__", "", nodeLab), 4,
                                 nchar(sub(".*\\__", "", nodeLab)))),
              size = 2) +
  # scale_colour_manual(values = c("grey5", "grey45", "grey65"),
  #                     labels = c("Species", "Tracking frequency", "Tracking duration"),
  #                     na.translate = FALSE) +
  layout_circular() +
  labs(colour = "Node") +
  theme_void() +
  theme(panel.grid = element_blank())
