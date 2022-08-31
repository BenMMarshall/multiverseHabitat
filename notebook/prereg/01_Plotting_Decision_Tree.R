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

(sampling_Plot <- ggplot(sampling_TreeData_joined, branch.length = "none") +
  geom_tree(aes(colour = species), size = 0.5) +
  geom_label(aes(x = branch, y = y, colour = species,
                 label = ifelse(grepl("td", nodeLabShort) | nodeLabShort == "Root",
                                NA, nodeLabShort)),
             fill = "white",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines"),
             label.size = 0) +
  # geom_tiplab(aes(
  #   # angle = angle,
  #   colour = nodeType,
  #   label = substr(sub(".*\\__", "", nodeLab), 4,
  #                  nchar(sub(".*\\__", "", nodeLab)))),
  #   size = 2, angle = 0, hjust = 0.5, nudge_x = 0.05) +
  scale_colour_manual(values = c("King Cobra" = "#AD6DED", "Vulture" = "#7D26D4", "Badger" = "#4F0E99"),
                      # labels = c("Species", "Tracking frequency", "Tracking duration"),
                      na.translate = FALSE) +
  # layout_circular() +
  labs(colour = "Species") +
  theme_void() +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = 2),
        legend.position = c(0.1, 0.9)))


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

analysis_data2add <- analysis_data2add %>%
  mutate(nodeType =
           case_when(
             nodeType == "ad" ~ "adeHabitatHS",
             nodeType == "dt" ~ "Design type",
             nodeType == "aa" ~ "Available area",
             nodeType == "ac" ~ "Area contour",
             nodeType == "am" ~ "amt",
             nodeType == "ap" ~ "Available points",
             nodeType == "mf" ~ "Model formula",
             nodeType == "ce" ~ "Covariate extract",
             nodeType == "ct" ~ "ctmcmove",
             nodeType == "ks" ~ "Knot spacing",
             nodeType == "it" ~ "Imputed timing",
             nodeType == "pm" ~ "Precision matrix",
             nodeType == "wt" ~ "Weighting of points",
             nodeType == "as" ~ "Available steps",
             nodeType == "im" ~ "Interpolation method",
             TRUE ~ nodeType
           ))

analysis_data2add$method <- substr(analysis_data2add$nodeLab, 1, 3)
analysis_data2add <- analysis_data2add %>%
  mutate(method =
           case_when(
             method == "ade" ~ "adeHabitatHS",
             method == "amt" ~ "amt",
             method == "ctm" ~ "ctmcmove",
             TRUE ~ method
           ))
analysis_data2add$method[analysis_data2add$method == "Roo"] <- NA


analysis_data2add$nodeLabShort <- sub(".*\\__", "", analysis_data2add$nodeLab)
analysis_data2add$nodeLabShort[!grepl("amt|ade|ctm", analysis_data2add$nodeLabShort)] <-
  substr(analysis_data2add$nodeLabShort[!grepl("amt|ade|ctm", analysis_data2add$nodeLabShort)],
         4,
         nchar(analysis_data2add$nodeLabShort[!grepl("amt|ade|ctm", analysis_data2add$nodeLabShort)]))
analysis_data2add <- analysis_data2add %>%
  mutate(nodeLabShort =
           case_when(
             nodeLabShort == "ade.eis" ~ "Eisera",
             nodeLabShort == "ade.wid" ~ "Wides",
             nodeLabShort == "ade.com" ~ "Compana",
             nodeLabShort == "amt.rsf" ~ "RSF",
             nodeLabShort == "amt.ssf" ~ "SSF",
             nodeLabShort == "ctm.ctm" ~ "ctmcmove",
             TRUE ~ "Root"
           ))

analysis_data2add$nodeLabShort[grepl("Root", analysis_data2add$nodeLabShort)] <- NA
#
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.B"] <- "Badger"
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.V"] <- "Vulture"
# analysis_data2add$nodeLabShort[analysis_data2add$nodeLabShort == "sp.K"] <- "King Cobra"

analysis_TreeData_joined <- full_join(analysis_TreeData, analysis_data2add, by = "node")

(analysis_Plot <- ggplot(analysis_TreeData_joined, branch.length = "none") +
  geom_tree(aes(colour = method), size = 0.5) +
  geom_label(aes(x = branch, y = y, colour = method,
                 label = nodeLabShort),
             fill = "white",
             label.padding = unit(0.15, "lines"),
             label.r = unit(0.1, "lines"),
             label.size = 0) +
  # geom_tiplab(aes(angle = angle,
  #                 colour = nodeType,
  #                 label = substr(sub(".*\\__", "", nodeLab), 4,
  #                                nchar(sub(".*\\__", "", nodeLab)))),
  #             size = 2) +
  scale_colour_manual(values = c("#E87D13", "#965A1D", "#302010"),
                      na.translate = FALSE) +
  # layout_circular() +
  labs(colour = "Package") +
  theme_void() +
  theme(panel.grid = element_blank(),
        legend.title = element_text(face = 2),
        legend.position = c(0.12, 0.9)))

# # "#E87D13", "#965A1D", "#302010"
# colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(4)[1]
# colorRampPalette(colors = c("#965A1D", "#876e54"))(4)[1]
# colorRampPalette(colors = c("#302010", "#4a3e33"))(4)[1]
# adeNodes <- 5
# amtNodes <- 5
# ctmNodes <- 5
#
# fullPalette <- c("adeHabitatHS" = colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(adeNodes)[adeNodes-adeNodes +1],
#                  "Design type" = colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(adeNodes)[adeNodes-adeNodes +2],
#                  "Available area" = colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(adeNodes)[adeNodes-adeNodes +3],
#                  "Area contour" = colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(adeNodes)[adeNodes-adeNodes +4],
#                  "Available points" = colorRampPalette(colors = c("#E87D13", "#e0bd9b"))(adeNodes)[adeNodes-adeNodes +5],
#                  "amt" = colorRampPalette(colors = c("#965A1D", "#876e54"))(amtNodes)[amtNodes-amtNodes +1],
#                  "Model formula" = colorRampPalette(colors = c("#965A1D", "#876e54"))(amtNodes)[amtNodes-amtNodes +2],
#                  "Covariate extract" = colorRampPalette(colors = c("#965A1D", "#876e54"))(amtNodes)[amtNodes-amtNodes +3],
#                  "Weighting of points" = colorRampPalette(colors = c("#965A1D", "#876e54"))(amtNodes)[amtNodes-amtNodes +4],
#                  "Available steps" = colorRampPalette(colors = c("#965A1D", "#876e54"))(amtNodes)[amtNodes-amtNodes +5],
#                  "ctmcmove" = colorRampPalette(colors = c("#302010", "#4a3e33"))(ctmNodes)[ctmNodes-ctmNodes +1],
#                  "Knot spacing" = colorRampPalette(colors = c("#302010", "#4a3e33"))(ctmNodes)[ctmNodes-ctmNodes +2],
#                  "Imputed timing" = colorRampPalette(colors = c("#302010", "#4a3e33"))(ctmNodes)[ctmNodes-ctmNodes +3],
#                  "Precision matrix" = colorRampPalette(colors = c("#302010", "#4a3e33"))(ctmNodes)[ctmNodes-ctmNodes +4],
#                  "Interpolation method" = colorRampPalette(colors = c("#302010", "#4a3e33"))(ctmNodes)[ctmNodes-ctmNodes +5])

library(patchwork)

sampling_Plot + analysis_Plot +
  plot_annotation(tag_level = c("A", "B")) &
  theme(plot.tag = element_text(face = 2))

ggsave(filename = here("notebook", "prereg", "figures", "preregDecisionTrees.png"),
       width = 360, height = 180, dpi = 300, units = "mm")
