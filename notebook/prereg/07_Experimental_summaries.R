
# summary plots draft -----------------------------------------------------

targets::tar_load(areaResults)
targets::tar_load(ssfResults)
targets::tar_load(directEstimates)

areaResults <- multiverseHabitat::parse_combined_results(areaResults)
ssfResults <- multiverseHabitat::parse_combined_results(ssfResults)

library(dplyr)
library(here)
library(ggplot2)
library(ggridges)

palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
names(palette) <- c("KINGCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

# remove the spcies and indi from the branches as they are acting as our repeats,
# that way each branch (y axis) has species*indi points
areaResults$choices <- sub("BADGER|VULTURE|KINGCOBRA", "", areaResults$branches)
areaResults$choices <- sub(".{3}$", "", areaResults$choices)

areaResults %>%
  group_by(choices) %>%
  mutate(meanEst = mean(Estimate, na.rm = TRUE),
         medEst = median(Estimate, na.rm = TRUE),
         minEst = min(Estimate, na.rm = TRUE),
         maxEst = max(Estimate, na.rm = TRUE),
         analysis = case_when(
           analysis == "wides" ~ "Wides",
           analysis == "rsf" ~ "Resource Selection Function",
           analysis == "ssf" ~ "(Integrated) Step Selection Function",
           TRUE ~ analysis)
  ) %>%
  ggplot(aes(y = reorder(choices, Estimate, FUN = median, na.rm = TRUE))) +
  # geom_segment(aes(x = minEst, xend = maxEst, yend = choices),
  #              alpha = 0.1, linewidth = 0.25) +
  geom_point(aes(x = Estimate, colour = sigColour),
             position = position_jitter(height = 0.01, width = 0),
             size = 0.75, alpha = 0.25) +
  geom_point(aes(x = medEst), colour = "#000000", size = 1) +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.45, colour = "#403F41",
             linetype = 2) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#E87D13", "#403F41", "#4F0E99"), na.value = "#403F41") +
  labs(y = "Choice path", x = "Estimated habitat preference for C2") +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    strip.background = element_blank(),
    strip.text = element_text(face = 4, hjust = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank())


# spec curve adaptation ---------------------------------------------------
# https://hal.inria.fr/hal-03558950/document has great ideas

rsfResults <- areaResults %>%
  filter(analysis == "rsf")

library(reshape2)
library(patchwork)

rsfResults$tf <- round(rsfResults$tf, digits = 2)
rsfResults$weighting <- round(rsfResults$weighting, digits = 0)

levelOrdering <- unique(c(sort(unique(rsfResults$td)),
                          sort(unique(rsfResults$tf)),
                          unique(rsfResults$area),
                          sort(unique(rsfResults$contour)),
                          sort(unique(rsfResults$availPointsPer)),
                          sort(unique(rsfResults$weighting))))

rsfResultsPlotData <- rsfResults %>%
  select(Estimate, indi, species, td,  tf, area, contour, availPointsPer, weighting, sigColour) %>%
  melt(c("Estimate", "indi", "species", "sigColour")) %>%
  mutate(
    variable = case_when(
      variable == "td" ~ "Tracking Duration (days)",
      variable == "tf" ~ "Tracking Frequency (points/hour)",
      variable == "area" ~ "Available Area Method",
      variable == "contour" ~ "Available Area Contour (%)",
      variable == "availPointsPer" ~ "Available Points Multipiler",
      variable == "weighting" ~ "Weighting of Available Points"
    ),
    indi = as.factor(indi),
    species = as.factor(species),
    value = factor(value, levels = levelOrdering)) %>%
  group_by(variable, value) %>%
  mutate(d_medEst = Estimate - median(rsfResults$Estimate, na.rm = TRUE)) %>%
  ungroup()

medData <- rsfResultsPlotData %>%
  group_by(variable, value) %>%
  summarise(medEst = median(Estimate, na.rm = TRUE))

nSummary <- rsfResultsPlotData %>%
  mutate(bunch = case_when(
    Estimate > 7.5 ~ 18.5,
    Estimate < -7.5 ~ -33,
    TRUE ~ -8
  )) %>%
  group_by(bunch, variable, value) %>%
  summarise(n = n())

(splitSpecCurve_rsf <- rsfResultsPlotData %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = value, colour = d_medEst),
               position = position_jitter(width = 0, height = 0.2), alpha = 0.05) +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1.5, colour = "#FFFFFF") +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1, colour = "#403F41") +
    geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
               linetype = 2) +
    facet_wrap(.~variable, ncol = 1, scales = "free_y", strip.position = "left") +
    labs(y = "", x = "Estimate") +
    # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
    scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      axis.text.y.left = element_text(margin = margin(0,-140,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      legend.position = "none",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())
)

splitSpecCurve_rsf_numbers <- splitSpecCurve_rsf +
    geom_text(data = nSummary, aes(x = bunch, y = value, label = n), fontface = 3)

overallMed <- data.frame("medEst" = median(rsfResults$Estimate, na.rm = TRUE),
                         "indexLoc" = round(nrow(rsfResults)/2, digits = 0))

(overallSpecCurve_rsf <- rsfResults %>%
    arrange(Estimate) %>%
    mutate(index = row_number(),
           indi = as.factor(indi),
           species = as.factor(species),
           d_medEst = Estimate - median(rsfResults$Estimate, na.rm = TRUE)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = index, colour = d_medEst),
               size = 1, alpha = 0.2)+
    geom_point(data = data.frame("medEst" = median(rsfResults$Estimate, na.rm = TRUE),
                                 "indexLoc" = round(nrow(rsfResults)/2, digits = 0)),
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2.5, colour = "#FFFFFF") +
    geom_point(data = data.frame("medEst" = median(rsfResults$Estimate, na.rm = TRUE),
                                 "indexLoc" = round(nrow(rsfResults)/2, digits = 0)),
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2, colour = "#403F41") +
    annotate("text", x = overallMed$medEst +14, y = overallMed$indexLoc, label = "Median",
             fontface = 4, size = 5, colour = palette["coreGrey"],
             hjust = 1, vjust = -0.2) +
    annotate("segment", x = overallMed$medEst +14, xend = overallMed$medEst,
             y = overallMed$indexLoc, yend = overallMed$indexLoc,
             linewidth = 0.75, colour = palette["coreGrey"]) +
    scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
    labs(y = "", x = "Estimate") +
    theme_bw() +
      theme(
        line = element_line(colour = palette["coreGrey"]),
        text = element_text(colour = palette["coreGrey"]),
        strip.background = element_blank(),
        strip.text = element_text(face = 4, hjust = 1, vjust = 1),
        strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
        axis.text.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.line.x = element_line(),
        strip.clip = "off",
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(18, "pt"),
        panel.grid = element_blank())
)

# (overallSpecCurve_rsf_flat <- rsfResults %>%
#     arrange(Estimate) %>%
#     mutate(index = row_number(),
#            indi = as.factor(indi),
#            species = as.factor(species),
#            d_medEst = Estimate - median(rsfResults$Estimate, na.rm = TRUE)) %>%
#     ggplot() +
#     geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
#                linetype = 1) +
#     geom_point(aes(x = Estimate, y = 1, colour = d_medEst),
#                size = 1, alpha = 0.2)+
#     scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
#     labs(y = "", x = "Estimate") +
#     theme_bw() +
#     theme(
#       line = element_line(colour = palette["coreGrey"]),
#       text = element_text(colour = palette["coreGrey"]),
#       strip.background = element_blank(),
#       strip.text = element_text(face = 4, hjust = 1, vjust = 1),
#       strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
#       axis.text.y.left = element_blank(),
#       axis.ticks.y.left = element_blank(),
#       axis.line.x = element_line(),
#       strip.clip = "off",
#       legend.position = "none",
#       panel.border = element_blank(),
#       panel.spacing = unit(18, "pt"),
#       panel.grid = element_blank())
# )

(rsfSpecComplete <- overallSpecCurve_rsf / splitSpecCurve_rsf +
    plot_layout(heights = c(1, 3), guides = "collect"))

ggsave(filename = here("notebook", "figures", "rsfSpecCurve_split.png"),
       plot = splitSpecCurve_rsf,
       width = 360, height = 240, units = "mm", dpi = 300)
# ggsave(filename = here("notebook", "figures", "rsfSpecCurve_split.pdf"),
#        plot = splitSpecCurve_rsf,
#        width = 360, height = 240, units = "mm", device = cairo_pdf)

ggsave(filename = here("notebook", "figures", "rsfSpecCurve_split_numbers.png"),
       plot = splitSpecCurve_rsf_numbers,
       width = 360, height = 240, units = "mm", dpi = 300)
# ggsave(filename = here("notebook", "figures", "rsfSpecCurve_split_numbers.pdf"),
#        plot = splitSpecCurve_rsf_numbers,
#        width = 360, height = 240, units = "mm", device = cairo_pdf)

ggsave(filename = here("notebook", "figures", "rsfSpecCurve_overall.png"),
       plot = overallSpecCurve_rsf,
       width = 200, height = 140, units = "mm", dpi = 300)
ggsave(filename = here("notebook", "figures", "rsfSpecCurve_overall_flat.png"),
       plot = overallSpecCurve_rsf_flat,
       width = 200, height = 60, units = "mm", dpi = 300)
# ggsave(filename = here("notebook", "figures", "rsfSpecCurve_overall.pdf"),
#        plot = overallSpecCurve_rsf,
#        width = 360, height = 240, units = "mm", device = cairo_pdf)
ggsave(filename = here("notebook", "figures", "rsfSpecCurve.png"),
       plot = rsfSpecComplete,
       width = 360, height = 240, units = "mm", dpi = 300)
# ggsave(filename = here("notebook", "figures", "rsfSpecCurve.pdf"),
#        plot = rsfSpecComplete,
#        width = 360, height = 240, units = "mm", device = cairo_pdf)


# wides spec curve --------------------------------------------------------

widesResults <- areaResults %>%
  filter(analysis == "wides")

library(reshape2)
library(patchwork)

widesResults$tf <- round(widesResults$tf, digits = 2)

levelOrdering <- unique(c(sort(unique(widesResults$td)),
                          sort(unique(widesResults$tf)),
                          unique(widesResults$area),
                          sort(unique(widesResults$contour)),
                          sort(unique(widesResults$availPointsPer)),
                          sort(unique(widesResults$weighting))))

widesResultsPlotData <- widesResults %>%
  select(Estimate, indi, species, td,  tf, area, contour, availPointsPer, sigColour) %>%
  melt(c("Estimate", "indi", "species", "sigColour")) %>%
  mutate(
    variable = case_when(
      variable == "td" ~ "Tracking Duration (days)",
      variable == "tf" ~ "Tracking Frequency (points/hour)",
      variable == "area" ~ "Available Area Method",
      variable == "contour" ~ "Available Area Contour (%)",
      variable == "availPointsPer" ~ "Available Points Multipiler"
    ),
    indi = as.factor(indi),
    species = as.factor(species),
    value = factor(value, levels = levelOrdering))

medData <- widesResultsPlotData %>%
  group_by(variable, value) %>%
  summarise(medEst = median(Estimate, na.rm = TRUE),
            nZeroNA = sum(Estimate == 0 | is.na(Estimate)))

(splitSpecCurve_wides <- widesResultsPlotData %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = value),
               position = position_jitter(width = 0, height = 0.2),
               alpha = 0.2, colour = palette["coreGrey"]) +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1.5, colour = "#FFFFFF") +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1, colour = "#403F41") +
    geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
               linetype = 2) +
    geom_text(data = medData, aes(x = -1, y = value, label = nZeroNA),
              colour = palette["coreGrey"], fontface = 4, hjust = 1) +
    facet_wrap(.~variable, ncol = 1, scales = "free_y", strip.position = "left") +
    labs(y = "", x = "Estimate") +
    # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      axis.text.y.left = element_text(margin = margin(0,-145,0,100)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())
)

overallMed <- data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                         "indexLoc" = round(nrow(widesResults)/2, digits = 0))

(overallSpecCurve_wides <- widesResults %>%
    arrange(Estimate) %>%
    mutate(index = row_number(),
           indi = as.factor(indi),
           species = as.factor(species)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = index),
               size = 1, alpha = 0.2, colour = palette["coreGrey"])+
    geom_point(data = data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                                 "indexLoc" = round(nrow(widesResults)/2, digits = 0)),
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2.5, colour = "#FFFFFF") +
    geom_point(data = data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                                 "indexLoc" = round(nrow(widesResults)/2, digits = 0)),
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2, colour = "#403F41") +
    geom_text(data = medData, aes(x = -1, y = 1, label = sum(nZeroNA)),
              colour = palette["coreGrey"], fontface = 4, hjust = 1) +
    annotate("text", x = overallMed$medEst +10, y = overallMed$indexLoc, label = "Median",
             fontface = 4, size = 5, colour = palette["coreGrey"],
             hjust = 1, vjust = -0.2) +
    annotate("segment", x = overallMed$medEst +10, xend = overallMed$medEst,
             y = overallMed$indexLoc, yend = overallMed$indexLoc,
             linewidth = 0.75, colour = palette["coreGrey"]) +
    # geom_vline(data = directEstimates %>%
    #              filter(Method == "wides", scale == "movement"),
    #            aes(xintercept = Estimate)) +
    # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
    labs(y = "", x = "Estimate") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())
)

(widesSpecComplete <- overallSpecCurve_wides / splitSpecCurve_wides +
    plot_layout(heights = c(1, 3), guides = "collect"))

ggsave(filename = here("notebook", "figures", "widesSpecCurve.png"),
       plot = widesSpecComplete,
       width = 360, height = 240, units = "mm", dpi = 300)


# ssf spec curve ----------------------------------------------------------

ssfResults

library(reshape2)
library(patchwork)

ssfResults$tf <- round(ssfResults$tf, digits = 2)

levelOrdering <- unique(c("5", sort(unique(ssfResults$td)), # 5 is added manually as it appears in two variables
                          sort(unique(ssfResults$availablePerStep)),
                          sort(unique(ssfResults$tf)),
                          unique(ssfResults$modelForm),
                          unique(ssfResults$stepDist),
                          unique(ssfResults$turnDist)
                          ))

ssfResultsPlotData <- ssfResults %>%
  select(Estimate, indi, species, td,  tf, modelForm, stepDist, turnDist, availablePerStep,
         sigColour) %>%
  melt(c("Estimate", "indi", "species", "sigColour")) %>%
  mutate(
    variable = case_when(
      variable == "td" ~ "Tracking Duration (days)",
      variable == "tf" ~ "Tracking Frequency (points/hour)",
      variable == "modelForm" ~ "Model formula (iSSF or SSF)",
      variable == "stepDist" ~ "Distribution of step lengths",
      variable == "turnDist" ~ "Distribution of turn angles",
      variable == "availablePerStep" ~ "Available points per step"
    ),
    indi = as.factor(indi),
    species = as.factor(species),
    value = factor(value, levels = levelOrdering))

medData <- ssfResultsPlotData %>%
  group_by(variable, value) %>%
  summarise(medEst = median(Estimate, na.rm = TRUE))

(splitSpecCurve_ssf <- ssfResultsPlotData %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = value, colour = sigColour),
               position = position_jitter(width = 0, height = 0.2), alpha = 0.2) +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1.5, colour = "#FFFFFF") +
    geom_point(data = medData, aes(x = medEst, y = value),
               alpha = 1, size = 1, colour = "#403F41") +
    geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
               linetype = 2) +
    facet_wrap(.~variable, ncol = 1, scales = "free_y", strip.position = "left") +
    labs(y = "", x = "Estimate") +
    scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())
)

overallMed <- data.frame("medEst" = median(ssfResults$Estimate, na.rm = TRUE),
                         "indexLoc" = round(nrow(ssfResults)/2, digits = 0))

(overallSpecCurve_ssf <- ssfResults %>%
    arrange(Estimate) %>%
    mutate(index = row_number(),
           indi = as.factor(indi),
           species = as.factor(species)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = index, colour = sigColour),
               size = 1, alpha = 0.2)+
    geom_point(data = data.frame("medEst" = median(ssfResults$Estimate, na.rm = TRUE),
                                 "indexLoc" = round(nrow(ssfResults)/2, digits = 0)),
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2.5, colour = "#FFFFFF") +
    geom_point(data = overallMed,
               aes(x = medEst, y = indexLoc),
               alpha = 1, size = 2, colour = "#403F41") +
    annotate("text", x = overallMed$medEst -5, y = overallMed$indexLoc, label = "Median",
             fontface = 4, size = 5, colour = palette["coreGrey"],
             hjust = 0, vjust = -0.2) +
    annotate("segment", x = overallMed$medEst -5, xend = overallMed$medEst,
             y = overallMed$indexLoc, yend = overallMed$indexLoc,
             linewidth = 0.75, colour = palette["coreGrey"]) +
    geom_vline(data = directEstimates %>%
                 filter(Method == "wides", scale == "movement"),
               aes(xintercept = Estimate)) +
    scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
    labs(y = "", x = "Estimate") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      axis.text.y.left = element_blank(),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())
)

(ssfSpecComplete <- overallSpecCurve_ssf / splitSpecCurve_ssf +
    plot_layout(heights = c(1, 3), guides = "collect"))

ggsave(filename = here("notebook", "figures", "ssfSpecCurve.png"),
       plot = ssfSpecComplete,
       width = 360, height = 240, units = "mm", dpi = 300)
