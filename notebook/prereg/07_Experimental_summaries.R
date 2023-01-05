
# summary plots draft -----------------------------------------------------

targets::tar_load(combinedResults)
# combResults <- combinedResults
combinedResults <- multiverseHabitat::parse_combined_results(combinedResults)

library(ggplot2)
library(dplyr)
library(ggridges)

palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
names(palette) <- c("KING\nCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

combinedResults %>%
  arrange(td) %>%
  # mutate(branches = factor(branches,
  #                          levels = branches)) %>%
  ggplot() +
  # geom_segment(aes(x = Estimate - SE, xend = Estimate + SE,
  #                  y = branches, yend = branches, colour = sigColour)) +
  geom_point(aes(x = Estimate, y = td, colour = sigColour),
             position = position_jitter(height = 0.1)) +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.45, colour = "#403F41",
             linetype = 2) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("red", "grey25", "blue"), na.value = "#403F41") +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank())

# remove the spcies and indi from the branches as they are acting as our repeats,
# that way each branch (y axis) has species*indi points
combinedResults$choices <- sub("BADGER|VULTURE|KINGCOBRA", "", combinedResults$branches)
combinedResults$choices <- sub(".{3}$", "", combinedResults$choices)

combinedResults %>%
  group_by(choices) %>%
  mutate(meanEst = mean(Estimate, na.rm = TRUE),
         minEst = min(Estimate, na.rm = TRUE),
         maxEst = max(Estimate, na.rm = TRUE),
         analysis = case_when(
           analysis == "wides" ~ "Wides",
           analysis == "rsf" ~ "Resource Selection Function",
           analysis == "ssf" ~ "(Integrated) Step Selection Function",
           TRUE ~ analysis)
  ) %>%
  ggplot(aes(y = reorder(choices, Estimate, FUN = mean, na.rm = TRUE))) +
  geom_segment(aes(x = minEst, xend = maxEst, yend = choices),
               alpha = 0.1, linewidth = 0.25) +
  geom_point(aes(x = Estimate, colour = sigColour),
             position = position_jitter(height = 0.01, width = 0),
             size = 0.75) +
  geom_point(aes(x = meanEst)) +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.45, colour = "#403F41",
             linetype = 2) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#E87D13", "#403F41", "#4F0E99"), na.value = "#000000") +
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

rsfResults <- combinedResults %>%
  filter(analysis == "rsf")

library(reshape2)
library(patchwork)

levelOrdering <- unique(c(sort(unique(rsfResults$td)),
                          sort(unique(rsfResults$tf)),
                          unique(rsfResults$area),
                          sort(unique(rsfResults$contour)),
                          sort(unique(rsfResults$availPointsPer)),
                          sort(unique(rsfResults$weighting))))

(splitSpecCurve <- rsfResults %>%
    select(Estimate, indi, species, td,  tf, area, contour, availPointsPer, weighting) %>%
    melt(c("Estimate", "indi", "species")) %>%
    mutate(
      variable = case_when(
        variable == "td" ~ "Tracking Duration (days)",
        variable == "tf" ~ "Tracking Frequency (hours)",
        variable == "area" ~ "Available Area Method",
        variable == "contour" ~ "Available Area Contour (%)",
        variable == "availPointsPer" ~ "Available Points Multipiler",
        variable == "weighting" ~ "Weighting of Used Points"
      ),
      indi = as.factor(indi),
      species = as.factor(species),
      value = factor(value, levels = levelOrdering)
    ) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = value, colour = indi, shape = species),
               position = position_jitter(width = 0, height = 0.2), alpha = 0.2) +
    geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
               linetype = 2) +
    facet_wrap(.~variable, ncol = 1, scales = "free_y", strip.position = "left") +
    labs(y = "", x = "Estimate") +
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

(overallSpecCurve <- rsfResults %>%
    arrange(Estimate) %>%
    mutate(index = row_number(),
           indi = as.factor(indi),
           species = as.factor(species)) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_point(aes(x = Estimate, y = index, colour = indi, shape = species),
               size = 1)+
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

overallSpecCurve / splitSpecCurve +
  plot_layout(heights = c(1, 3))

