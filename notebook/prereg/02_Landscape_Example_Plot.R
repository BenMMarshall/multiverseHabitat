library(here)
library(ggplot2)
library(ggtext)
library(patchwork)
library(NLMR)
library(reshape2)

RandomFields::RFoptions(install="no")

seed <- 2022
set.seed(seed)
palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010")
names(palette) <- c("KING\nCOBRA", "VULTURE", "BADGER", "2", "1", "0")

row = 2000; col = 2000

gf1 <- suppressMessages(NLMR::nlm_gaussianfield(ncol = col,
                                                nrow = row,
                                                resolution = 1,
                                                autocorr_range = 40,
                                                mag_var = 5,
                                                nug = 0.2,
                                                mean = 0.5,
                                                user_seed = seed,
                                                rescale = TRUE))

forageQual <- gf1

forageQual[forageQual[] < 0.4] <- 0
# set min 0 max 1, normalise the values between 1 and 0
forageQual[] <- (forageQual[] - min(forageQual[], na.rm = TRUE)) /
  (max(forageQual[], na.rm = TRUE) - min(forageQual[], na.rm = TRUE))

moveQual <- gf1
# areas with high resources are accessible (> 0.6, increased by 0.5), but the
# fastest least resistance routes are actually edge habitat areas (0.6 to 0.3,
# increased by 1). Core areas of low resrouce are also difficult to move
# through.
moveQual[moveQual[] > 0.6] <- moveQual[moveQual[] > 0.6] + 0.5
moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] <-
  moveQual[moveQual[] < 0.6 & moveQual[] > 0.3] + 1
moveQual[] <- (moveQual[] - min(moveQual[], na.rm = TRUE)) /
  (max(moveQual[], na.rm = TRUE) - min(moveQual[], na.rm = TRUE))


shelterQual <- gf1
# shelter sites are best found near the edge of high resource areas, but deeper than the best movement routes
shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] <-
  shelterQual[shelterQual[] < 0.7 & shelterQual[] > 0.5] + 1
shelterQual[] <- (shelterQual[] - min(shelterQual[], na.rm = TRUE)) /
  (max(shelterQual[], na.rm = TRUE) - min(shelterQual[], na.rm = TRUE))

landscapeLayersList <- list(
  "shelter" = matrix(data = raster::getValues(shelterQual),
                     nrow = row,
                     ncol = col),
  "forage" = matrix(data = raster::getValues(forageQual),
                    nrow = row,
                    ncol = col),
  "movement" = matrix(data = raster::getValues(moveQual),
                      nrow = row,
                      ncol = col))

classLandscape <- gf1
 # > 0.6
 # <= 0.6 & > 0.3
 # <= 0.3
classLandscape[classLandscape[] > 0.6] <- 2
classLandscape[classLandscape[] <= 0.6 & classLandscape[] > 0.3] <- 1
classLandscape[classLandscape[] <= 0.3] <- 0

classLandscapeList <- list(
  "classified" = matrix(data = raster::getValues(classLandscape),
                        nrow = row,
                        ncol = col))

save(classLandscapeList, file = here("notebook", "prereg", "landscapePrelim.RData"))

plot_landscapeLayersList <- function(targetList){

  combinedLayers <- do.call(rbind, lapply(names(targetList), function(x){
    currLayer <- targetList[[x]]
    layerDF <- reshape2::melt(currLayer, c("col", "row"))
    layerDF$layer <- x
    return(layerDF)
  }))
  combinedLayers$layer <- factor(combinedLayers$layer, levels = c("shelter", "forage", "movement"))

  ggplot(combinedLayers) +
    geom_raster(aes(x = col, y = row, fill = value)) +
    facet_wrap(.~layer,
               labeller = as_labeller(facetLabels <- c(
                 shelter = "<span style='color:#302010'>Shelter Quality</span>",
                 forage = "<span style='color:#965A1D'>Foraging Resources</span>",
                 movement = "<span style='color:#E87D13'>Movement Ease</span>"
                 ))
    ) +
    scale_x_continuous(breaks = seq(0, 2000, 500)) +
    scale_y_continuous(breaks = seq(0, 2000, 500)) +
    coord_fixed(expand = 0.001) +
    scale_fill_gradient2(high = palette["2"],
                         mid = palette["1"],
                         low = palette["0"],
                         midpoint = 0.5,
                         breaks = seq(0,1,0.2),
                         label = seq(0,1,0.2),
                         limits = c(0,1),
                         expand = c(0,0))+
    labs(x = "", y = "",
         fill = "<b>Weighting</b><br>
         <i><span style='font-size:6pt'>(Weighting: higher values are more likely to be chosen)</span></i>") +
    theme_bw() +
    theme(
      text = element_text(colour = "#191919"),
      line = element_line(colour = "#808080"),
      axis.text = element_text(size = 5, colour = "#808080"),
      axis.title = element_markdown(size = 7, face = 2),
      axis.title.x = element_markdown(angle = 0, hjust = 0, vjust = 0,
                                      margin = margin(t = 10, r = 0, b = 0, l = 0)),
      axis.title.y = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                      margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.ticks = element_line(size = 0.5),
      axis.ticks.length = unit(1.5, "mm"),
      axis.line = element_line(size = 0.5),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_markdown(size = 10, face = 4,
                                    hjust = 0),
      legend.position = "bottom",
      legend.title.align = 0.5,
      legend.title = element_markdown(size = 8)) +
    guides(fill = guide_colourbar(
      direction = "horizontal",
      title.position = "top",
      label.hjust = 0.5,
      label.vjust = 1,
      nbin = 100,
      draw.llim = FALSE,
      draw.ulim = FALSE,
      ticks.linewidth = unit(0.5, "mm"),
      ticks.colour = "#191919",
      barwidth = unit(70, "mm"),
      barheight = unit(3, "mm")))
}

simlandscapesPlot <- plot_landscapeLayersList(landscapeLayersList)

layerDF <- reshape2::melt(classLandscapeList$classified, c("col", "row"))
layerDF$layer <- "classified"
classLayer <- layerDF

habitatPlot <- ggplot(classLayer) +
  geom_raster(aes(x = col, y = row, fill = value)) +
  facet_wrap(.~layer,
             labeller = as_labeller(facetLabels <- c(
               classified = "<span style='color:#4F0E99'>Classified Habitats</span>"
             ))
  ) +
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  scale_y_continuous(breaks = seq(0, 2000, 500)) +
  coord_fixed(expand = 0.001) +
  scale_fill_gradient2(high = palette["BADGER"],
                       mid = palette["VULTURE"],
                       low = palette["KING\nCOBRA"],
                       midpoint = 0.5,
                       breaks = seq(0,2,1),
                       label = seq(0,2,1),
                       limits = c(0,2),
                       expand = c(0,0))+
  labs(x = "", y = "",
       fill = "<b>Habitat Classes</b><br>
         <i><span style='font-size:6pt'>(Classes: 0, 1, and 2)</span></i>") +
  theme_bw() +
  theme(
    text = element_text(colour = "#191919"),
    line = element_line(colour = "#808080"),
    axis.text = element_text(size = 5, colour = "#808080"),
    axis.title = element_markdown(size = 7, face = 2),
    axis.title.x = element_markdown(angle = 0, hjust = 0, vjust = 0,
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_markdown(angle = 0, hjust = 1, vjust = 1,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
    axis.ticks = element_line(size = 0.5),
    axis.ticks.length = unit(1.5, "mm"),
    axis.ticks.y = element_blank(),
    axis.line = element_line(size = 0.5),
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_markdown(size = 10, face = 4,
                                  hjust = 0),
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.title = element_markdown(size = 8)) +
  guides(fill = guide_colourbar(
    direction = "horizontal",
    title.position = "top",
    label.hjust = 0.5,
    label.vjust = 1,
    nbin = 100,
    draw.llim = FALSE,
    draw.ulim = FALSE,
    ticks.linewidth = unit(0.5, "mm"),
    ticks.colour = "#191919",
    barwidth = unit(35, "mm"),
    barheight = unit(3, "mm")))

layout <- c(
  patchwork::area(t = 0, l = 0, b = 1, r = 4),
  patchwork::area(t = 0, l = 4.2, b = 1, r = 5.2)
)
simlandscapesPlot + habitatPlot +
  plot_layout(design = layout)

ggsave(filename = here("notebook", "prereg", "figures", "landscapeExample.png"),
       width = 240, height = 120, dpi = 300, units = "mm")
