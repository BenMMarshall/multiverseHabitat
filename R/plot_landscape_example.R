#' Plot the landscapes used for simulation and the classified used for analysis
#'
#' @name plot_landscape_example
#' @description A
#' @param species
#' @param seed
#' @return a
#'
#' @export
plot_landscape_example <- function(
    species = "BADGER", seed = 2022
){

  require(reshape2)
  require(raster)
  require(ggplot2)
  require(ggtext)
  require(patchwork)

  landscape <- multiverseHabitat::simulate_landscape(species = "BADGER", seed = 2022)

  palette <- multiverseHabitat::get_palette()

  print("break 1")

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

  simlandscapesPlot <- plot_landscapeLayersList(landscape[-c(4, 5, 6)])

  layerDF <- reshape2::melt(landscape$classified, c("col", "row"))
  layerDF$layer <- "classified"
  layerDFScram <- reshape2::melt(landscape$classifiedScram, c("col", "row"))
  layerDFScram$layer <- "classifiedScram"

  classLayer <- rbind(layerDF, layerDFScram)

  habitatPlot <- ggplot(classLayer) +
    geom_raster(aes(x = col, y = row, fill = value)) +
    facet_wrap(.~layer,
               labeller = as_labeller(facetLabels <- c(
                 classified = "<span style='color:#4F0E99'>Classified Habitats</span>",
                 classifiedScram = "<span style='color:#7D26D4'>Classified Habitats Scrambled</span>"
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
      axis.ticks = element_line(linewidth = 0.5),
      axis.ticks.length = unit(1.5, "mm"),
      axis.ticks.y = element_blank(),
      axis.line = element_line(linewidth = 0.5),
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

  # layout <- c(
  #   patchwork::area(t = 0.1, l = 0.1, b = 1, r = 4),
  #   patchwork::area(t = 0.1, l = 4.2, b = 1, r = 5.2)
  # )
  # patchwork::wrap_plots(simlandscapesPlot + habitatPlot +
  #                         patchwork::plot_layout(design = layout))
  patchwork::wrap_plots(simlandscapesPlot / habitatPlot)

  ggsave(filename = here::here("notebook", "figures", "landscapeExample.png"),
         width = 240, height = 240, dpi = 300, units = "mm")
}
