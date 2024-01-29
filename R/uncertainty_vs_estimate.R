#' Generate uncertainty plots
#'
#' @name uncertainty_vs_estimate
#' @description A
#' @param aResults output tar_targets resulting in areaResults
#' @param sResults output tar_targets resulting in ssfResults
#' @return a
#'
#' @export
uncertainty_vs_estimate <- function(aResults, sResults, wResults){

  palette <- multiverseHabitat::get_palette()
  modelPalette <- unname(palette[1:4])
  names(modelPalette) <- c(
    "<b style='color:#AD6DED'>Wides</b>",
    "<b style='color:#7D26D4'>RSF</b>",
    "<b style='color:#4F0E99'>SSF</b>",
    "<b style='color:#E87D13'>wRSF</b>"
  )

  areaResults <- multiverseHabitat::parse_combined_results(aResults)

  rsfResults <- areaResults %>%
    dplyr::filter(analysis == "rsf")

  rsfUncertainEstimate <- rsfResults %>%
    mutate(uncertainty = Upper - Lower) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
               linetype = 2) +
    geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
               linetype = 2) +
    geom_point(aes(x = Estimate, y = uncertainty, colour = Estimate), alpha = 0.25,
               pch = 3, size = 0.75) +
    labs(y = "Range of\n95% CI", x = "Estimate", title = "<b style='color:#7D26D4'>RSF</b>") +
    scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
    scale_y_log10(breaks = c(c(0,1) %o% 10^(1:7))) +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      plot.title = element_markdown(size = 9),
      strip.background = element_blank(),
      axis.title.y = element_text(hjust = 1, vjust = 1,
                                  angle = 0, face = 4, size = 7),
      axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      strip.clip = "off",
      legend.position = "none",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())


  ssfResults <- multiverseHabitat::parse_combined_results(sResults)

  ssfUncertainEstimate <- ssfResults %>%
    mutate(uncertainty = Upper - Lower) %>%
    ggplot() +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
               linetype = 2) +
    geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
               linetype = 2) +
    geom_point(aes(x = Estimate, y = uncertainty, colour = Estimate), alpha = 0.25,
               pch = 3, size = 0.75) +
    labs(y = "Range of\n95% CI", x = "Estimate", title = "<b style='color:#4F0E99'>SSF</b>") +
    scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
    scale_y_log10(breaks = c(c(0,1) %o% 10^(1:7))) +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      plot.title = element_markdown(size = 9),
      strip.background = element_blank(),
      axis.title.y = element_text(hjust = 1, vjust = 1,
                                  angle = 0, face = 4, size = 7),
      axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
      axis.line.y = element_line(),
      axis.line.x = element_line(),
      strip.clip = "off",
      legend.position = "none",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid = element_blank())

  wrsfResults <- multiverseHabitat::parse_combined_results(wResults[!is.na(wResults$Estimate),])

  wrsfUncertainEstimate <- wrsfResults %>%
      mutate(uncertainty = Upper - Lower) %>%
      ggplot() +
      geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
                 linetype = 2) +
      geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
                 linetype = 2) +
      geom_point(aes(x = Estimate, y = uncertainty, colour = Estimate), alpha = 0.5,
                 pch = 3, size = 0.75) +
      labs(y = "Range of\n95% CI", x = "Estimate", title = "<b style='color:#E87D13'>wRSF</b>") +
      scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
      scale_y_log10(breaks = c(c(0,1) %o% 10^(1:7))) +
      theme_bw() +
      theme(
        line = element_line(colour = palette["coreGrey"]),
        text = element_text(colour = palette["coreGrey"]),
        plot.title = element_markdown(size = 9),
        strip.background = element_blank(),
        axis.title.y = element_text(hjust = 1, vjust = 1,
                                    angle = 0, face = 4, size = 7),
        axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
        axis.line.y = element_line(),
        axis.line.x = element_line(),
        strip.clip = "off",
        legend.position = "none",
        panel.border = element_blank(),
        panel.spacing = unit(18, "pt"),
        panel.grid = element_blank())

  (completePlot <-
      (rsfUncertainEstimate +
         theme(axis.title.x = element_blank())) /
      (ssfUncertainEstimate +
         theme(axis.title.y = element_blank(),
               axis.title.x = element_blank())) /
      (wrsfUncertainEstimate +
         theme(axis.title.y = element_blank()))
  )

  ggsave(filename = here("notebook", "figures", "uncertaintyPlot.png"),
         plot = completePlot,
         width = 140, height = 140, units = "mm", dpi = 300)

}

