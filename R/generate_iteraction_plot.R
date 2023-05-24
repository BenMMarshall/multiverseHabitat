#' Generate interaction plot needed for RMD manuscript
#'
#' @name generate_iteraction_plot
#' @description Generate interaction plot needed for RMD manuscript
#' @return Saves a png and pdf files.
#'
#' @export
generate_iteraction_plot <- function(){

  areaBrms_rsf <- qs::qread(here::here("_targets", "objects", "areaBrms_rsf"))

  palette <- multiverseHabitat::get_palette()

  # rsf interations ---------------------------------------------------------

  intConditions <- list(
    area = c("dBBMM", "AKDE", "MCP", "KDEhref")
  )

  tdAreaData <- brms::conditional_effects(areaBrms_rsf$modOUT_dEst, effects = "tdScaled:area",
                                          int_conditions = intConditions, points = TRUE)

  endLabelstd <- tdAreaData$`tdScaled:area` %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(maxx = max(tdScaled),
              miny = min(estimate__))

  tdAreaPlot <- tdAreaData$`tdScaled:area` %>%
    ggplot() +
    geom_line(aes(x = tdScaled, y = estimate__, colour = area)) +
    geom_ribbon(aes(x = tdScaled,
                    ymin = lower__, ymax = upper__,
                    fill = area), alpha = 0.1) +
    # geom_text(data = endLabelstd, aes(x = maxx, y = miny,
    #                                 label = area, colour = area),
    #           vjust = 0.5, hjust = 0) +
    labs(y = "Estimated effect", x = "Tracking Duration (scaled)",
         fill = "Area method", colour = "Area method") +
    scale_colour_manual(values = unname(palette[c("2", "BADGER", "1", "KINGCOBRA")])) +
    scale_fill_manual(values = unname(palette[c("2", "BADGER", "1", "KINGCOBRA")])) +
    coord_cartesian(ylim = c(-0.25,4))+
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

  tfAreaData <- brms::conditional_effects(areaBrms_rsf$modOUT_dEst, effects = "tfScaled:area",
                                          int_conditions = intConditions, points = TRUE)

  endLabelstf <- tfAreaData$`tfScaled:area` %>%
    dplyr::group_by(area) %>%
    dplyr::summarise(maxx = max(tfScaled),
              miny = min(estimate__))

  tfAreaPlot <- tfAreaData$`tfScaled:area` %>%
    ggplot() +
    geom_line(aes(x = tfScaled, y = estimate__, colour = area)) +
    geom_ribbon(aes(x = tfScaled,
                    ymin = lower__, ymax = upper__,
                    fill = area), alpha = 0.1) +
    # geom_text(data = endLabelstf, aes(x = maxx, y = miny,
    #                                 label = area, colour = area),
    #           vjust = 0.5, hjust = 0) +
    labs(y = "Estimated effect", x = "Tracking Frequency (scaled)",
         fill = "Area method", colour = "Area method") +
    scale_colour_manual(values = unname(palette[c("2", "BADGER", "1", "KINGCOBRA")])) +
    scale_fill_manual(values = unname(palette[c("2", "BADGER", "1", "KINGCOBRA")])) +
    coord_cartesian(ylim = c(-0.25,4))+
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      axis.line.x = element_line(),
      axis.line.y = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

  (bothIterations <- patchwork::wrap_plots(tdAreaPlot + tfAreaPlot) +
      patchwork::plot_layout(guides = "collect"))

  ggsave(bothIterations,
         filename = here("notebook", "figures", "rsfEffectPlot_iteractions.png"),
         dpi = 300, width = 210, height = 100,
         units = "mm")
  ggsave(bothIterations,
         filename = here("notebook", "figures", "rsfEffectPlot_iteractions.pdf"),
         width = 210, height = 100,
         units = "mm", device = cairo_pdf)

}
