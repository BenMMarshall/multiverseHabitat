#' Generate combined effect plots
#'
#' @name generate_allEffect_plots
#' @description A
#' @param modelExtracts output tar_targets resulting in ssfBrms or areaBrms
#' @return a
#'
#' @export
generate_allEffect_plots <- function(modelExtracts){

  # modelExtracts <- extractedValues

  palette <- multiverseHabitat::get_palette()
  modelPalette <- unname(palette[1:4])
  names(modelPalette) <- c(
    "<b style='color:#AD6DED'>Wides</b>",
    "<b style='color:#7D26D4'>RSF</b>",
    "<b style='color:#4F0E99'>SSF</b>",
    "<b style='color:#E87D13'>wRSF</b>"
    )
    # "#AD6DED"
    # "#7D26D4"
    # "#4F0E99"
    # "#E87D13"

  betasOutputs <- modelExtracts$betasOutputs

  betasOutputsPlotData <- betasOutputs %>%
    filter(!str_detect(.variable, ":")) %>%
    mutate(facetSplit = factor(case_when(
      .variable %in% c("b_Intercept",
                       "b_tdScaled",
                       "b_tfScaled") ~  "Sampling Choices",
      .variable %in% c("b_availablePerStepScaled",
                       "b_stepDistgamma",
                       "b_turnDistvonmises",
                       "b_modelFormmf.ss") ~  "<b style='color:#4F0E99'>SSF Only</b>",
      .variable %in% c("b_areaMCP",
                       "b_areaAKDE",
                       "b_areaKDEhref",
                       "b_areadBBMM",
                       "b_contourScaled",
                       "b_weightingScaled",
                       "b_availPointsPerScaled",
                       "b_samplingPatternst") ~  "<b style='color:#E87D13'>Area Based Only</b>",
    ), levels = c(
      "Sampling Choices",
      "<b style='color:#4F0E99'>SSF Only</b>",
      "<b style='color:#E87D13'>Area Based Only</b>")
    )) %>%
    mutate(
      .variable = factor(case_when(
        .variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
        .variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
        .variable == "b_modelFormmf.ss" ~ "\u03B2 Model Formula: Not Integrated",
        .variable == "b_stepDistgamma" ~ "\u03B2 Step Distribution: Gamma",
        .variable == "b_turnDistvonmises" ~ "\u03B2 Turn Distribution: Von Mises",
        .variable == "b_availablePerStepScaled" ~ "\u03B2 Available Points Per Step",
        .variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
        .variable == "b_areaAKDE" ~ "\u03B2 Available Area: AKDE",
        .variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDE href",
        .variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
        .variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified",
        .variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
        .variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
        .variable == "b_weightingScaled" ~ "\u03B2 Available Points Weighting"
      ),
      levels = rev(c(
        "\u03B2 Sample Size",
        "\u03B2 Tracking Duration",
        "\u03B2 Tracking Frequency",

        "\u03B2 Available Points Per Step",
        "\u03B2 Step Distribution: Gamma",
        "\u03B2 Turn Distribution: Von Mises",
        "\u03B2 Model Formula: Not Integrated",

        "\u03B2 Available Area: MCP",
        "\u03B2 Available Area: AKDE",
        "\u03B2 Available Area: KDE href",
        "\u03B2 Available Area: dBBMM",
        "\u03B2 Available Area Contour",
        "\u03B2 Available Points Multipiler",
        "\u03B2 Sampling Pattern: Stratified",
        "\u03B2 Available Points Weighting"
      ))
      )) %>%
    mutate(rawAbs = str_extract(model, "dEst|rEst"),
           method = str_extract(model, "Wides|RSF|SSF|wrsf")) %>%
    mutate(method = factor(case_when(
      method == "Wides" ~ "<b style='color:#AD6DED'>Wides</b>",
      method == "RSF" ~ "<b style='color:#7D26D4'>RSF</b>",
      method == "SSF" ~ "<b style='color:#4F0E99'>SSF</b>",
      method == "wrsf" ~ "<b style='color:#E87D13'>wRSF</b>"),
      levels = c(
        "<b style='color:#E87D13'>wRSF</b>",
        "<b style='color:#4F0E99'>SSF</b>",
        "<b style='color:#AD6DED'>Wides</b>",
        "<b style='color:#7D26D4'>RSF</b>"
      ))
    ) %>%
    filter(!.variable == "b_Intercept")


  gradLimits <- range(c(betasOutputsPlotData$.lower, betasOutputsPlotData$.upper))
  labelLocation <- data.frame(gradLimits)
  labelLocationText <- data.frame(gradIndent = gradLimits + c(0.25, -0.25))
  labelText <- c("Closer to median\npreference estimate",
                 "Farther from median\npreference estimate")
  arrowAdj <- c(0.015, -0.015)
  facetSplit <- factor(rep("<b style='color:#E87D13'>Area Based Only</b>", 2), levels = c(
    "Sampling Choices",
    "Step Generation Choices",
    "<b style='color:#4F0E99'>SSF Only</b>",
    "<b style='color:#E87D13'>Area Based Only</b>")
  )
  hjust <- c(1,0)

  annotationDF <- cbind(labelLocation, labelLocationText, labelText, arrowAdj,
                        facetSplit, hjust)

  modelLabels <- tribble(
    ~x, ~y, ~text, ~hjust, ~vjust, ~facetSplit, ~rawAbs, ~labCol,
    4.35,   -0.65, "<b style='color:#AD6DED'>\u25CF = Wides</b>", 0.5, 0, "<b style='color:#E87D13'>Area Based Only</b>", "rEst", "#AD6DED",
    5.40,   -0.65, "<b style='color:#7D26D4'>\u25B2 = RSF</b>", 0.5, 1, "<b style='color:#E87D13'>Area Based Only</b>", "dEst", "#7D26D4",
    2.55,   -0.78, "<b style='color:#4F0E99'>\u25C6 = Step Selection</b>", 0.5, 0, "<b style='color:#4F0E99'>SSF Only</b>", "rEst", "#4F0E99",
    1.20,   -0.72, "<b style='color:#E87D13'>\u25BC = wRSF</b>", 0.5, 1, "Sampling Choices", "dEst", "#E87D13"
  )
  modelLabels <- modelLabels %>%
    mutate(facetSplit = factor(facetSplit, levels = c(
      "Sampling Choices",
      "<b style='color:#4F0E99'>SSF Only</b>",
      "<b style='color:#E87D13'>Area Based Only</b>")))

  arrowsDF <- tribble(
    ~x, ~y, ~xend, ~yend, ~facetSplit, ~rawAbs, ~labCol,
    4.35,   -0.65, 2.95, -0.02, "<b style='color:#E87D13'>Area Based Only</b>", "rEst", "#AD6DED",
    4.92,   -0.65, 3.35, -0.22, "<b style='color:#E87D13'>Area Based Only</b>", "dEst", "#7D26D4",
    2.55,   -0.75, 0.85, -0.01, "<b style='color:#4F0E99'>SSF Only</b>", "rEst", "#4F0E99",
    1.12,   -0.46, 1.61, -0.28, "Sampling Choices", "dEst", "#E87D13"
  )
  arrowsDF <- arrowsDF %>%
    mutate(facetSplit = factor(facetSplit, levels = c(
      "Sampling Choices",
      "<b style='color:#4F0E99'>SSF Only</b>",
      "<b style='color:#E87D13'>Area Based Only</b>")))

  allEffectsPlot <- betasOutputsPlotData %>%
    ggplot() +
    geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    geom_errorbar(aes(x = .variable, ymin = .lower, ymax = .upper,
                      colour = method),
                  position = position_dodge(0.75), width = 0) +
    geom_vline(xintercept = seq(0.5,20.5,1), linewidth = 0.25, alpha = 0.5, colour = "#403F41",
               linetype = 2) +
    geom_richtext(data = modelLabels,
                  aes(x = x, y = y, label = text,
                      label.color = labCol,
                      hjust = hjust, vjust = vjust),
                  label.size = 0.75, label.r = unit(0.1, "lines"),
                  fill = "#FFFFFF", alpha = 1) +
    geom_curve(data = arrowsDF,
               aes(x = x, xend = xend, y = y, yend = yend,
                   colour = labCol),
               curvature = 0.35,
               arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")))+
    geom_segment(data = annotationDF,
                 aes(x = -0.2, xend = -0.2,
                     y = 0.02, yend = gradIndent + arrowAdj),
                 colour = "#9F9FA0",
                 arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                 linewidth = 1.25) +
    geom_text(data = annotationDF,
              aes(x = -0.1, y = gradIndent,
                  label = labelText, hjust = hjust),
              colour = "#9F9FA0", vjust = 0.5, lineheight = 0.95,
              size = 3, fontface = 4) +
    geom_point(aes(x = .variable, y = .value,
                   colour = method, shape = method, fill = method),
               position = position_dodge(0.75)) +
    geom_point(data = annotationDF,
               aes(x = -0.75, y = 0)) +
    scale_fill_manual(values = modelPalette,
                      breaks = c(
                        "<b style='color:#E87D13'>wRSF</b>",
                        "<b style='color:#4F0E99'>SSF</b>",
                        "<b style='color:#AD6DED'>Wides</b>",
                        "<b style='color:#7D26D4'>RSF</b>")) +
    scale_colour_manual(values = modelPalette,
                        breaks = c(
                          "<b style='color:#E87D13'>wRSF</b>",
                          "<b style='color:#4F0E99'>SSF</b>",
                          "<b style='color:#AD6DED'>Wides</b>",
                          "<b style='color:#7D26D4'>RSF</b>")) +
    scale_shape_manual(values = c(25,23,21,24),
                       breaks = c(
                         "<b style='color:#E87D13'>wRSF</b>",
                         "<b style='color:#4F0E99'>SSF</b>",
                         "<b style='color:#AD6DED'>Wides</b>",
                         "<b style='color:#7D26D4'>RSF</b>")) +
    facet_grid(rows = vars(facetSplit), cols = vars(rawAbs),
               scales = "free", space = "free", switch = "y") +
    coord_flip() +
    labs(x = "", y = "Beta point estimate and 95% HDCI",
         colour = "Model", shape = "Model", fill = "Model") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text.y.left = element_markdown(face = 4, hjust = 0, vjust = 1, angle = 0,
                                           margin = margin(-5,10,0,-152)),
      # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.title.x = element_text(margin = margin(5,0,0,0)),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      legend.position = "none",
      # legend.position = c(0.1, 0.2),
      # legend.title = element_text(face = 4),
      # legend.text = element_markdown(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )

  ggsave(allEffectsPlot,
         filename = here("notebook", "figures", "_allEffectsPlot.png"),
         dpi = 300, width = 260, height = 200,
         units = "mm")

  return(allEffectsPlot)
}