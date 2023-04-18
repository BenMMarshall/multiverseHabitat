#' Generate specification curves
#'
#' @name generate_spec_curves
#' @description A
#' @param compiledResults output tar_targets resulting in areaCompiled or ssfCompiled
#' @param method c("rsf", "ssf", "wides")
#' @return a
#'
#' @export
generate_spec_curves <- function(compiledResults, method){

  # library(dplyr)
  # library(here)
  # library(ggplot2)
  # library(ggridges)
  # library(reshape2)
  # library(patchwork)

  if(!method %in% c("rsf", "ssf", "wides", "wrsf")){
    stop("Type must be rsf, ssf, wides, wrsf")
  }

  # spec curve adaptation ---------------------------------------------------
  # https://hal.inria.fr/hal-03558950/document has great ideas
  palette <- multiverseHabitat::get_palette()
  # summary plots draft -----------------------------------------------------

  # targets::tar_load(areaResults)

  if(method == "rsf"){

    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)

    rsfResults <- areaResults %>%
      dplyr::filter(analysis == "rsf")

    rsfResults$tf <- round(rsfResults$tf, digits = 2)
    rsfResults$weighting <- round(rsfResults$weighting, digits = 0)

    rsfResults$samplingPattern <- ifelse(rsfResults$samplingPattern == "rd",
                                         "Random",
                                         "Stratified")

    levelOrdering <- unique(c(sort(unique(rsfResults$td)),
                              sort(unique(rsfResults$tf)),
                              unique(rsfResults$area),
                              sort(unique(rsfResults$contour)),
                              sort(unique(rsfResults$availPointsPer)),
                              sort(unique(rsfResults$samplingPattern)),
                              sort(unique(rsfResults$weighting))))

    rsfResultsPlotData <- rsfResults %>%
      dplyr::select(Estimate, Lower, Upper, indi, species,
                    td,  tf,
                    area, contour, availPointsPer, samplingPattern, weighting, sigColour) %>%
      reshape2::melt(c("Estimate", "Lower", "Upper", "indi", "species", "sigColour")) %>%
      dplyr::mutate(
        variable = case_when(
          variable == "td" ~ "Tracking Duration (days)",
          variable == "tf" ~ "Tracking Frequency (points/hour)",
          variable == "area" ~ "Available Area Method",
          variable == "contour" ~ "Available Area Contour (%)",
          variable == "availPointsPer" ~ "Available Points Multipiler",
          variable == "samplingPattern" ~ "Sampling Pattern",
          variable == "weighting" ~ "Weighting of Available Points"
        ),
        indi = as.factor(indi),
        species = as.factor(species),
        value = factor(value, levels = levelOrdering)) %>%
      dplyr::group_by(variable, value) %>%
      dplyr::mutate(d_medEst = Estimate - median(rsfResults$Estimate, na.rm = TRUE)) %>%
      dplyr::ungroup()

    medData <- rsfResultsPlotData %>%
      dplyr::group_by(variable, value) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE))

    nSummary <- rsfResultsPlotData %>%
      dplyr::mutate(bunch = case_when(
        Estimate > 7.5 ~ 18.5,
        Estimate < -7.5 ~ -33,
        TRUE ~ -8
      )) %>%
      dplyr::group_by(bunch, variable, value) %>%
      dplyr::summarise(n = n())

    (splitSpecCurve_rsf <- rsfResultsPlotData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = value, colour = d_medEst),
        #            position = position_jitter(width = 0, height = 0.2), alpha = 0.05) +
        geom_point(aes(x = Estimate, y = value, colour = d_medEst),
                   position = position_jitter(width = 0, height = 0.2), alpha = 0.25,
                   pch = 3, size = 0.75) +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1.5, colour = "#FFFFFF") +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1, colour = "#403F41") +
        geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                   linetype = 2) +
        facet_grid(variable~., scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.y.left = element_text(angle = 0, margin = margin(-8.5,12,0,0)),
          axis.text.y.left = element_text(margin = margin(0,-165,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
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
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species),
                      d_medEst = Estimate - median(rsfResults$Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_errorbarh(aes(xmin = Lower, xmax = Upper, y = index, colour = d_medEst),
        #               alpha = 0.005, linewidth = 0.2) +
        # coord_cartesian(xlim = c(-35, 20)) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst), alpha = 0.25,
                   pch = 3, size = 0.75)+
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


    (rsfSpecComplete <- overallSpecCurve_rsf / splitSpecCurve_rsf +
        plot_layout(heights = c(1, 3), guides = "collect"))

    ggsave(filename = here("notebook", "figures", "rsfSpecCurve.png"),
           plot = rsfSpecComplete,
           width = 360, height = 240, units = "mm", dpi = 300)
    # ggsave(filename = here("notebook", "figures", "rsfSpecCurve.pdf"),
    #        plot = rsfSpecComplete,
    #        width = 360, height = 240, units = "mm", device = cairo_pdf)

    return(rsfSpecComplete)

  } else if(method == "wides"){

    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)

    widesResults <- areaResults %>%
      dplyr::filter(analysis == "wides")

    widesResults$tf <- round(widesResults$tf, digits = 2)

    levelOrdering <- unique(c(sort(unique(widesResults$td)),
                              sort(unique(widesResults$tf)),
                              unique(widesResults$area),
                              sort(unique(widesResults$contour)),
                              sort(unique(widesResults$availPointsPer)),
                              sort(unique(widesResults$weighting))))

    widesResultsPlotData <- widesResults %>%
      dplyr::select(Estimate, indi, species, td,  tf, area, contour, availPointsPer, sigColour) %>%
      reshape2::melt(c("Estimate", "indi", "species", "sigColour")) %>%
      dplyr::mutate(
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
      dplyr::group_by(variable, value) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE),
                       nZeroNA = sum(Estimate == 0 | is.na(Estimate)))

    (splitSpecCurve_wides <- widesResultsPlotData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = value),
        #            position = position_jitter(width = 0, height = 0.2),
        #            alpha = 0.2, colour = palette["coreGrey"]) +
        geom_point(aes(x = Estimate, y = value),
                   position = position_jitter(width = 0, height = 0.2), alpha = 0.25,
                   pch = 3, size = 0.75, colour = palette["coreGrey"]) +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1.5, colour = "#FFFFFF") +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1, colour = "#403F41") +
        geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                   linetype = 2) +
        geom_text(data = medData, aes(x = -0.2, y = value, label = nZeroNA),
                  colour = palette["coreGrey"], fontface = 4, hjust = 1) +
        facet_grid(variable~., scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.y.left = element_text(angle = 0, margin = margin(-8.5,12,0,0)),
          axis.text.y.left = element_text(margin = margin(0,-165,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
          axis.ticks.y.left = element_blank(),
          axis.line.x = element_line(),
          strip.clip = "off",
          legend.position = "none",
          panel.border = element_blank(),
          panel.spacing = unit(18, "pt"),
          panel.grid = element_blank())
    )

    overallMed <- data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                             "indexLoc" = round(nrow(widesResults)/2, digits = 0))

    (overallSpecCurve_wides <- widesResults %>%
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = index),
        #            size = 1, alpha = 0.2, colour = palette["coreGrey"])+
        geom_point(aes(x = Estimate, y = index), alpha = 0.25,
                   pch = 3, size = 0.75, colour = palette["coreGrey"])+
        geom_point(data = data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                                     "indexLoc" = round(nrow(widesResults)/2, digits = 0)),
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2.5, colour = "#FFFFFF") +
        geom_point(data = data.frame("medEst" = median(widesResults$Estimate, na.rm = TRUE),
                                     "indexLoc" = round(nrow(widesResults)/2, digits = 0)),
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2, colour = "#403F41") +
        geom_text(data = medData %>% ungroup(), aes(x = -0.2, y = 1, label = sum(nZeroNA)),
                  colour = palette["coreGrey"], fontface = 4, hjust = 1) +
        annotate("text", x = overallMed$medEst +1, y = overallMed$indexLoc, label = "Median",
                 fontface = 4, size = 5, colour = palette["coreGrey"],
                 hjust = 1, vjust = -0.2) +
        annotate("segment", x = overallMed$medEst +1, xend = overallMed$medEst,
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
          legend.position = "none",
          panel.border = element_blank(),
          panel.spacing = unit(18, "pt"),
          panel.grid = element_blank())
    )

    (widesSpecComplete <- overallSpecCurve_wides / splitSpecCurve_wides +
        plot_layout(heights = c(1, 3), guides = "collect"))

    ggsave(filename = here("notebook", "figures", "widesSpecCurve.png"),
           plot = widesSpecComplete,
           width = 360, height = 240, units = "mm", dpi = 300)

    return(widesSpecComplete)

  } else if(method == "ssf"){

    ssfResults <- multiverseHabitat::parse_combined_results(compiledResults)

    ssfResults$tf <- round(ssfResults$tf, digits = 2)

    ssfResults$modelForm <- ifelse(ssfResults$modelForm == "mf.is",
                                   "Integrated",
                                   "Standard")
    ssfResults$stepDist <- ifelse(ssfResults$stepDist == "gamma",
                                  "Gamma",
                                  "Exponential")
    ssfResults$turnDist <- ifelse(ssfResults$turnDist == "unif",
                                  "Uniform",
                                  "Von Mises")

    levelOrdering <- unique(c("5", sort(unique(ssfResults$td)), # 5 is added manually as it appears in two variables
                              sort(unique(ssfResults$availablePerStep)),
                              sort(unique(ssfResults$tf)),
                              unique(ssfResults$modelForm),
                              unique(ssfResults$stepDist),
                              unique(ssfResults$turnDist)
    ))

    ssfResultsPlotData <- ssfResults %>%
      dplyr::select(Estimate, indi, species, td,  tf, modelForm, stepDist, turnDist, availablePerStep,
                    sigColour) %>%
      reshape2::melt(c("Estimate", "indi", "species", "sigColour")) %>%
      dplyr::mutate(
        variable = case_when(
          variable == "td" ~ "Tracking Duration (days)",
          variable == "tf" ~ "Tracking Frequency (points/hour)",
          variable == "modelForm" ~ "Model formula (SSF or iSSF)",
          variable == "stepDist" ~ "Distribution of step lengths",
          variable == "turnDist" ~ "Distribution of turn angles",
          variable == "availablePerStep" ~ "Available points per step"
        ),
        indi = as.factor(indi),
        species = as.factor(species),
        value = factor(value, levels = levelOrdering)) %>%
      dplyr::group_by(variable, value) %>%
      dplyr::mutate(d_medEst = Estimate - median(ssfResults$Estimate, na.rm = TRUE)) %>%
      dplyr::ungroup()

    medData <- ssfResultsPlotData %>%
      dplyr::group_by(variable, value) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE))

    (splitSpecCurve_ssf <- ssfResultsPlotData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = value, colour = d_medEst),
        #            position = position_jitter(width = 0, height = 0.2), alpha = 0.2) +
        geom_point(aes(x = Estimate, y = value, colour = d_medEst),
                   position = position_jitter(width = 0, height = 0.2), alpha = 0.25,
                   pch = 3, size = 0.75) +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1.5, colour = "#FFFFFF") +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1, colour = "#403F41") +
        geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                   linetype = 2) +
        facet_grid(variable~., scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.y.left = element_text(angle = 0, margin = margin(-8.5,12,0,0)),
          axis.text.y.left = element_text(margin = margin(0,-165,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
          axis.ticks.y.left = element_blank(),
          axis.line.x = element_line(),
          strip.clip = "off",
          legend.position = "none",
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
               species = as.factor(species),
               d_medEst = Estimate - median(ssfResults$Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst),
                   alpha = 0.25,
                   pch = 3, size = 0.75)+
        geom_point(data = data.frame("medEst" = median(ssfResults$Estimate, na.rm = TRUE),
                                     "indexLoc" = round(nrow(ssfResults)/2, digits = 0)),
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2.5, colour = "#FFFFFF") +
        geom_point(data = overallMed,
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2, colour = "#403F41") +
        annotate("text", x = overallMed$medEst +5, y = overallMed$indexLoc, label = "Median",
                 fontface = 4, size = 5, colour = palette["coreGrey"],
                 hjust = 1, vjust = -0.2) +
        annotate("segment", x = overallMed$medEst +5, xend = overallMed$medEst,
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

    (ssfSpecComplete <- overallSpecCurve_ssf / splitSpecCurve_ssf +
        plot_layout(heights = c(1, 3), guides = "collect"))

    ggsave(filename = here("notebook", "figures", "ssfSpecCurve.png"),
           plot = ssfSpecComplete,
           width = 360, height = 240, units = "mm", dpi = 300)

    return(ssfSpecComplete)

  } else if(method == "wrsf"){

    wrsfResults <- multiverseHabitat::parse_combined_results(compiledResults[!is.na(compiledResults$Estimate),])

    wrsfResults$tf <- round(wrsfResults$tf, digits = 2)

    levelOrdering <- unique(c(sort(unique(wrsfResults$td)),
                              sort(unique(wrsfResults$tf))))

    wrsfResultsPlotData <- wrsfResults %>%
      dplyr::select(Estimate, Lower, Upper, indi, species,
                    td,  tf) %>%
      reshape2::melt(c("Estimate", "Lower", "Upper", "indi", "species")) %>%
      dplyr::mutate(
        variable = case_when(
          variable == "td" ~ "Tracking Duration (days)",
          variable == "tf" ~ "Tracking Frequency (points/hour)"
        ),
        indi = as.factor(indi),
        species = as.factor(species),
        value = factor(value, levels = levelOrdering)
      ) %>%
      dplyr::group_by(variable, value) %>%
      dplyr::mutate(d_medEst = Estimate - median(wrsfResults$Estimate, na.rm = TRUE)) %>%
      dplyr::ungroup()

    medData <- wrsfResultsPlotData %>%
      dplyr::group_by(variable, value) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE))

    nSummary <- wrsfResultsPlotData %>%
      dplyr::mutate(bunch = case_when(
        Estimate > 7.5 ~ 18.5,
        Estimate < -7.5 ~ -33,
        TRUE ~ -8
      )) %>%
      dplyr::group_by(bunch, variable, value) %>%
      dplyr::summarise(n = n())

    (splitSpecCurve_wrsf <- wrsfResultsPlotData %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_point(aes(x = Estimate, y = value, colour = d_medEst),
        #            position = position_jitter(width = 0, height = 0.2), alpha = 0.05) +
        geom_point(aes(x = Estimate, y = value, colour = d_medEst),
                   position = position_jitter(width = 0, height = 0.2), alpha = 0.25,
                   pch = 3, size = 0.75) +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1.5, colour = "#FFFFFF") +
        geom_point(data = medData, aes(x = medEst, y = value),
                   alpha = 1, size = 1, colour = "#403F41") +
        geom_hline(yintercept = seq(0.5,10.5,1), linewidth = 0.5, alpha = 0.25, colour = "#403F41",
                   linetype = 2) +
        facet_grid(variable~., scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.y.left = element_text(angle = 0, margin = margin(-8.5,12,0,0)),
          axis.text.y.left = element_text(margin = margin(0,-165,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
          axis.ticks.y.left = element_blank(),
          axis.line.x = element_line(),
          strip.clip = "off",
          legend.position = "none",
          panel.border = element_blank(),
          panel.spacing = unit(18, "pt"),
          panel.grid = element_blank())
    )

    splitSpecCurve_wrsf_numbers <- splitSpecCurve_wrsf +
      geom_text(data = nSummary, aes(x = bunch, y = value, label = n), fontface = 3)

    overallMed <- data.frame("medEst" = median(wrsfResults$Estimate, na.rm = TRUE),
                             "indexLoc" = round(nrow(wrsfResults)/2, digits = 0))

    (overallSpecCurve_wrsf <- wrsfResults %>%
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species),
                      d_medEst = Estimate - median(wrsfResults$Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.25, alpha = 0.9, colour = "#403F41",
                   linetype = 1) +
        # geom_errorbarh(aes(xmin = Lower, xmax = Upper, y = index, colour = d_medEst),
        #               alpha = 0.005, linewidth = 0.2) +
        # coord_cartesian(xlim = c(-35, 20)) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst), alpha = 0.25,
                   pch = 3, size = 0.75)+
        geom_point(data = data.frame("medEst" = median(wrsfResults$Estimate, na.rm = TRUE),
                                     "indexLoc" = round(nrow(wrsfResults)/2, digits = 0)),
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2.5, colour = "#FFFFFF") +
        geom_point(data = data.frame("medEst" = median(wrsfResults$Estimate, na.rm = TRUE),
                                     "indexLoc" = round(nrow(wrsfResults)/2, digits = 0)),
                   aes(x = medEst, y = indexLoc),
                   alpha = 1, size = 2, colour = "#403F41") +
        annotate("text", x = overallMed$medEst +6, y = overallMed$indexLoc,
                 label = "Median",
                 fontface = 4, size = 5, colour = palette["coreGrey"],
                 hjust = 1, vjust = -0.2) +
        annotate("segment", x = overallMed$medEst +6, xend = overallMed$medEst,
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

    (wrsfSpecComplete <- overallSpecCurve_wrsf / splitSpecCurve_wrsf +
        plot_layout(heights = c(1, 3), guides = "collect"))

    ggsave(filename = here("notebook", "figures", "wrsfSpecCurve.png"),
           plot = wrsfSpecComplete,
           width = 360, height = 200, units = "mm", dpi = 300)

    return(wrsfSpecComplete)

  }
}
