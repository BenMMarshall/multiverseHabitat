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
  # library(stringr)

  if(!method %in% c("rsf", "ssf", "wides", "wrsf")){
    stop("Type must be rsf, ssf, wides, wrsf")
  }

  # spec curve adaptation ---------------------------------------------------
  # https://hal.inria.fr/hal-03558950/document has great ideas
  palette <- multiverseHabitat::get_palette()

  # targets::tar_load(areaResults)
  # compiledResults <- areaResults
  if(method == "rsf"){

    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)

    rsfResults <- areaResults %>%
      dplyr::filter(analysis == "rsf")

    rsfResults$tf <- round(rsfResults$tf, digits = 2)
    rsfResults$weighting <- round(rsfResults$weighting, digits = 0)

    rsfResults$samplingPattern <- ifelse(rsfResults$samplingPattern == "rd",
                                         "Random",
                                         "Stratified")

    rsfResults$classLandscape <- ifelse(str_detect(rsfResults$classLandscape, "Scram"),
                                        "Scrambled Habitat Layer (i.e., No selection)",
                                        "Correct Habitat Layer (i.e., Positive selection)")

    levelOrdering <- unique(c(sort(unique(rsfResults$td)),
                              sort(unique(rsfResults$tf)),
                              unique(rsfResults$area),
                              sort(unique(rsfResults$contour)),
                              sort(unique(rsfResults$availPointsPer)),
                              sort(unique(rsfResults$samplingPattern)),
                              sort(format(unique(rsfResults$weighting), big.mark = ","))))

    rsfResultsPlotData <- rsfResults %>%
      dplyr::mutate(weighting = format(weighting, big.mark = ",")) %>%
      dplyr::select(Estimate, Lower, Upper, indi, species,
                    td,  tf,
                    area, contour, availPointsPer, samplingPattern, weighting, sigColour,
                    classLandscape) %>%
      reshape2::melt(c("Estimate", "Lower", "Upper", "indi", "species", "sigColour", "classLandscape")) %>%
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
      dplyr::group_by(classLandscape) %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE)) %>%
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::mutate(d_medEst = Estimate - medEst) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = factor(variable, levels = c(
        "Tracking Duration (days)",
        "Tracking Frequency (points/hour)",
        "Available Area Method",
        "Available Area Contour (%)",
        "Available Points Multipiler",
        "Sampling Pattern",
        "Weighting of Available Points")
      ))

    medData <- rsfResultsPlotData %>%
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE))

    nSummary <- rsfResultsPlotData %>%
      dplyr::mutate(bunch = case_when(
        Estimate > 7.5 ~ 18.5,
        Estimate < -7.5 ~ -33,
        TRUE ~ -8
      )) %>%
      dplyr::group_by(bunch, variable, value, classLandscape) %>%
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
        facet_grid(variable~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.x.top = element_blank(),
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

    overallMed <- rsfResults %>%
      mutate(n = n()) %>%
      group_by(classLandscape) %>%
      summarise(medEst = median(Estimate, na.rm = TRUE),
                n = n[1])

    (overallSpecCurve_rsf <- rsfResults %>%
        group_by(classLandscape) %>%
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species),
                      d_medEst = Estimate - median(Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.75, colour = "#403F41",
                   linetype = 2) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst),
                   alpha = 0.25,
                   pch = 3, size = 0.25)+
        geom_segment(data = overallMed,
                     aes(x = medEst, xend = medEst, y = Inf,
                         yend = -Inf),
                     alpha = 1, linewidth = 0.45, linetype = 1,
                     colour = palette["BADGER"]) +
        geom_text(data = overallMed, aes(x = medEst, y = 0,
                                         label = paste0(" Median = ",
                                                        round(medEst, digits = 2))),
                  hjust = 0, vjust = 0, fontface = 4, colour = palette["BADGER"]) +
        scale_colour_gradient2(low = palette["BADGER"],
                               mid = palette["coreGrey"],
                               high = palette["2"]) +
        scale_y_continuous(expand = c(0.05, 0)) +
        facet_grid(.~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
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
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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
           width = 265, height = 240, units = "mm", dpi = 300)
    # ggsave(filename = here("notebook", "figures", "rsfSpecCurve.pdf"),
    #        plot = rsfSpecComplete,
    #        width = 360, height = 240, units = "mm", device = cairo_pdf)

    return(rsfSpecComplete)

  } else if(method == "wides"){

    areaResults <- multiverseHabitat::parse_combined_results(compiledResults)

    widesResults <- areaResults %>%
      dplyr::filter(analysis == "wides")

    widesResults$tf <- round(widesResults$tf, digits = 2)

    widesResults$classLandscape <- ifelse(str_detect(widesResults$classLandscape, "Scram"),
                                          "Scrambled Habitat Layer (i.e., No selection)",
                                          "Correct Habitat Layer (i.e., Positive selection)")

    levelOrdering <- unique(c(sort(unique(widesResults$td)),
                              sort(unique(widesResults$tf)),
                              unique(widesResults$area),
                              sort(unique(widesResults$contour)),
                              sort(unique(widesResults$availPointsPer)),
                              sort(unique(widesResults$weighting))))

    widesResultsPlotData <- widesResults %>%
      dplyr::select(Estimate, indi, species, td,  tf, area, contour, availPointsPer, sigColour,
                    classLandscape) %>%
      reshape2::melt(c("Estimate", "indi", "species", "sigColour", "classLandscape")) %>%
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
        value = factor(value, levels = levelOrdering)) %>%
      dplyr::mutate(variable = factor(variable, levels = c(
          "Tracking Duration (days)",
          "Tracking Frequency (points/hour)",
          "Available Area Method",
          "Available Area Contour (%)",
          "Available Points Multipiler"
      )))

    medData <- widesResultsPlotData %>%
      dplyr::group_by(variable, value, classLandscape) %>%
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
        geom_text(data = medData, aes(x = 0.05, y = value, label = nZeroNA),
                  colour = palette["coreGrey"], fontface = 4, hjust = 0) +
        facet_grid(variable~classLandscape, scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.x.top = element_blank(),
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

    overallMed <- widesResults %>%
      mutate(n = n()) %>%
      group_by(classLandscape) %>%
      summarise(medEst = median(Estimate, na.rm = TRUE),
                n = n[1])

    (overallSpecCurve_wides <- widesResults %>%
        group_by(classLandscape) %>%
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species),
                      d_medEst = Estimate - median(Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.75, colour = "#403F41",
                   linetype = 2) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst),
                   alpha = 0.25,
                   pch = 3, size = 0.25)+
        geom_segment(data = overallMed,
                     aes(x = medEst, xend = medEst, y = Inf,
                         yend = -Inf),
                     alpha = 1, linewidth = 0.45, linetype = 1,
                     colour = palette["BADGER"]) +
        geom_text(data = overallMed, aes(x = medEst, y = 0,
                                         label = paste0(" Median = ",
                                                        round(medEst, digits = 2))),
                  hjust = 0, vjust = 0, fontface = 4, colour = palette["BADGER"]) +
        scale_colour_gradient2(low = palette["BADGER"],
                               mid = palette["coreGrey"],
                               high = palette["2"]) +
        scale_y_continuous(expand = c(0.05, 0)) +
        facet_grid(.~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
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
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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
           width = 240, height = 220, units = "mm", dpi = 300)

    return(widesSpecComplete)

  } else if(method == "ssf"){

    # targets::tar_load(ssfResults)
    # compiledResults <- ssfResults

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

    ssfResults$classLandscape <- ifelse(str_detect(ssfResults$classLandscape, "Scram"),
                                        "Scrambled Habitat Layer (i.e., No selection)",
                                        "Correct Habitat Layer (i.e., Positive selection)")

    levelOrdering <- unique(c("5", sort(unique(ssfResults$td)), # 5 is added manually as it appears in two variables
                              sort(unique(ssfResults$availablePerStep)),
                              sort(unique(ssfResults$tf)),
                              unique(ssfResults$modelForm),
                              unique(ssfResults$stepDist),
                              unique(ssfResults$turnDist)
    ))

    ssfResultsPlotData <- ssfResults %>%
      dplyr::select(Estimate, indi, species, td,  tf, modelForm, stepDist, turnDist, availablePerStep,
                    sigColour, classLandscape) %>%
      reshape2::melt(c("Estimate", "indi", "species", "sigColour", "classLandscape")) %>%
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
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::mutate(d_medEst = Estimate - median(Estimate, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = factor(variable, levels = c(
        "Tracking Duration (days)",
        "Tracking Frequency (points/hour)",
        "Available points per step",
        "Model formula (SSF or iSSF)",
        "Distribution of step lengths",
        "Distribution of turn angles"
      )))

    medData <- ssfResultsPlotData %>%
      dplyr::group_by(variable, value, classLandscape) %>%
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
        facet_grid(variable~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.x.top = element_blank(),
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

    overallMed <- ssfResults %>%
      mutate(n = n()) %>%
      group_by(classLandscape) %>%
      summarise(medEst = median(Estimate, na.rm = TRUE),
                n = n[1])

    (overallSpecCurve_ssf <- ssfResults %>%
        arrange(Estimate) %>%
        mutate(index = row_number(),
               indi = as.factor(indi),
               species = as.factor(species),
               d_medEst = Estimate - median(ssfResults$Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.75, colour = "#403F41",
                   linetype = 2) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst),
                   alpha = 0.25,
                   pch = 3, size = 0.25)+
        geom_segment(data = overallMed,
                     aes(x = medEst, xend = medEst, y = Inf,
                         yend = -Inf),
                     alpha = 1, linewidth = 0.45, linetype = 1,
                     colour = palette["BADGER"]) +
        geom_text(data = overallMed, aes(x = medEst, y = 0,
                                         label = paste0(" Median = ",
                                                        round(medEst, digits = 2))),
                  hjust = 0, vjust = 0, fontface = 4, colour = palette["BADGER"]) +
        scale_colour_gradient2(low = palette["BADGER"],
                               mid = palette["coreGrey"],
                               high = palette["2"]) +
        scale_y_continuous(expand = c(0.05, 0)) +
        facet_grid(.~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
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
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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
           width = 240, height = 220, units = "mm", dpi = 300)

    return(ssfSpecComplete)

  } else if(method == "wrsf"){

    # targets::tar_load(wrsfResults)
    # compiledResults <- wrsfResults

    wrsfResults <- multiverseHabitat::parse_combined_results(compiledResults[!is.na(compiledResults$Estimate),])

    wrsfResults$tf <- round(wrsfResults$tf, digits = 2)

    wrsfResults$classLandscape <- ifelse(str_detect(wrsfResults$classLandscape, "Scram"),
                                         "Scrambled Habitat Layer (i.e., No selection)",
                                         "Correct Habitat Layer (i.e., Positive selection)")

    levelOrdering <- unique(c(sort(unique(wrsfResults$td)),
                              sort(unique(wrsfResults$tf))))

    wrsfResultsPlotData <- wrsfResults %>%
      dplyr::select(Estimate, Lower, Upper, indi, species,
                    td,  tf, classLandscape) %>%
      reshape2::melt(c("Estimate", "Lower", "Upper", "indi", "species", "classLandscape")) %>%
      dplyr::mutate(
        variable = case_when(
          variable == "td" ~ "Tracking Duration (days)",
          variable == "tf" ~ "Tracking Frequency (points/hour)"
        ),
        indi = as.factor(indi),
        species = as.factor(species),
        value = factor(value, levels = levelOrdering)
      ) %>%
      dplyr::group_by(classLandscape) %>%
      dplyr::mutate(medEst = median(Estimate, na.rm = TRUE)) %>%
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::mutate(d_medEst = Estimate - medEst) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(variable = factor(variable, levels = c(
        "Tracking Duration (days)",
        "Tracking Frequency (points/hour)")))

    medData <- wrsfResultsPlotData %>%
      dplyr::group_by(variable, value, classLandscape) %>%
      dplyr::summarise(medEst = median(Estimate, na.rm = TRUE))

    nSummary <- wrsfResultsPlotData %>%
      dplyr::mutate(bunch = case_when(
        Estimate > 7.5 ~ 18.5,
        Estimate < -7.5 ~ -33,
        TRUE ~ -8
      )) %>%
      dplyr::group_by(bunch, variable, value, classLandscape) %>%
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
        facet_grid(variable~classLandscape, scales = "free_y", space = "free", switch = "y") +
        labs(y = "", x = "Estimate") +
        # scale_colour_manual(values = unname(palette[c("2", "coreGrey", "BADGER")]), na.value = "#000000") +
        scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
        theme_bw() +
        theme(
          line = element_line(colour = palette["coreGrey"]),
          text = element_text(colour = palette["coreGrey"]),
          strip.background = element_blank(),
          strip.text = element_text(face = 4, hjust = 1, vjust = 1),
          strip.text.x.top = element_blank(),
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

    overallMed <- wrsfResults %>%
      mutate(n = n()) %>%
      group_by(classLandscape) %>%
      summarise(medEst = median(Estimate, na.rm = TRUE),
                n = n[1])

    (overallSpecCurve_wrsf <- wrsfResults %>%
        dplyr::group_by(classLandscape) %>%
        dplyr::arrange(Estimate) %>%
        dplyr::mutate(index = row_number(),
                      indi = as.factor(indi),
                      species = as.factor(species),
                      d_medEst = Estimate - median(Estimate, na.rm = TRUE)) %>%
        ggplot() +
        geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.75, colour = "#403F41",
                   linetype = 2) +
        # geom_point(aes(x = Estimate, y = index, colour = d_medEst),
        #            size = 1, alpha = 0.2)+
        geom_point(aes(x = Estimate, y = index, colour = d_medEst),
                   alpha = 0.25,
                   pch = 3, size = 0.25)+
        geom_segment(data = overallMed,
                     aes(x = medEst, xend = medEst, y = Inf,
                         yend = -Inf),
                     alpha = 1, linewidth = 0.45, linetype = 1,
                     colour = palette["BADGER"]) +
        geom_text(data = overallMed, aes(x = medEst, y = 0,
                                         label = paste0(" Median = ",
                                                        round(medEst, digits = 2))),
                  hjust = 0, vjust = 0, fontface = 4, colour = palette["BADGER"]) +
        scale_colour_gradient2(low = palette["BADGER"],
                               mid = palette["coreGrey"],
                               high = palette["2"]) +
        scale_y_continuous(expand = c(0.05, 0)) +
        facet_grid(.~classLandscape,
                   scales = "free_y", space = "free", switch = "y") +
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
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
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
           width = 240, height = 100, units = "mm", dpi = 300)

    return(wrsfSpecComplete)

  }
}
