#' Generate effect plots
#'
#' @name generate_effect_plots
#' @description A
#' @param brmsResults output tar_targets resulting in ssfBrms or areaBrms
#' @param method c("rsf", "ssf", "wides")
#' @return a
#'
#' @export
generate_effect_plots <- function(brmsResults, method, ...){

  if(!method %in% c("rsf", "ssf", "wides", "wrsf")){
    stop("Type must be rsf, ssf, wides, wrsf")
  }

  # library(dplyr)
  # library(ggplot2)
  # library(tidybayes)
  # library(stringr)
  # library(reshape2)
  # library(here)

  palette <- multiverseHabitat::get_palette()

  # Model results -----------------------------------------------------------

  # areaResults <- multiverseHabitat::parse_combined_results(areaResults)
  # areaResults$tf <- round(areaResults$tf, digits = 2)
  #
  # modelDataRSF <- areaResults %>%
  #   filter(analysis == "rsf") %>%
  #   mutate(medEst = median(Estimate, na.rm = TRUE),
  #          absDeltaEst = abs(Estimate - medEst)) %>%
  #   mutate(tfScaled = (tf-mean(tf))/sd(tf),
  #          tdScaled = (td-mean(td))/sd(td),
  #          availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
  #          weightingScaled = (weighting-mean(weighting))/sd(weighting),
  #          contourScaled = (contour-mean(contour))/sd(contour))

  # targets::tar_load("areaBrms_wides")
  # targets::tar_load("areaBrms_rsf")
  # targets::tar_load("ssfBrms")
  # targets::tar_load("wrsfBrms")
  # names(areaBrms_rsf)

  # brmsResults <- areaBrms_rsf
  # brmsResults <- areaBrms_wides
  # brmsResults <- ssfBrms
  # brmsResults <- wrsfBrms

  numberModels <- unlist(lapply(brmsResults, function(x){
    class(x) == "brmsfit"
  }))

  n <- 0
  effectPlotList <- vector("list", sum(numberModels))
  for(i in 1:sum(numberModels)){

    # i <- 3
    modName <- names(brmsResults)[i]
    modCurrent <- brmsResults[[i]]

    if(class(modCurrent) == "brmsfit"){

      gradLimits <- modCurrent %>%
        spread_draws(`b_.*`, regex = TRUE) %>%
        dplyr::select(-.chain, -.iteration, -.draw) %>%
        melt() %>%
        filter(!variable %in% c("b_Intercept"),
               !str_detect(variable, ":")) %>%
        summarise(
          minX = min(value),
          maxX = max(value)
        )

      if(method == "rsf"){

        # RSF EFFECTS -------------------------------------------------------------

        rsfEffectsData <- modCurrent %>%
          spread_draws(`b_.*`, regex = TRUE) %>%
          dplyr::select(-.chain, -.iteration, -.draw) %>%
          melt() %>%
          filter(!variable %in% c("b_Intercept"),
                 !str_detect(variable, ":")) %>%
          mutate(
            variable = case_when(
              variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
              variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
              variable == "b_areaAKDE" ~ "\u03B2 Available Area: AKDE",
              variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
              variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
              variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
              variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
              variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
              variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified",
              variable == "b_weightingScaled" ~ "\u03B2 Weighting of Available Points"
            )
          )

        if(modName == "modOUT_rsf_raw"){
          labelLocation <- c(-0.6, 1.1)
          labelLocationText <- c(-0.55, 1.05)
          labelText <- c("Lower \npreference estimates",
                         "Higher \npreference estimates")
          gradAdj <- c(0.01, -0.01)
        } else {
          labelLocation <- c(-1.1, 1.2)
          labelLocationText <- c(-1.05, 1.15)
          labelText <- c("Closer to median\npreference estimate",
                         "Farther from median\npreference estimate")
          gradAdj <- c(1, -0.1)
        }

        rsfEffectsPlot <- rsfEffectsData %>%
          ggplot(aes(x = value, y = variable)) +
          geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                     linetype = 1) +
          stat_slab(aes(fill = after_stat(x)), fill_type = "gradient",
                    alpha = 0.85) +
          stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                             point_interval = median_hdci, .width = c(.66, .95),
                             stroke = 1.25, colour = palette[c("coreGrey")], size = 4) +
          stat_summary(aes(colour = after_stat(x) > 0),
                       position = position_dodge(width = 0.2, preserve = "single"),
                       fun = median, linewidth = 0.25, size = 0.3) +
          # scale_fill_manual(values = unname(palette[c("BADGER", "2")])) +
          # scale_fill_gradient(low = palette["BADGER"], high = palette["2"])+
          scale_fill_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"],
                               limits = c(gradLimits[1,1]+gradAdj[1],
                                          gradLimits[1,2]+gradAdj[2]),
                               # limits = c(-0.5, 0.35),
                               midpoint = 0,
                               oob = scales::oob_squish_any) +
          scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +

          annotate("segment", x = -0.1, xend = labelLocation[1], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["BADGER"]) +
          annotate("text", x = labelLocationText[1], y = 0.85,
                   label = labelText[1],
                   colour = palette["BADGER"], hjust = 0, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          annotate("segment", x = 0.1, xend = labelLocation[2], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["2"]) +
          annotate("text", x = labelLocationText[2], y = 0.85,
                   label = labelText[2],
                   colour = palette["2"], hjust = 1, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +

          coord_cartesian(clip = "off", ylim = c(1, NA)) +
          labs(x = "Beta", y = "") +
          theme_bw() +
          theme(
            line = element_line(colour = palette["coreGrey"]),
            text = element_text(colour = palette["coreGrey"]),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, hjust = 1, vjust = 1),
            # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
            # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
            axis.title.x = element_text(margin = margin(5,0,0,0)),
            axis.ticks.y.left = element_blank(),
            axis.line.x = element_line(),
            strip.clip = "off",
            panel.border = element_blank(),
            panel.spacing = unit(18, "pt"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "none")


        ggsave(rsfEffectsPlot,
               filename = here("notebook", "figures", paste0(modName, "_effectsPlot.png")),
               dpi = 300, width = 210, height = 140,
               units = "mm")
        n <- n+1
        effectPlotList[[n]] <- rsfEffectsPlot

      } else if(method == "wides"){

        # WIDES EFFECTS -----------------------------------------------------------

        widesEffectData <- modCurrent %>%
          spread_draws(`b_.*`, regex = TRUE) %>%
          dplyr::select(-.chain, -.iteration, -.draw) %>%
          melt() %>%
          filter(!variable %in% c("b_Intercept"),
                 !str_detect(variable, ":")) %>%
          mutate(
            variable = case_when(
              variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
              variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
              variable == "b_areaAKDE" ~ "\u03B2 Available Area: AKDE",
              variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
              variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
              variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
              variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
              variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
              variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified"
            )
          )

        if(modName == "modOUT_wides_raw"){
          labelLocation <- c(-0.055, 0.05)
          labelLocationText <- c(-0.052, 0.047)
          labelText <- c("Lower \npreference estimates",
                         "Higher \npreference estimates")
          gradAdj <- c(0.01, -0.01)
        } else {
          labelLocation <- c(-0.022, 0.023)
          labelLocationText <- c(-0.021, 0.022)
          labelText <- c("Closer to median\npreference estimate",
                         "Farther from median\npreference estimate")
          gradAdj <- c(0.01, 0)
        }

        widesEffectPlot <-
          widesEffectData %>%
          ggplot(aes(x = value, y = variable)) +
          geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                     linetype = 1) +
          stat_slab(aes(fill = after_stat(x)), fill_type = "gradient",
                    alpha = 0.85) +
          stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                             point_interval = median_hdci, .width = c(.66, .95),
                             stroke = 1.25, colour = palette[c("coreGrey")]) +
          stat_summary(aes(colour = after_stat(x) > 0),
                       position = position_dodge(width = 0.2, preserve = "single"),
                       fun = median, size = 0.25) +
          # scale_fill_manual(values = unname(palette[c("BADGER", "2")])) +
          scale_fill_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"],
                               limits = c(gradLimits[1,1]+gradAdj[1],
                                          gradLimits[1,2]+gradAdj[2]),
                               # limits = c(-0.5, 0.35),
                               midpoint = 0,
                               oob = scales::oob_squish_any) +
          scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +

          annotate("segment", x = -0.01, xend = labelLocation[1], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["BADGER"]) +
          annotate("text", x = labelLocationText[1], y = 0.85,
                   label = labelText[1],
                   colour = palette["BADGER"], hjust = 0, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          annotate("segment", x = 0.01, xend = labelLocation[2], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["2"]) +
          annotate("text", x = labelLocationText[2], y = 0.85,
                   label = labelText[2],
                   colour = palette["2"], hjust = 1, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +

          coord_cartesian(clip = "off", ylim = c(1, NA)) +
          labs(x = "Beta", y = "") +
          theme_bw() +
          theme(
            line = element_line(colour = palette["coreGrey"]),
            text = element_text(colour = palette["coreGrey"]),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, hjust = 1, vjust = 1),
            # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
            # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
            axis.title.x = element_text(margin = margin(5,0,0,0)),
            axis.ticks.y.left = element_blank(),
            axis.line.x = element_line(),
            strip.clip = "off",
            panel.border = element_blank(),
            panel.spacing = unit(18, "pt"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "none")

        ggsave(widesEffectPlot,
               filename = here("notebook", "figures", paste0(modName, "_effectsPlot.png")),
               dpi = 300, width = 210, height = 130,
               units = "mm")
        n <- n+1
        effectPlotList[[n]] <- widesEffectPlot

        # SSF EFFECTS -------------------------------------------------------------

      } else if(method == "ssf"){

        ssfEffectData <- modCurrent %>%
          spread_draws(`b_.*`, regex = TRUE) %>%
          dplyr::select(-.chain, -.iteration, -.draw) %>%
          melt() %>%
          filter(!variable %in% c("b_Intercept")) %>%
          mutate(
            variable = case_when(
              variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
              variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
              variable == "b_modelFormmf.ss" ~ "\u03B2 Model Formula: Not Integrated",
              variable == "b_stepDistgamma" ~ "\u03B2 Step Distribution: Gamma",
              variable == "b_turnDistvonmises" ~ "\u03B2 Turn Distribution: Von Mises",
              variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
              variable == "b_availablePerStepScaled" ~ "\u03B2 Available Points Per Step"
            )
          )

        if(modName == "modOUT_ssf_raw"){
          labelLocation <- c(-1.75, 0.9)
          labelLocationText <- c(-1.7, 0.85)
          labelText <- c("Lower \npreference estimates",
                         "Higher \npreference estimates")
          gradAdj <- c(0, 0)
        } else {
          labelLocation <- c(-1.75, 0.9)
          labelLocationText <- c(-1.7, 0.85)
          labelText <- c("Closer to median\npreference estimate",
                         "Farther from median\npreference estimate")
          gradAdj <- c(0, 0)
        }

        ssfEffectPlot <-
          ssfEffectData %>%
          ggplot(aes(x = value, y = variable)) +
          geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                     linetype = 1) +
          stat_slab(aes(fill = after_stat(x)), fill_type = "gradient",
                    alpha = 0.85) +
          stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                             point_interval = median_hdci, .width = c(.66, .95),
                             stroke = 1.25, colour = palette[c("coreGrey")]) +
          stat_summary(aes(colour = after_stat(x) > 0),
                       position = position_dodge(width = 0.2, preserve = "single"),
                       fun = median, size = 0.25) +
          scale_fill_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"],
                               limits = c(gradLimits[1,1]+gradAdj[1],
                                          gradLimits[1,2]+gradAdj[2]),
                               # limits = c(-0.5, 0.35),
                               midpoint = 0,
                               oob = scales::oob_squish_any) +
          scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +
          annotate("segment", x = -0.1, xend = labelLocation[1], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["BADGER"]) +
          annotate("text", x = labelLocationText[1], y = 0.85,
                   label = labelText[1],
                   colour = palette["BADGER"], hjust = 0, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          annotate("segment", x = 0.1, xend = labelLocation[2], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["2"]) +
          annotate("text", x = labelLocationText[2], y = 0.85,
                   label = labelText[2],
                   colour = palette["2"], hjust = 1, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          coord_cartesian(clip = "off", ylim = c(1, NA)) +
          labs(x = "Beta", y = "") +
          theme_bw() +
          theme(
            line = element_line(colour = palette["coreGrey"]),
            text = element_text(colour = palette["coreGrey"]),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, hjust = 1, vjust = 1),
            # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
            # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
            axis.title.x = element_text(margin = margin(5,0,0,0)),
            axis.ticks.y.left = element_blank(),
            axis.line.x = element_line(),
            strip.clip = "off",
            panel.border = element_blank(),
            panel.spacing = unit(18, "pt"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "none")

        ggsave(ssfEffectPlot,
               filename = here("notebook", "figures", paste0(modName, "_effectsPlot.png")),
               dpi = 300, width = 210, height = 120,
               units = "mm")

        n <- n+1
        effectPlotList[[n]] <- ssfEffectPlot

        # WRSF EFFECTS -------------------------------------------------------------

      } else if(method == "wrsf"){

        wrsfEffectData <- modCurrent %>%
          spread_draws(`b_.*`, regex = TRUE) %>%
          dplyr::select(-.chain, -.iteration, -.draw) %>%
          melt() %>%
          filter(!variable %in% c("b_Intercept")) %>%
          mutate(
            variable = case_when(
              variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
              variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency"
            )
          )

        if(modName == "modOUT_wrsf_raw"){
          labelLocation <- c(-0.75, 0.32)
          labelLocationText <- c(-0.72, 0.3)
          labelText <- c("Lower \npreference estimates",
                         "Higher \npreference estimates")
          gradAdj <- c(0, 0)
        } else {
          labelLocation <- c(-0.75, 0.32)
          labelLocationText <- c(-0.72, 0.3)
          labelText <- c("Closer to median\npreference estimate",
                         "Farther from median\npreference estimate")
          gradAdj <- c(0, 0)
        }

        wrsfEffectPlot <-
          wrsfEffectData %>%
          ggplot(aes(x = value, y = variable)) +
          geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
                     linetype = 1) +
          stat_slab(aes(fill = after_stat(x)), fill_type = "gradient",
                    alpha = 0.85) +
          stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                             point_interval = median_hdci, .width = c(.66, .95),
                             stroke = 1.25, colour = palette[c("coreGrey")]) +
          stat_summary(aes(colour = after_stat(x) > 0),
                       position = position_dodge(width = 0.2, preserve = "single"),
                       fun = median, size = 0.25) +
          scale_fill_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"],
                               limits = c(gradLimits[1,1]+gradAdj[1],
                                          gradLimits[1,2]+gradAdj[2]),
                               # limits = c(-0.5, 0.35),
                               midpoint = 0,
                               oob = scales::oob_squish_any) +
          scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +
          annotate("segment", x = -0.05, xend = labelLocation[1], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["BADGER"]) +
          annotate("text", x = labelLocationText[1], y = 0.7,
                   label = labelText[1],
                   colour = palette["BADGER"], hjust = 0, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          annotate("segment", x = 0.05, xend = labelLocation[2], y = 0.65, yend = 0.65,
                   linewidth = 1.25,
                   arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
                   colour = palette["2"]) +
          annotate("text", x = labelLocationText[2], y = 0.7,
                   label = labelText[2],
                   colour = palette["2"], hjust = 1, vjust = 0, lineheight = 0.95,
                   size = 3, fontface = 4) +
          coord_cartesian(clip = "off", ylim = c(1, NA)) +
          labs(x = "Beta", y = "") +
          theme_bw() +
          theme(
            line = element_line(colour = palette["coreGrey"]),
            text = element_text(colour = palette["coreGrey"]),
            strip.background = element_blank(),
            strip.text = element_text(face = 4, hjust = 1, vjust = 1),
            # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
            # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
            axis.title.x = element_text(margin = margin(5,0,0,0)),
            axis.ticks.y.left = element_blank(),
            axis.line.x = element_line(),
            strip.clip = "off",
            panel.border = element_blank(),
            panel.spacing = unit(18, "pt"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "none")

        ggsave(wrsfEffectPlot,
               filename = here("notebook", "figures", paste0(modName, "_effectsPlot.png")),
               dpi = 300, width = 210, height = 100,
               units = "mm")

        n <- n+1
        effectPlotList[[n]] <- wrsfEffectPlot

      } # ifelse method

    } # if class brmsfit
  } # end of for loop

  return(effectPlotList)

}
