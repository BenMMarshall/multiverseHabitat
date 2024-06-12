library(here)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

palette <- multiverseHabitat::get_palette()
compiledResults <- read_csv(here("data", "ssfResults.csv.gz"))

ssfResults <- compiledResults

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

overallMed <- ssfResults %>%
  mutate(n = n()) %>%
  group_by(classLandscape) %>%
  summarise(medEst = median(Estimate, na.rm = TRUE),
            n = n[1])

(overallSpecCurve_ssf <- ssfResults %>%
    filter(classLandscape == "Correct Habitat Layer (i.e., Positive selection)") %>%
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
    # facet_grid(.~classLandscape,
    #            scales = "free_y", space = "free", switch = "y") +
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
