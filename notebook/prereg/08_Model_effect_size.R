
# library(stringr)
library(dplyr)
library(brms)
# library(rstan)
library(bayesplot)
library(tidybayes)

# library(ggplot2)
# library(ggridges)
# library(scico)
# library(ggtext)

palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
names(palette) <- c("KINGCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

# targets::tar_load(areaResults)
# targets::tar_load(ssfResults)

if(method == "rsf"){


  # parse combined results converts tf to points/hour to help interpretation
  areaResults <- multiverseHabitat::parse_combined_results(areaResults)
  areaResults$tf <- round(areaResults$tf, digits = 2)

  modelDataRSF <- areaResults %>%
    filter(analysis == "rsf") %>%
    mutate(medEst = median(Estimate, na.rm = TRUE),
           absDeltaEst = abs(Estimate - medEst)) %>%
    mutate(tfScaled = (tf-mean(tf))/sd(tf),
           tdScaled = (td-mean(td))/sd(td),
           availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
           weightingScaled = (weighting-mean(weighting))/sd(weighting),
           contourScaled = (contour-mean(contour))/sd(contour))

  hist(modelDataRSF$absDeltaEst, breaks = 20000)
  # model formula
  # rsf
  formRSF <- bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                  area + contourScaled + availPointsPerScaled + samplingPattern +
                  weightingScaled +
                  (1|species/indi))


  get_prior(formRSF, data = modelDataRSF)
  # priors
  brmpriorRSF <- c(
    set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
    set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
    set_prior("cauchy(0.1, 3)", coef = "areaMCP"),
    set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
    set_prior("cauchy(0.1, 3)", coef = "availPointsPerScaled"),
    set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
    set_prior("cauchy(0.1, 3)", coef = "tfScaled"),
    set_prior("cauchy(0.1, 3)", coef = "weightingScaled")
  )
  modOUT_dEstRSF <- brm(formula = formRSF,
                        data = modelDataRSF,
                        family = gamma,
                        prior = brmpriorRSF,
                        warmup = 1000, iter = 2000, chains = 4,
                        # warmup = 2000, iter = 5000, chains = 4,
                        cores = 4,
                        thin = 2,
                        # control = list(adapt_delta = 0.90,
                        #                max_treedepth = 15),
                        seed = 1,
                        save_pars = save_pars(all = TRUE),
                        save_model = here("notebook", "modelOutput", "absDeltaEstModel_RSF.txt"),
                        file = here("notebook", "modelOutput", "absDeltaEstModel_RSF"))

  return(list(method = "rsf",
              brmOUT = modOUT_dEstRSF))

} else if(method == "wides"){
  # parse combined results converts tf to points/hour to help interpretation
  areaResults <- multiverseHabitat::parse_combined_results(areaResults)
  areaResults$tf <- round(areaResults$tf, digits = 2)

  modelDataWides <- areaResults %>%
    filter(analysis == "wides") %>%
    mutate(medEst = median(Estimate, na.rm = TRUE),
           absDeltaEst = abs(Estimate - medEst)) %>%
    mutate(tfScaled = (tf-mean(tf))/sd(tf),
           tdScaled = (td-mean(td))/sd(td),
           availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
           contourScaled = (contour-mean(contour))/sd(contour))


  hist(modelDataWides$absDeltaEst, breaks = 20000)
  # 2 % of the wides results are NA
  sum(is.na(modelDataWides$Estimate))/
    nrow(modelDataWides) *100
  # wides
  formWides <- bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                    area + contourScaled + availPointsPerScaled + samplingPattern +
                    (1|species/indi))
  get_prior(formWides, data = modelDataWides)
  brmpriorWides <- c(
    set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
    set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
    set_prior("cauchy(0.1, 3)", coef = "areaMCP"),
    set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
    set_prior("cauchy(0.1, 3)", coef = "availPointsPerScaled"),
    set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
    set_prior("cauchy(0.1, 3)", coef = "tfScaled")
  )

  modOUT_dEstWides <- brm(formula = formWides,
                          data = modelDataWides,
                          family = gamma,
                          prior = brmpriorWides,
                          warmup = 1000, iter = 2000, chains = 4,
                          # warmup = 2000, iter = 5000, chains = 4,
                          cores = 4,
                          thin = 2,
                          # control = list(adapt_delta = 0.90,
                          #                max_treedepth = 15),
                          seed = 1,
                          save_pars = save_pars(all = TRUE),
                          save_model = here("notebook", "modelOutput", "absDeltaEstModel_Wides.txt"),
                          file = here("notebook", "modelOutput", "absDeltaEstModel_Wides"))

  return(list(method = "wides",
              brmOUT = modOUT_dEstWides))

} else if(method == "ssf"){

  ssfResults <- multiverseHabitat::parse_combined_results(ssfResults)
  ssfResults$tf <- round(ssfResults$tf, digits = 2)

  modelDataSSF <- ssfResults %>%
    mutate(medEst = median(Estimate, na.rm = TRUE),
           absDeltaEst = abs(Estimate - medEst)) %>%
    mutate(tfScaled = (tf-mean(tf))/sd(tf),
           tdScaled = (td-mean(td))/sd(td),
           availablePerStepScaled  = (availablePerStep-mean(availablePerStep))/sd(availablePerStep))

  hist(modelDataSSF$absDeltaEst, breaks = 20000)
  # ssf
  formSSF <- bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                  modelForm + stepDist + turnDist + availablePerStepScaled +
                  (1|species/indi))
  # AKA (1|species) + (1|species:indi) for a nested group effect intercept
  get_prior(formSSF, data = ssfModelData)
  brmpriorSSF <- c(
    set_prior("cauchy(0.1, 3)", coef = "availablePerStepScaled"),
    set_prior("cauchy(0.1, 3)", coef = "modelFormmf.ss"),
    set_prior("cauchy(0.1, 3)", coef = "stepDistgamma"),
    set_prior("cauchy(0.1, 3)", coef = "turnDistvonmises"),
    set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
    set_prior("cauchy(0.1, 3)", coef = "tfScaled")
  )
  # ggplot(data.frame("x" = rcauchy(2000, location = 0.1, scale = 1))) +
  #   geom_density(aes(x = x)) +
  #   coord_cartesian(xlim = c(-100, 100))

  modOUT_dEstSSF <- brm(formula = formSSF,
                        data = modelDataSSF,
                        family = gamma,
                        prior = brmpriorSSF,
                        warmup = 1000, iter = 2000, chains = 4,
                        # warmup = 2000, iter = 5000, chains = 4,
                        cores = 4,
                        thin = 2,
                        # control = list(adapt_delta = 0.90,
                        #                max_treedepth = 15),
                        seed = 1,
                        save_pars = save_pars(all = TRUE),
                        save_model = here("notebook", "modelOutput", "absDeltaEstModel_SSF.txt"),
                        file = here("notebook", "modelOutput", "absDeltaEstModel_SSF.txt"))

  return(list(method = "ssf",
              brmOUT = modOUT_dEstSSF))
}


# Review convergence ------------------------------------------------------

for(mod in ls(pattern = "modOUT")){
  # mod <- ls(pattern = "modOUT")[1]
  currMod <- get(mod)
  vars <- get_variables(currMod)
  varsToPlot <- vars[stringr::str_detect(vars, "b_")]

  traceplot <- mcmc_trace(currMod, pars = varsToPlot)

  ggsave(traceplot,
         filename = here("notebook", "modelOutput", paste0(mod, "_traceplot.png")),
         dpi = 300, width = 210, height = 140,
         units = "mm")

  acfplot <- mcmc_acf(currMod, pars = varsToPlot)

  ggsave(acfplot,
         filename = here("notebook", "modelOutput", paste0(mod, "_acfplot.png")),
         dpi = 300, width = 210, height = 140,
         units = "mm")
}


# review the divergent instances and check where they are in the parameter space
# params_cp <- as.data.frame(extract(brmfit$fit, permuted=FALSE))
# names(params_cp) <- gsub("chain:1.", "", names(params_cp), fixed = TRUE)
# names(params_cp) <- gsub("[", ".", names(params_cp), fixed = TRUE)
# names(params_cp) <- gsub("]", "", names(params_cp), fixed = TRUE)
#
# divergent <- rstan::get_sampler_params(brmfit$fit, inc_warmup=FALSE)[[1]][,'divergent__']
# sum(divergent)
#
# params_cp$divergent <- divergent
#
# div_params_cp <- params_cp[params_cp$divergent == 1,]
# nondiv_params_cp <- params_cp[params_cp$divergent == 0,]
#
# par(mar = c(4, 4, 0.5, 0.5))
# plot(nondiv_params_cp$b_Intercept, log(nondiv_params_cp$b_log10mass),
#      col="grey55", pch=16, cex=0.8, xlab="theta.1", ylab="log(tau)",
#      xlim=c(-15, 5), ylim=c(-0.1,1))
# points(div_params_cp$b_Intercept, log(div_params_cp$b_log10mass),
#        col="red", pch=16, cex=0.8)

# Model results -----------------------------------------------------------

tdScaledEffect <- modOUT_dEstRSF %>%
  spread_draws(b_tdScaled) %>%
  median_hdci(.width = c(0.95))

modOUT_dEstRSF %>%
  spread_draws(r_species[condition,]) %>%
  median_hdi(.width = c(0.95))

modOUT_dEstRSF %>%
  spread_draws(`r_species:indi`[condition,]) %>%
  median_hdi(.width = c(0.95))

(r2scores <- performance::r2_bayes(modOUT_dEstRSF))

varsToPlot

(rsfEffectPlot <- modOUT_dEstRSF %>%
    spread_draws(`b_.*`, regex = TRUE) %>%
    select(-.chain, -.iteration, -.draw) %>%
    melt() %>%
    filter(!variable %in% c("b_Intercept")) %>%
    mutate(
      variable = case_when(
        variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
        variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
        variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
        variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
        variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
        variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
        variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
        variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified",
        variable == "b_weightingScaled" ~ "\u03B2 Weighting of Used Points"
      )
    ) %>%
    ggplot(aes(x = value, y = variable)) +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    stat_slab(aes(fill = after_stat(x > 0)), fill_type = "gradient",
              alpha = 1) +
    stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                       point_interval = median_hdci, .width = c(.66, .95),
                       stroke = 1.25, colour = palette[c("coreGrey")]) +
    stat_summary(aes(colour = after_stat(x) > 0),
                 position = position_dodge(width = 0.2, preserve = "single"),
                 fun = median, size = 0.25) +
    scale_fill_manual(values = unname(palette[c("BADGER", "2")])) +
    scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +
    annotate("segment", x = 0.01, xend = 0.5, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["2"]) +
    annotate("text", x = 0.5, y = -0.1, label = "Greater deviation\nfrom median preference estimate",
             colour = palette["2"], hjust = 0, vjust = 0.5, lineheight = 0.95) +
    annotate("segment", x = -0.01, xend = -0.5, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["BADGER"]) +
    annotate("text", x = -0.5, y = -0.1, label = "Closer to\nmedian preference estimate",
             colour = palette["BADGER"], hjust = 1, vjust = 0.5, lineheight = 0.95) +
    coord_cartesian(clip = "off", ylim = c(1, NA)) +
    labs(x = "Estimated Beta", y = "") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.title.x = element_text(margin = margin(60,0,0,0)),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"))

ggsave(rsfEffectPlot,
       filename = here("notebook", "figures", "rsfEffectPlot.png"),
       dpi = 300, width = 210, height = 140,
       units = "mm")

# wides results -----------------------------------------------------------

(r2scores <- performance::r2_bayes(modOUT_dEstWides))

varsToPlot

(widesEffectPlot <- modOUT_dEstWides %>%
    spread_draws(`b_.*`, regex = TRUE) %>%
    select(-.chain, -.iteration, -.draw) %>%
    melt() %>%
    filter(!variable %in% c("b_Intercept")) %>%
    mutate(
      variable = case_when(
        variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
        variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
        variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
        variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
        variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
        variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
        variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
        variable == "b_samplingPatternst" ~ "\u03B2 Sampling Pattern: Stratified"
      )
    ) %>%
    ggplot(aes(x = value, y = variable)) +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    stat_slab(aes(fill = after_stat(x > 0)), fill_type = "gradient",
              alpha = 1) +
    stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                       point_interval = median_hdci, .width = c(.66, .95),
                       stroke = 1.25, colour = palette[c("coreGrey")]) +
    stat_summary(aes(colour = after_stat(x) > 0),
                 position = position_dodge(width = 0.2, preserve = "single"),
                 fun = median, size = 0.25) +
    scale_fill_manual(values = unname(palette[c("BADGER", "2")])) +
    scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +
    annotate("segment",
             x = 0.01, xend = 0.05, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["2"]) +
    annotate("text",
             x = 0.05, y = -0.1,
             label = "Greater deviation\nfrom median preference estimate",
             colour = palette["2"], hjust = 0, vjust = 0.5, lineheight = 0.95) +
    annotate("segment",
             x = -0.01, xend = -0.05, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["BADGER"]) +
    annotate("text",
             x = -0.05, y = -0.1,
             label = "Closer to\nmedian preference estimate",
             colour = palette["BADGER"], hjust = 1, vjust = 0.5, lineheight = 0.95) +
    coord_cartesian(clip = "off", ylim = c(1, NA)) +
    labs(x = "Estimated Beta", y = "") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.title.x = element_text(margin = margin(60,0,0,0)),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"))

ggsave(widesEffectPlot,
       filename = here("notebook", "figures", "widesEffectPlot.png"),
       dpi = 300, width = 210, height = 140,
       units = "mm")

# ssf results -----------------------------------------------------------

(r2scores <- performance::r2_bayes(modOUT_dEstSSF))

varsToPlot

(ssfEffectPlot <- modOUT_dEstSSF %>%
    spread_draws(`b_.*`, regex = TRUE) %>%
    select(-.chain, -.iteration, -.draw) %>%
    melt() %>%
    filter(!variable %in% c("b_Intercept")) %>%
    mutate(
      variable = case_when(
        variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
        variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
        variable == "b_modelFormmf.ss" ~ "\u03B2 Model Formula: Step Selection",
        variable == "b_stepDistgamma" ~ "\u03B2 Step Distribution: Gamma",
        variable == "b_turnDistvonmises" ~ "\u03B2 Turn Distribution: Von Mises",
        variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
        variable == "b_availablePerStepScaled" ~ "\u03B2 Available Points Per Step"
      )
    ) %>%
    ggplot(aes(x = value, y = variable)) +
    geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.9, colour = "#403F41",
               linetype = 1) +
    stat_slab(aes(fill = after_stat(x > 0)), fill_type = "gradient",
              alpha = 1) +
    stat_pointinterval(position = position_dodge(width = 0.4, preserve = "single"),
                       point_interval = median_hdci, .width = c(.66, .95),
                       stroke = 1.25, colour = palette[c("coreGrey")]) +
    stat_summary(aes(colour = after_stat(x) > 0),
                 position = position_dodge(width = 0.2, preserve = "single"),
                 fun = median, size = 0.25) +
    scale_fill_manual(values = unname(palette[c("BADGER", "2")])) +
    scale_colour_manual(values = unname(palette[c("BADGER", "2")])) +
    annotate("segment",
             x = 0.01, xend = 0.05, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["2"]) +
    annotate("text",
             x = 0.05, y = -0.1,
             label = "Greater deviation\nfrom median preference estimate",
             colour = palette["2"], hjust = 0, vjust = 0.5, lineheight = 0.95) +
    annotate("segment",
             x = -0.01, xend = -0.05, y = -0.1, yend = -0.1,
             linewidth = 1.25,
             arrow = arrow(angle = 30, type = "closed", length = unit(2, "mm")),
             colour = palette["BADGER"]) +
    annotate("text",
             x = -0.05, y = -0.1,
             label = "Closer to\nmedian preference estimate",
             colour = palette["BADGER"], hjust = 1, vjust = 0.5, lineheight = 0.95) +
    coord_cartesian(clip = "off", ylim = c(1, NA)) +
    labs(x = "Estimated Beta", y = "") +
    theme_bw() +
    theme(
      line = element_line(colour = palette["coreGrey"]),
      text = element_text(colour = palette["coreGrey"]),
      strip.background = element_blank(),
      strip.text = element_text(face = 4, hjust = 1, vjust = 1),
      # strip.text.y.left = element_text(angle = 0, margin = margin(-8,10,0,0)),
      # axis.text.y.left = element_text(margin = margin(0,-119,0,80)), # 2nd value needed to alligns with facet, 4th gives space left
      axis.title.x = element_text(margin = margin(60,0,0,0)),
      axis.ticks.y.left = element_blank(),
      axis.line.x = element_line(),
      strip.clip = "off",
      panel.border = element_blank(),
      panel.spacing = unit(18, "pt"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"))

ggsave(ssfEffectPlot,
       filename = here("notebook", "figures", "ssfEffectPlot.png"),
       dpi = 300, width = 210, height = 140,
       units = "mm")
