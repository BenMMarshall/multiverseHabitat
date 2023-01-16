
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

targets::tar_load(combinedResults)
combinedResults <- multiverseHabitat::parse_combined_results(combinedResults)

names(combinedResults)

# parse combined results converts tf to points/hour to help interpretation
combinedResults$tf <- round(combinedResults$tf, digits = 2)

rsfModelData <- combinedResults %>%
  filter(analysis == "rsf") %>%
  mutate(medEst = median(Estimate, na.rm = TRUE),
         absDeltaEst = abs(Estimate - medEst)) %>%
  mutate(tfScaled = (tf-mean(tf))/sd(tf),
         tdScaled = (td-mean(td))/sd(td),
         availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
         # weightingScaled = (weighting-mean(weighting))/sd(weighting),
         #### PLACEHOLDER WEIGHTING UNTIL MORE THAN 1 VARIATION COMPLETE
         weightingScaled = weighting,
         # contour = as.factor(contour)
         contourScaled = (contour-mean(contour))/sd(contour))

widesModelData <- combinedResults %>%
  filter(analysis == "wides") %>%
  mutate(medEst = median(Estimate, na.rm = TRUE),
         absDeltaEst = abs(Estimate - medEst)) %>%
  mutate(tfScaled = (tf-mean(tf))/sd(tf),
         tdScaled = (td-mean(td))/sd(td),
         availPointsPerScaled  = (availPointsPer-mean(availPointsPer))/sd(availPointsPer),
         contourScaled = (contour-mean(contour))/sd(contour))

# model formula
# wides
formWides <- bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                  area + contourScaled + availPointsPerScaled +
                  (1|species/indi))
# rsf
formRSF <- bf(absDeltaEst ~ 1 + tdScaled + tfScaled +
                area + contourScaled + availPointsPerScaled + weightingScaled +
                (1|species/indi))
# ssf
# formSSF <- bf(xxx ~ 1 + td + tf +
#                 MethodSSF_mf + MethodSSF_ce + MethodSSF_as +
#                 (1|species/indi))
# AKA (1|species) + (1|species:indi) for a nested group effect intercept

get_prior(formRSF, data = rsfModelData)
get_prior(formWides, data = widesModelData)

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
brmpriorWides <- c(
  set_prior("cauchy(0.1, 3)", coef = "areadBBMM"),
  set_prior("cauchy(0.1, 3)", coef = "areaKDEhref"),
  set_prior("cauchy(0.1, 3)", coef = "areaMCP"),
  set_prior("cauchy(0.1, 3)", coef = "contourScaled"),
  set_prior("cauchy(0.1, 3)", coef = "availPointsPerScaled"),
  set_prior("cauchy(0.1, 3)", coef = "tdScaled"),
  set_prior("cauchy(0.1, 3)", coef = "tfScaled")
)

ggplot(data.frame("x" = rcauchy(2000, location = 0.1, scale = 1))) +
  geom_density(aes(x = x)) +
  coord_cartesian(xlim = c(-100, 100))

rsfModOUT_dEst <- brm(formula = formRSF,
                      data = rsfModelData,
                      family = gaussian,
                      prior = brmpriorRSF,
                      warmup = 2000, iter = 5000, chains = 4,
                      cores = 12,
                      thin = 2,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 15),
                      seed = 1,
                      save_pars = save_pars(all = TRUE),
                      save_model = "./notebook/prereg/modelOutputs/absDeltaEstModel_rsf.txt",
                      file = "./notebook/prereg/modelOutputs/absDeltaEstModel_rsf")

rsfModOUT_dEst

widesModOUT_dEst <- brm(formula = formWides,
                      data = widesModelData,
                      family = gaussian,
                      prior = brmpriorWides,
                      warmup = 2000, iter = 5000, chains = 4,
                      cores = 12,
                      thin = 2,
                      control = list(adapt_delta = 0.90,
                                     max_treedepth = 15),
                      seed = 1,
                      save_pars = save_pars(all = TRUE),
                      save_model = "./notebook/prereg/modelOutputs/absDeltaEstModel_wides.txt",
                      file = "./notebook/prereg/modelOutputs/absDeltaEstModel_wides")

widesModOUT_dEst

# Review convergence ------------------------------------------------------

## check how many need reviewing
varsToPlot <- get_variables(rsfModOUT_dEst)[2:10]
# varsToPlot <- get_variables(widesModOUT_dEst)[2:8]

png(file = "./notebook/prereg/modelOutputs/Traceplot.png", res = 300, width = 210, height = 140,
    units = "mm")
mcmc_trace(rsfModOUT_dEst, pars = varsToPlot)
dev.off()

png(file = "./notebook/prereg/modelOutputs/ACFplot.png", res = 300, width = 210, height = 140,
    units = "mm")
mcmc_acf(rsfModOUT_dEst, pars = varsToPlot)
dev.off()

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

tdScaledEffect <- rsfModOUT_dEst %>%
  spread_draws(b_tdScaled) %>%
  median_hdci(.width = c(0.95))

rsfModOUT_dEst %>%
  spread_draws(r_species[condition,]) %>%
  median_hdi(.width = c(0.95))

rsfModOUT_dEst %>%
  spread_draws(`r_species:indi`[condition,]) %>%
  median_hdi(.width = c(0.95))

r2scores <- performance::r2_bayes(rsfModOUT_dEst)

varsToPlot

rsfModOUT_dEst %>%
  spread_draws(`b_.*`, regex = TRUE) %>%
  select(-.chain, -.iteration, -.draw) %>%
  melt() %>%
  ### WEIGHTING FILTERED OUT UNTIL VARIATION
  filter(!variable %in% c("b_weightingScaled", "b_Intercept")) %>%
  mutate(
    variable = case_when(
      variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
      variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
      variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
      variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
      variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
      variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
      variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
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
    legend.position = "none")


# wides results -----------------------------------------------------------

r2scores <- performance::r2_bayes(widesModOUT_dEst)

varsToPlot

widesModOUT_dEst %>%
  spread_draws(`b_.*`, regex = TRUE) %>%
  select(-.chain, -.iteration, -.draw) %>%
  melt() %>%
  ### WEIGHTING FILTERED OUT UNTIL VARIATION
  filter(!variable %in% c("b_weightingScaled", "b_Intercept")) %>%
  mutate(
    variable = case_when(
      variable == "b_tdScaled" ~ "\u03B2 Tracking Duration",
      variable == "b_tfScaled" ~ "\u03B2 Tracking Frequency",
      variable == "b_areaMCP" ~ "\u03B2 Available Area: MCP",
      variable == "b_areadBBMM" ~ "\u03B2 Available Area: dBBMM",
      variable == "b_areaKDEhref" ~ "\u03B2 Available Area: KDEhref",
      variable == "b_contourScaled" ~ "\u03B2 Available Area Contour",
      variable == "b_availPointsPerScaled" ~ "\u03B2 Available Points Multipiler",
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
    legend.position = "none")
