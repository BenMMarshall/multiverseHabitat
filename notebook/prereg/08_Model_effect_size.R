
#### Code from:
### Lots of movement, little progress: A review of reptile home range literature
## Matt Crane*, Inês Silva, Benjamin Michael Marshall, Colin Thomas Strine**
# * mattecology@gmail.com **strine.conservation@gmail.com
# bioRxiv Preprint DOI: https://doi.org/10.1101/2020.12.03.409292

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

targets::tar_load(combinedResults)
combinedResults

# model formula
# wides
formWides <- bf(xxx ~ 1 + td + tf +
                  areaMethod + areaContour + Method_ap +
                  (1|species/indi))
# rsf
formRSF <- bf(xxx ~ 1 + td + tf +
                areaMethod + areaContour + Method_ap + Method_we +
                (1|species/indi))
# ssf
formSSF <- bf(xxx ~ 1 + td + tf +
                MethodSSF_mf + MethodSSF_ce + MethodSSF_as +
                (1|species/indi))
# AKA (1|species) + (1|species:indi) for a nested group effect intercept

get_prior(formSSF, data = combinedResults)

# priors
brmprior <- c(set_prior("cauchy(0.1, 3)", coef = "log10mass"),
              set_prior("cauchy(0, 1)", class = "sd", group = "order"))

# ggpubr::ggdensity(rcauchy(2000, location = 0.1, scale = 3))

moddata <- combinedResults

brmfit <- brm(formula = brmform,
              data = moddata,
              family = gaussian,
              prior = brmprior,
              warmup = 2000, iter = 5000, chains = 4,
              cores = 6,
              thin = 2,
              control = list(adapt_delta = 0.90,
                             max_treedepth = 15),
              seed = 1,
              save_pars = save_pars(all = TRUE),
              save_model = "./notebook/prereg/modelOutputs/effectSizeModel.txt",
              file = "./notebook/prereg/modelOutputs/effectSizeModel")

brmfit

# Review convergence ------------------------------------------------------

## check how many need reviewing
varsToPlot <- get_variables(brmfit)[1:6]

png(file = "./notebook/prereg/modelOutputs/Traceplot.png", res = 300, width = 210, height = 140,
    units = "mm")
mcmc_trace(brmfit, pars = varsToPlot)
dev.off()

png(file = "./notebook/prereg/modelOutputs/ACFplot.png", res = 300, width = 210, height = 140,
    units = "mm")
mcmc_acf(brmfit, pars = varsToPlot)
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

# get_variables(brmfit)
#
# intercept <- brmfit %>%
#   spread_draws(b_Intercept) %>%
#   median_hdi(.width = c(0.95))
#
# interbase <- intercept[1:3]
# exp(interbase) / (1 + exp(interbase)) * 100
# # 0.062% chance of being studied, low of 0.017 to high of 0.181%
#
# log10massEffect <- brmfit %>%
#   spread_draws(b_log10mass) %>%
#   median_hdi(.width = c(0.95))
#
# brmfit %>%
#   spread_draws(r_order[condition,]) %>%
#   median_hdi(.width = c(0.95))
#
# r2scores <- performance::r2_bayes(brmfit)
#
# brmfit
#
# fittedVals <- fitted(brmfit, nsamples = 100, probs = c(0.025, 0.0975))
#
# moddata %>%
#   mutate(fitted = fittedVals[,1],
#          plotloc = case_when(
#            order == "Crocodylia" & studied == 0 ~ -0.1,
#            order == "Crocodylia" & studied == 1 ~ 1.4,
#            order == "Squamata (Serpentes)" & studied == 0 ~ -0.2,
#            order == "Squamata (Serpentes)" & studied == 1 ~ 1.3,
#            order == "Squamata (Sauria)" & studied == 0 ~ -0.3,
#            order == "Squamata (Sauria)" & studied == 1 ~ 1.2,
#            order == "Testudines" & studied == 0 ~ -0.4,
#            order == "Testudines" & studied == 1 ~ 1.1)
#   ) %>%
#   mutate(order = case_when(
#     order == "Squamata (Sauria)" ~ "Sauria",
#     order == "Squamata (Serpentes)" ~ "Serpentes",
#     TRUE ~ order)) %>%
#   ggplot() +
#   geom_hline(yintercept = c(0,1), linetype = 2, alpha = 0.25) +
#   geom_line(aes(x = log10mass, y = fitted, colour = order)) +
#   geom_point(aes(x = log10mass, y = plotloc, colour = order),
#              position = position_jitter(seed = 1), alpha = 0.5,
#              pch = 16) +
#   geom_text(data = data.frame(
#     label = c("Crocodylia", "Serpentes", "Sauria", "Testudines"),
#     locy = c(1.4, 1.3, 1.2, 1.1, -0.1, -0.2, -0.3, -0.4),
#     locx = rep(6.3, 8)),
#     aes(x = locx, y = locy, label = label, colour = label),
#     vjust = 0.5, hjust = 0, fontface = 2) +
#   geom_text(data = data.frame(
#     label = c("Not Studied", "Studied"),
#     locy = c(-0.05, 1.05),
#     locx = rep(-1.2, 2)),
#     aes(x = locx, y = locy, label = label),
#     vjust = 0.5, hjust = 0, fontface = 4) +
#   annotate("richtext", x = -1.2, y = 0.9,
#            label = paste0("<span style = 'font-size:9pt;'>Conditional <i>R<sup>2</sup></i>: <b>",
#                           round(r2scores$R2_Bayes, digits = 3),
#                           "</b><br>Population effect of mass: <b>",
#                           round(log10massEffect$b_log10mass, digits = 3),
#                           "</b><br></span><span style = 'font-size:7pt;'>(CrI 95%: ",
#                           round(log10massEffect$.lower, digits = 2), "–",
#                           round(log10massEffect$.upper, digits = 2), ")</span>"),
#            hjust = 0, vjust = 1, lineheight = 0.95,
#            label.color = NA,
#            label.padding = grid::unit(rep(0, 4), "pt")) +
#   scale_y_continuous(breaks = seq(0,1,0.25),
#                      limits = c(-0.45, 1.45)) +
#   scale_x_continuous(limits = c(-1.2, 6.3), expand = expansion(add = c(0.2, 0.2))) +
#   coord_cartesian(clip = "off") +
#   labs(y = "P(studied)", x = "Mass (log10 g)",
#        colour = "Clade") +
#   scale_colour_manual(values = scico(n = 4, palette = "roma", direction = 1)[c(1,3,2,4)]) +
#   theme_bw() +
#   theme(axis.title = element_text(face = 2),
#         axis.title.x = element_text(hjust = 1),
#         axis.line.x = element_line(),
#         axis.text.x = element_text(),
#         axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
#         legend.title = element_text(face = 2),
#         legend.position = "none",
#         plot.margin = unit(c(1,4,0.5,2), "lines"),
#         panel.spacing.x = unit(0,"line"),
#         panel.grid = element_blank(),
#         panel.border = element_blank(),
#         axis.line = element_line())
#
# ggsave("./Figures/Mass effect on study.png", width = 180,
#        height = 100,
#        dpi = 300, units = "mm")
# ggsave("./Figures/Mass effect on study.pdf", width = 180,
#        height = 100, units = "mm")
