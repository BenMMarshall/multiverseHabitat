targets::tar_load(areaResults)
targets::tar_load(ssfResults)

palette <- multiverseHabitat::get_palette()

areaResults <- multiverseHabitat::parse_combined_results(areaResults)

rsfResults <- areaResults %>%
  dplyr::filter(analysis == "rsf")

rsfUncertainEstimate <- rsfResults %>%
  mutate(uncertainty = Upper - Lower) %>%
  ggplot() +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
             linetype = 2) +
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
             linetype = 2) +
  geom_point(aes(x = Estimate, y = uncertainty, colour = Estimate), alpha = 0.05) +
  labs(y = "Range of 95%\nconfidence intervals", x = "Estimate") +
  scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
  scale_y_log10(breaks = c(c(0,1,5) %o% 10^(1:7))) +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    strip.background = element_blank(),
    axis.title.y = element_text(hjust = 1, vjust = 1,
                                angle = 0),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    strip.clip = "off",
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(18, "pt"),
    panel.grid = element_blank())


ssfResults <- multiverseHabitat::parse_combined_results(ssfResults)

ssfUncertainEstimate <- ssfResults %>%
  mutate(uncertainty = Upper - Lower) %>%
  ggplot() +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
             linetype = 2) +
  geom_hline(yintercept = 0, linewidth = 0.5, alpha = 0.5, colour = "#403F41",
             linetype = 2) +
  geom_point(aes(x = Estimate, y = uncertainty, colour = Estimate), alpha = 0.05) +
  labs(y = "Range of 95%\nconfidence intervals", x = "Estimate") +
  scale_colour_gradient2(low = palette["BADGER"], mid = palette["coreGrey"], high = palette["2"]) +
  scale_y_log10(breaks = c(c(0,1,5) %o% 10^(1:7))) +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    strip.background = element_blank(),
    axis.title.y = element_text(hjust = 1, vjust = 1,
                                angle = 0),
    axis.text.y = element_text(margin = margin(0, 5, 0, 10)),
    axis.line.y = element_line(),
    axis.line.x = element_line(),
    strip.clip = "off",
    legend.position = "none",
    panel.border = element_blank(),
    panel.spacing = unit(18, "pt"),
    panel.grid = element_blank())

(rsfUncertainEstimate +
    theme(axis.title.x = element_blank())) /
  (ssfUncertainEstimate +
     theme(axis.title.y = element_blank()))

