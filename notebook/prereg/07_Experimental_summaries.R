
targets::tar_load("methOUT_method_indi_wides_10_1_90_dBBMM_0.5_7_1_badger")

class(methOUT_method_indi_wides_10_1_90_dBBMM_0.5_7_1_badger)[1]
methOUT_method_indi_wides_10_1_90_dBBMM_0.5_7_1_badger$wi["c2"]
methOUT_method_indi_wides_10_1_90_dBBMM_0.5_7_1_badger$se.wi["c2"]

targets::tar_load("methOUT_method_indi_rsf_10_1_90_MCP_0.5_7_1_badger")

class(methOUT_method_indi_rsf_10_1_90_MCP_0.5_7_1_badger)[1]
coefDF <- summary(methOUT_method_indi_rsf_10_1_90_MCP_0.5_7_1_badger)$coefficients

# point est
coefDF[rownames(coefDF) == "valuesc2",][1]
# se
coefDF[rownames(coefDF) == "valuesc2",][2]


targets::tar_load("ssfOUT_mf.is_start_10_0.5_7_1_badger")
class(ssfOUT_mf.is_start_10_0.5_7_1_badger)[1]

coefDF <- summary(ssfOUT_mf.is_start_10_0.5_7_1_badger)$coefficients

# point est
coefDF[rownames(coefDF) == "valuesc2",][1]
# se
coefDF[rownames(coefDF) == "valuesc2",][3]

targets::tar_load("methOUT_method_indi_wides_10_1_90_dBBMM_1_7_4_badger")
targets::tar_delete("methOUT_method_indi_wides_1_1_90_MCP_1_15_1_badger")
# summary plots draft -----------------------------------------------------

targets::tar_load(combinedResults)

combinedResults

combinedResults$branches <- rownames(combinedResults)

combinedResults$analysis <- stringr::str_extract(combinedResults$branches, "wides|rsf|ssf")
combinedResults$species <- stringr::str_extract(combinedResults$branches, "badger|vulture|kingcobra")

indi <- sapply(stringr::str_split(combinedResults$branches, "_"), function(x){
  x[length(x) -1]
})
td <- sapply(stringr::str_split(combinedResults$branches, "_"), function(x){
  x[length(x) -2]
})
tf <- sapply(stringr::str_split(combinedResults$branches, "_"), function(x){
  x[length(x) -3]
})

combinedResults$indi <- indi
combinedResults$td <- td
combinedResults$tf <- tf

combinedResults$upper <- combinedResults$Estimate + combinedResults$SE
combinedResults$lower <- combinedResults$Estimate - combinedResults$SE

combinedResults$sigColour <-
  ifelse(combinedResults$upper > 0 & combinedResults$lower > 0, "preference",
         ifelse(combinedResults$upper < 0 & combinedResults$lower < 0, "avoidance",
                "no effect"))

combinedResults$sigColour

library(ggplot2)
library(dplyr)
library(ggridges)

palette <- c("#AD6DED", "#7D26D4", "#4F0E99", "#E87D13", "#965A1D", "#302010", "#403F41")
names(palette) <- c("KING\nCOBRA", "VULTURE", "BADGER", "2", "1", "0", "coreGrey")

combinedResults %>%
  arrange(td) %>%
  # mutate(branches = factor(branches,
  #                          levels = branches)) %>%
  ggplot() +
  # geom_segment(aes(x = Estimate - SE, xend = Estimate + SE,
  #                  y = branches, yend = branches, colour = sigColour)) +
  geom_point(aes(x = Estimate, y = td, colour = sigColour),
             position = position_jitter(height = 0.1)) +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.45, colour = "#403F41",
             linetype = 2) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("red", "grey25", "blue"), na.value = "#403F41") +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank())

# remove the spcies and indi from the branches as they are acting as our repeats,
# that way each branch (y axis) has species*indi points
combinedResults$choices <- sub("badger|vulture|kingcobra", "", combinedResults$branches)
combinedResults$choices <- sub(".{3}$", "", combinedResults$choices)

combinedResults %>%
  group_by(choices) %>%
  mutate(meanEst = mean(Estimate, na.rm = TRUE),
         minEst = min(Estimate, na.rm = TRUE),
         maxEst = max(Estimate, na.rm = TRUE),
         analysis = case_when(
           analysis == "wides" ~ "Wides",
           analysis == "rsf" ~ "Resource Selection Function",
           analysis == "ssf" ~ "(Integrated) Step Selection Function",
           TRUE ~ analysis)
         ) %>%
  ggplot(aes(y = reorder(choices, Estimate, FUN = mean, na.rm = TRUE))) +
  geom_segment(aes(x = minEst, xend = maxEst, yend = choices),
               alpha = 0.1) +
  geom_point(aes(x = Estimate, colour = sigColour),
             position = position_jitter(height = 0.01, width = 0),
             size = 0.75) +
  geom_point(aes(x = meanEst,)) +
  geom_vline(xintercept = 0, linewidth = 0.5, alpha = 0.45, colour = "#403F41",
             linetype = 2) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("#E87D13", "#403F41", "#4F0E99"), na.value = "#000000") +
  labs(y = "Choice path", x = "Estimated habitat preference for C2") +
  theme_bw() +
  theme(
    line = element_line(colour = palette["coreGrey"]),
    text = element_text(colour = palette["coreGrey"]),
    strip.background = element_blank(),
    strip.text = element_text(face = 4, hjust = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank())


