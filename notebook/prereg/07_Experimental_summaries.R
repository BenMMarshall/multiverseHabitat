
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

ggplot(combinedResults) +
  geom_segment(aes(x = Estimate - SE, xend = Estimate + SE,
                   y = branches, yend = branches, colour = sigColour)) +
  geom_point(aes(x = Estimate, y = branches, colour = sigColour)) +
  facet_wrap(.~analysis, ncol = 1, scales = "free") +
  scale_colour_manual(values = c("red", "grey25", "blue"), na.value = "black")

order(combinedResults$branches, combinedResults$indi)
