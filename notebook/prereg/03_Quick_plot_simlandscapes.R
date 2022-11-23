tesland <- multiverseHabitat::simulate_landscape(species = "VULTURE", seed = 2022)

tesland$classified
library(reshape2)
targetList <- tesland
combinedLayers <- do.call(rbind, lapply(names(targetList), function(x){
  currLayer <- targetList[[x]]
  layerDF <- melt(currLayer, c("col", "row"))
  layerDF$layer <- x
  return(layerDF)
}))
combinedLayers$layer <- factor(combinedLayers$layer, levels = c("shelter", "forage", "movement",
                                                                "classified"))

ggplot(combinedLayers) +
  geom_raster(aes(x = col, y = row, fill = value)) +
  facet_wrap(.~layer) +
  scale_fill_gradient(limits = c(0, 1))
