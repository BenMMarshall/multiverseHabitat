#' Extract choices from combined results
#'
#' @name parse_combined_results
#' @description A
#' @param combResults a
#' @return a
#'
#' @export
parse_combined_results <- function(combResults){

  # branches contain all info
  combResults$branches <- rownames(combResults)

  # analysis
  combResults$analysis <- stringr::str_extract(combResults$branches, "wides|rsf|ssf")

  # species
  combResults$species <- stringr::str_extract(combResults$branches, "BADGER|VULTURE|KINGCOBRA")

  # individuals
  indi <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -1]
  })
  combResults$indi <- as.numeric(indi)

  # tracking frequency
  tf <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -2]
  })
  # convert tf to points/hour to help interpretation
  combResults$tf <- 1/as.numeric(tf)

  # tracking duration
  td <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -3]
  })
  combResults$td <- as.numeric(td)

  # area method
  combResults$area <- stringr::str_extract(combResults$branches, "MCP|KDEhref|AKDE|dBBMM")

  # points per step for SSF
  availablePerStep <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -4]
  })
  combResults$availablePerStep <- as.numeric(availablePerStep)

  # contour
  contour <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -5]
  })
  # adds NA for SSF
  combResults$contour <- as.numeric(contour)

  # also at contour is the start/end for the SSF models
  combResults$covarExtract <- ifelse(contour %in% c("start", "end"), contour, NA)
  # available points for wides and rsf
  availPointsPer <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -7]
  })
  # adds NA for SSF
  combResults$availPointsPer <- as.numeric(availPointsPer)
  # gets model form from the same location
  combResults$modelForm <- ifelse(availPointsPer %in% c("mf.is", "mf.ss"), availPointsPer, NA)
  # weighting for RSF only, but will pull out wides with 1's but doesn't matter
  # as it wont be used in the model
  weighting <- sapply(stringr::str_split(combResults$branches, "_"), function(x){
    x[length(x) -6]
  })
  combResults$weighting <- as.numeric(weighting)

  # prepare upper and lower based on SE
  combResults$upper <- combResults$Estimate + combResults$SE
  combResults$lower <- combResults$Estimate - combResults$SE
  # and a column that describes whether SE overlaps 0
  combResults$sigColour <-
    ifelse(combResults$upper > 0 & combResults$lower > 0, "preference",
           ifelse(combResults$upper < 0 & combResults$lower < 0, "avoidance",
                  "no effect"))

  return(combResults)
}
