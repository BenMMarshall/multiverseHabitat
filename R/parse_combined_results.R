#' Extract choices from combined results
#'
#' @name parse_combined_results
#' @description A
#' @param combResults a
#' @return a
#'
#' @export
parse_combined_results <- function(combResults){
  # combResults <- areaResults
  # combResults <- ssfResults

  # branches contain all info
  combResults$branches <- rownames(combResults)

  # if statement to catch wRSF results that won't have the rowname numbers on the end
  if(!is.na(as.numeric(substr(combResults$branches[1],
                              nchar(combResults$branches[1]),
                              nchar(combResults$branches[1]))))){
    # remove the unique numbers added cos they were rownames
    combResults$branches <- sub("\\..[^\\.]*$", "", combResults$branches)
  }

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

  # prepare upper and lower based on SE
  # combResults$upper <- combResults$Estimate + combResults$SE
  # combResults$lower <- combResults$Estimate - combResults$SE
  # and a column that describes whether SE overlaps 0
  combResults$sigColour <-
    ifelse(combResults$Upper > 0 & combResults$Lower > 0, "preference",
           ifelse(combResults$Upper < 0 & combResults$Lower < 0, "avoidance",
                  "no effect"))

  return(combResults)
}
