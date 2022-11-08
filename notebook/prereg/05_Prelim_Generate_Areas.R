library(here)
library(multiverseHabitat)

dir.create(here("notebook", "prereg", "prelimMultiData", "polyData"))
fileNames <- list.files(here("notebook", "prereg", "prelimMultiData", "dataSubset"),
                        pattern = ".csv")
# c("MCP", "KDE_LSCV", "KDE_href", "AKDE", "dBBMM")
# As our most infrequent tracking is 168 hours (1 week), we will set the window
# to the number of data points collected over 168 hours, and a margin of 48
# hours

# window == 168
# margin == 48

# tracking frequency hours
# sam_02_tf <- c("tf.0.5", "tf.01", "tf.02", "tf.06", "tf.12", "tf.24", "tf.48", "tf.168")
# tracking duration days
# sam_03_td <- c("td.007", "td.015", "td.030", "td.060", "td.120", "td.240", "td.365")

contours <- c(90, 95, 99)

# library(foreach)
# library(doParallel)
# # setup parallel backend
# cores <- detectCores()
# cl <- makeCluster(cores[1]-1)
# registerDoParallel(cl)
# foreach(file = fileNames) %dopar% {
#   # inner bits of the loop here
# }
# # stop cluster
# stopCluster(cl)
### NEED TO ADD LIBRARY CALLS INSIDE LOOP FOR PARALLEL

for(file in fileNames){
  print(file)
  # file <- fileNames[1]
  tfRaw <- strsplit(file, "__", fixed = TRUE)[[1]][3]
  tfNum <- as.numeric(sub("tf.", "", tfRaw))

  wsDP <- 168/tfNum
  mrgDP <- 48/tfNum

  # have to be odd for dBBMM
  if(wsDP %% 2 == 0){
    wsDP <- wsDP -1
  }
  if(mrgDP %% 2 == 0){
    mrgDP <- mrgDP -1
  }

  movementData <- read.csv(file = here("notebook", "prereg", "prelimMultiData", "dataSubset",
                                       file))
  movementData$datetime <- as.POSIXct(movementData$datetime)

  # dim(movementData)[1]

  for(mthd in c("MCP",
                # "KDE_LSCV",
                "KDE_href", "AKDE", "dBBMM")){

    print(mthd)
      # mthd <- "MCP"
      if(mthd == "MCP"){
        mthdTreeName <- "aa.mp"
      } else if(mthd == "KDE_href"){
        mthdTreeName <- "aa.hr"
      } else if(mthd == "KDE_LSCV"){
        mthdTreeName <- "aa.ls"
      } else if(mthd == "AKDE"){
        mthdTreeName <- "aa.ak"
      } else if(mthd == "dBBMM"){
        mthdTreeName <- "aa.db"
      }

      saveLocations <- here("notebook", "prereg", "prelimMultiData", "polyData",
           paste0(sub("_sampled.csv", "", file),
                  "__", mthdTreeName,
                  "__ac.", contours, ".RData"))

      # if already complete and the file date are newer than the simdata files, skip
      if(all(file.exists(saveLocations))){
        if(all(
          file.info(saveLocations[grep("ac.90", saveLocations)])$ctime >
          file.info(here("notebook", "prereg", "prelimMultiData", "dataSubset",
                         file))$ctime,
          file.info(saveLocations[grep("ac.95", saveLocations)])$ctime >
          file.info(here("notebook", "prereg", "prelimMultiData", "dataSubset",
                         file))$ctime,
          file.info(saveLocations[grep("ac.99", saveLocations)])$ctime >
          file.info(here("notebook", "prereg", "prelimMultiData", "dataSubset",
                         file))$ctime)){
        }
        print("Not updated ---")
        {next}
      } else {
        # generate or overwrite the areas
        polyOut <- build_available_area(movementData,
                                        contour = contours,
                                        method = mthd,
                                        dBBMMsettings = c(wsDP, mrgDP))

        p90 <- polyOut[[1]]
        p95 <- polyOut[[2]]
        p99 <- polyOut[[3]]

        save(p90, file = saveLocations[grep("ac.90", saveLocations)])
        save(p95, file = saveLocations[grep("ac.95", saveLocations)])
        save(p99, file = saveLocations[grep("ac.99", saveLocations)])
      }

  }
}

# load(here("notebook", "prereg", "prelimMultiData", "polyData",
#      "sp.B__i001__tf.0.5__td.007__aa.mp__ac.95.RData"))

