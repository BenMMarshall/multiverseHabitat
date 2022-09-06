
# Sampling prelim code ----------------------------------------------------

movementData <- read.csv(here("notebook", "prereg", "prelimData.csv"))
movementData$datetime <- as.POSIXct(movementData$timestep * 60,
                                    origin = "2022-01-01")

library(lubridate)
library(dplyr)

movementData %>%
  select(behave, datetime)

library(ggplot2)
movementData %>%
  select(behave, datetime) %>%
  mutate(
    time = as.POSIXct(paste0("2022-01-01",
                             hour(datetime), ":", minute(datetime)),
                      format = "%Y-%m-%d %H:%M"),
    month = month(datetime)) %>%
  ggplot() +
  geom_point(aes(x = time, y = behave, colour = month), position = position_jitter(height = 0.4))

# tracking frequency hours
sam_02_tf <- c("tf.0.5", "tf.01", "tf.02", "tf.06", "tf.12", "tf.24", "tf.48", "tf.168")
# tracking duration days
sam_03_td <- c("td.007", "td.015", "td.030", "td.060", "td.120", "td.240", "td.365")

movementData$hour <- as.numeric(substr(movementData$datetime, 12, 13))
movementData$minute <- as.numeric(substr(movementData$datetime, 15, 16))
movementData$yday <- as.numeric(format(movementData$datetime,"%j"))


subset_frequency <- function(movementData, freqPreset){

  if(freqPreset == 0.5){
    # every 0.5 hours
    sub_OUT <- movementData[movementData$minute == 0 |
                              movementData$minute == 30,]

  } else if(freqPreset == 1){
    # every 1 hours
    sub_OUT <- movementData[movementData$minute == 0,]

  } else if(freqPreset == 2){
    # every 2 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              movementData$hour %% 2 == 0,]

  } else if(freqPreset == 6){
    # every 6 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              movementData$hour %% 6 == 0,]

  } else if(freqPreset == 12){
    # every 12 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              (movementData$hour == 6 |
                                 movementData$hour == 18),]

  } else if(freqPreset == 24){
    # every 24 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              movementData$hour == 12,]

  } else if(freqPreset == 48){
    # every 48 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              movementData$hour == 12 &
                              movementData$yday %% 2 == 0,]

  } else if(freqPreset == 168){
    # every 168 hours
    sub_OUT <- movementData[movementData$minute == 0 &
                              movementData$hour == 12 &
                              movementData$yday %% 7 == 0,]

  }

  return(sub_OUT)

}



subset_duration <- function(movementData, daysDuration){

  sub_OUT <- movementData[movementData$datetime <
                            as.POSIXct("2022-01-01 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S") + 60*60*24* daysDuration,]
  return(sub_OUT)

}

# subset_duration(subset_frequency(movementData,
#                                  freqPreset = 2),
#                 1)

samplingCombinations_full <- expand.grid(sam_02_tf, sam_03_td)
names(samplingCombinations_full) <- c("tf", "td")
samplingCombinations <- samplingCombinations_full
samplingCombinations$tf <- as.numeric(sub("tf\\.", "", samplingCombinations$tf))
samplingCombinations$td <- as.numeric(sub("td\\.", "", samplingCombinations$td))

subset_duration(subset_frequency(movementData,
                                 freqPreset = samplingCombinations[1,1]),
                daysDuration = samplingCombinations[1,2])

sampledDataList <- apply(samplingCombinations, 1, function(x){
  # print(paste(x[1], x[2]))
  subset_duration(subset_frequency(movementData,
                                   freqPreset = x[1]),
                  daysDuration = x[2])
})

names(sampledDataList) <-
  apply(samplingCombinations_full, 1,
        function(x) paste0(x[1], "__", x[2]))

dir.create(here("notebook", "prereg", "prelimMultiData"))
usethis::use_git_ignore("prelimMultiData")

lapply(names(sampledDataList), function(x){
  write.csv(sampledDataList[[x]],
            file = here("notebook", "prereg", "prelimMultiData",
                        paste0(x, "_sampled.csv")),
            row.names = FALSE)
  print(paste0(x, "_sampled.csv"))
})
