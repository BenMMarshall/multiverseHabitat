#' Subset simulated tracking data
#'
#' @name subset_frequency
#' @description A function to subset simulated tracked data, either by frequency
#'   or by duration. The input data.frame must have a POSIXct datetime column.
#' @param movementData The locations data.frame from the abmAnimalMovement
#'   simulation.
#' @param freqPreset Must be one of the following: 0.5, 1.0, 2.0, 6.0, 12.0,
#'   24.0, 48.0, 168.0 hours
#' @param daysDuration Duration in days
#' @return A data.frame reduced down to desired duration and frequency preset.
#'
#' @export
subset_frequency <- function(movementData, freqPreset){

  movementData$hour <- as.numeric(substr(movementData$datetime, 12, 13))
  # instances returning NA, replace with 0
  movementData$hour[is.na(movementData$hour)] <- 0
  movementData$minute <- as.numeric(substr(movementData$datetime, 15, 16))
  # instances returning NA, replace with 0
  movementData$minute[is.na(movementData$minute)] <- 0
  movementData$yday <- as.numeric(format(movementData$datetime,"%j"))

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

#' @export
subset_duration <- function(movementData, daysDuration){

  sub_OUT <- movementData[movementData$datetime <
                            as.POSIXct("2022-01-01 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S") + 60*60*24* daysDuration,]
  return(sub_OUT)

}
