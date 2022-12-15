#' Extract point estimates and SE
#'
#' @name extract_estimate
#' @description A
#' @param OUT the output from any of the method functions
#' @return A dataframe with the
#'
#' @export
extract_estimate <- function(OUT){

  if(!require(adehabitatHS)){
    stop("adehabitatHS not installed")
  }
  if(!require(amt)){
    stop("amt not installed")
  }

  if(class(OUT)[1] == "character"){
    return(
      data.frame("Estimate" = NA,
                 "SE" = NA)
    )
  }

  if(class(OUT)[1] == "wiIII"){

    return(
      data.frame("Estimate" = OUT$wi["c2"],
                 "SE" = OUT$se.wi["c2"])
    )

  } else if(class(OUT)[1] == "glm"){

    coefDF <- summary(OUT)$coefficients
    return(
      data.frame("Estimate" = coefDF[rownames(coefDF) == "valuesc2",][1],
                 "SE" = coefDF[rownames(coefDF) == "valuesc2",][2])
    )

  } else if(class(OUT)[1] == "fit_clogit"){

    coefDF <- summary(OUT)$coefficients

    return(
      data.frame("Estimate" = coefDF[rownames(coefDF) == "valuesc2",][1],
                 "SE" = coefDF[rownames(coefDF) == "valuesc2",][3])
    )

  } # if end
} # func end
