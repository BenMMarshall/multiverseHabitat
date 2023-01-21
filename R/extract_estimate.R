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
    OUTDF <- data.frame("Estimate" = NA,
                        "SE" = NA,
                        "Method" = "wides")
    return(OUTDF)
  }

  if(class(OUT)[1] == "wiIII"){

    if(any(names(OUT$wi) %in% "c2")){

      OUTDF <- data.frame("Estimate" = OUT$wi["c2"],
                          "SE" = OUT$se.wi["c2"],
                          "Method" = "wides")
    } else {
      OUTDF <- data.frame("Estimate" = NA,
                          "SE" = NA,
                          "Method" = "wides")
    }

    return(OUTDF)

  } else if(class(OUT)[1] == "data.frame"){

    if(OUT$method[1] == "rsf"){

      # coefDF <- summary(OUT)$coefficients
      coefDF <- OUT
      if(any(rownames(coefDF) %in% "valuesc2")){

        OUTDF <- data.frame(coefDF[rownames(coefDF) == "valuesc2",][1],
                            coefDF[rownames(coefDF) == "valuesc2",][2],
                            "rsf")
        names(OUTDF) <- c("Estimate", "SE", "Method")
        return(OUTDF)

      } else {

        OUTDF <- data.frame(NA,
                            NA,
                            "rsf")
        names(OUTDF) <- c("Estimate", "SE", "Method")
        return(OUTDF)

      }


    } else if(OUT$method[1] == "ssf"){

      # } else if(class(OUT)[1] == "fit_clogit"){

      # coefDF <- summary(OUT)$coefficients
      coefDF <- OUT

      if(any(rownames(coefDF) %in% "valuesc2")){

        OUTDF <- data.frame(coefDF[rownames(coefDF) == "valuesc2",][1],
                            coefDF[rownames(coefDF) == "valuesc2",][3],
                            "ssf")
        names(OUTDF) <- c("Estimate", "SE", "Method")
        return(OUTDF)

      } else{

        OUTDF <- data.frame(NA,
                            NA,
                            "ssf")
        names(OUTDF) <- c("Estimate", "SE", "Method")
        return(OUTDF)
      }
    }

  } # if end
} # func end
