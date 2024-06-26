#' Render the manuscript rmd
#'
#' @name render_rmd
#' @description A
#' @param extractedValues table of R2 and effects for quick reference in the rmd
#' @return Nothing, PDF (or output) will be saved to a folder.
#'
#' @export
render_rmd <- function(extractedValues,
                       ...){

  rmarkdown::render(input = here::here("notebook", "manuscript",
                                       "multiverseHabitatManuscript.Rmd"),
                    output_file = here::here("notebook", "manuscript",
                                             "multiverseHabitatManuscript.pdf"))
}
