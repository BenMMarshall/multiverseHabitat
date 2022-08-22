#' Convert a vector to a named list
#'
#' @name vec_to_list
#' @description A function to take a vector and convert it into a list where
#'   each slot of the list contains a vector element and is named the same.
#' @param inVector A vector.
#' @return A named list, with slots and names equal to the input vector.
#'
#' @examples
#'
#' vec_to_list(c("a", "b", "c"))
#'
#' @export
vec_to_list <- function(inVector){
  OutList <- as.list(inVector)
  names(OutList) <- inVector
  return(OutList)
}
