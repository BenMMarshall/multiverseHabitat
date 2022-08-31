#' Build a multi-layered nested list
#'
#' @name build_nested_list
#' @description A function to nest any number of lists within each other. The
#'   first list provided has the second list nested within each of the slots.
#'   Each of the slots of the second list will have the third list nested
#'   within. This will repeat recursively to generate a "tree" of nested lists.
#'   The names of all lower lists have the names of higher lists pasted onto
#'   them, making all list slots equal to the path to that location within the
#'   tree/nested list.
#' @param ... any number of lists to be nested within each other, from level 1
#'   downwards.
#' @param pasteSep The separator between the pasted names to reflect the path to
#'   a given slot within the nested list. Passed to the `nest_list()` function.
#' @return A nested list with the number of levels equal to the number of lists
#'   provided.
#' @seealso nest_list
#'
#' @examples
#'
#' vector1 <- c("a", "b", "c")
#' vector2 <- c("e", "f", "g", "h")
#' vector3 <- c("i", "j")
#'
#' list1 <- vec_to_list(vector1)
#' list2 <- vec_to_list(vector2)
#' list3 <- vec_to_list(vector3)
#'
#' build_nested_list(list1,
#'                   list2,
#'                   list3, pasteSep = "__")
#'
#' @export
build_nested_list <- function(..., pasteSep){
  inList <- list(...)
  nItems <- length(inList)
  for(n in 2:(nItems)){
    if(n == 2){
      nested_list <- nest_list(inList[[n-1]], inList[[n]], pasteSep = pasteSep)
      names(nested_list) <- inList[[n-1]]
    } else {
      nested_list <- lapply(nested_list, function(x, pasteSep = pasteSep){
        if (is.list(x)) {
          nest_list(x, inList[[n]], pasteSep = pasteSep)
        } else {
          inList[[n]]
        }
      }, pasteSep = pasteSep)
    }
  }
  return(nested_list)
}
