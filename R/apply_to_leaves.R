#' Add a list to the leaves of a nested list
#'
#' @name apply_to_leaves
#' @description A function to added together nested lists, by adding one to
#'   every leaf of another.
#' @param nestedList The nested list whose leaves will be added to.
#' @param toNest The nested list to be added to each of the leaves.
#' @param pasteSep The character string that will be used to paste together list
#'   names.
#' @return A renamed nested list where the names describe the path to a list's
#'   location within the nested list.
#' @seealso prefix_all
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
#' nestedList <- build_nested_list(list1,
#'                                 list2,
#'                                 pasteSep = "__")
#'
#' combinedList <- apply_to_leaves(nestedList = nestedList,
#'                                 toNest = list3, pasteSep = "__")
#'
#' @export
apply_to_leaves <- function(nestedList, toNest, pasteSep){

  lapply(nestedList, function(x,
                              pasteSep = pasteSep){
    if (is.list(x)) {
      apply_to_leaves(x, toNest, pasteSep = pasteSep)
    } else {
      prefix_all(toNest, x, pasteSep = pasteSep)
    }
  }, pasteSep = pasteSep)
}
