#' Add prefix to a nested list
#'
#' @name prefix_all
#' @description A function to add a prefix to all named elements of a nested
#'   list. Used with `apply_to_leaves()`.
#' @param nestedList A named nested list.
#' @param prefix A character string that will be applied to the names of a given
#'   list.
#' @param pasteSep A character string that will be pasted between the names and
#'   prefix.
#' @return A renamed nested list.
#' @seealso apply_to_leaves
#'
#' @examples
#'
#' vector1 <- c("a", "b", "c")
#' vector2 <- c("e", "f", "g", "h")
#'
#' list1 <- vec_to_list(vector1)
#' list2 <- vec_to_list(vector2)
#'
#' nestedList <- build_nested_list(list1,
#'                                 list2,
#'                                 pasteSep = "__")
#'
#' nestedListPrefix <- prefix_all(nestedList, "example", pasteSep = "__")
#'
#' @export
prefix_all <- function(nestedList, prefix, pasteSep){
  names(nestedList) <- paste0(prefix, pasteSep, names(nestedList))
  lapply(nestedList, function(x, pasteSep = pasteSep, prefix = prefix){
    if (is.list(x)) {
      prefix_all(x, prefix = prefix, pasteSep = pasteSep)
    } else {
      paste0(prefix, pasteSep, x)
    }
  }, pasteSep = pasteSep, prefix = prefix)
}
