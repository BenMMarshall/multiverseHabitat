#' Nest one list within another
#'
#' @name nest_list
#' @description Takes two named lists and nests the places the second within
#'   every slot of the first list, while making the list names match the path
#'   from the top list.
#' @param parentList The list into which the second list will be nested within.
#' @param toNest The list that will be repeatedly added to every slot of the top
#'   nest.
#' @param pasteSep The separator to use between level names during paste
#'   process. Default is "__".
#' @param parentNames The names from the top list that will be pasted onto the
#'   lower list names to mirror the path to the lower list slots.
#' @return A named list, with the loweest level being replaced with the path.
#' @details This function is only provided to aid with tree generation using the
#'   build_nested_list function.
#' @seealso build_nested_list
#'
#' @examples
#'
#' vector1 <- c("a", "b", "c")
#' vector2 <- c("e", "f", "g", "h")
#'
#' list1 <- vec_to_list(vector1)
#' list2 <- vec_to_list(vector2)
#'
#' nest_list(parentList = list1, toNest = list2, pasteSep = "_")
#'
#' @export
nest_list <- function(parentList, toNest, pasteSep = "__",
                      parentNames = names(parentList)){
  tempNest <- lapply(seq_along(parentList),
                     function(x, toNest, parentNames, pasteSep){
                       names(toNest) <- paste0(parentNames[x], pasteSep, names(toNest))

                       lapply(toNest, function(y){
                         paste0(parentNames[x], pasteSep, y)
                       })

                     }, toNest, pasteSep, parentNames = names(parentList))
  names(tempNest) <- parentNames
  return(tempNest)
}

# nest_list <- function(parentList, toNest, pasteSep = "__",
#                       parentNames = names(parentList)){
#   tempNest <- lapply(seq_along(parentList),
#                      function(x, toNest, parentNames, pasteSep){
#                        names(toNest) <- paste0(parentNames[x], pasteSep, names(toNest))
#                        toNest
#                      }, toNest, pasteSep, parentNames = names(parentList))
#   names(tempNest) <- parentNames
#   return(tempNest)
# }
