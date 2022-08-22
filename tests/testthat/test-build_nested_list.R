testthat::test_that("build_nested_list object is list", {
  vector1 <- c("a", "b", "c")
  vector2 <- c("e", "f", "g", "h")
  vector3 <- c("i", "j")
  list1 <- vec_to_list(vector1)
  list2 <- vec_to_list(vector2)
  list3 <- vec_to_list(vector3)
  nestedList <- build_nested_list(list1,
                                  list2,
                                  list3, pasteSep = "__")
  testthat::expect_type(nestedList, "list")
})
testthat::test_that("build_nested_list object within match", {
  vector1 <- c("a", "b", "c")
  vector2 <- c("e", "f", "g", "h")
  vector3 <- c("i", "j")
  list1 <- vec_to_list(vector1)
  list2 <- vec_to_list(vector2)
  list3 <- vec_to_list(vector3)
  nestedList <- build_nested_list(list1,
                                  list2,
                                  list3, pasteSep = "__")
  testthat::expect_equal(nestedList$a$a__e$a__e__i, "i")
})
testthat::test_that("build_nested_list list name match", {
  vector1 <- c("a", "b", "c")
  vector2 <- c("e", "f", "g", "h")
  vector3 <- c("i", "j")
  list1 <- vec_to_list(vector1)
  list2 <- vec_to_list(vector2)
  list3 <- vec_to_list(vector3)
  nestedList <- build_nested_list(list1,
                                  list2,
                                  list3, pasteSep = "__")
  testthat::expect_equal(names(nestedList[[1]][[1]])[1],
                         paste(vector1[1],
                               vector2[1],
                               vector3[1],
                               sep = "__"))
})
testthat::test_that("build_nested_list list sub list length correct", {
  vector1 <- c("a", "b", "c")
  vector2 <- c("e", "f", "g", "h")
  vector3 <- c("i", "j")
  list1 <- vec_to_list(vector1)
  list2 <- vec_to_list(vector2)
  list3 <- vec_to_list(vector3)
  nestedList <- build_nested_list(list1,
                                  list2,
                                  list3, pasteSep = "__")
  testthat::expect_equal(all(lengths(nestedList) == length(vector2)), TRUE)
  testthat::expect_equal(all(lengths(nestedList$a) == length(vector3)), TRUE)
})
