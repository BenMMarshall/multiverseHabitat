testthat::test_that("vec_to_list object is list", {
  testthat::expect_type(vec_to_list(c("a", "b", "c")), "list")
})
testthat::test_that("vec_to_list object within matches", {
  testthat::expect_equal(vec_to_list(c("a", "b", "c"))[[3]],
                         "c")
})
testthat::test_that("vec_to_list names of slot matches", {
  testthat::expect_equal(names(vec_to_list(c("a", "b", "c")))[3],
                         "c")
})
