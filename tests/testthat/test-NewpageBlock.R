testthat::test_that("NewpageBlock object can be created", {
  testthat::expect_error(NewpageBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type NewpageBlock", {
  testthat::expect_true(inherits(NewpageBlock$new(), "NewpageBlock"))
})

testthat::test_that("set_content accepts a string", {
  block <- NewpageBlock$new()
  testthat::expect_error(block$get_content(), regexp = NA)
})
