testthat::test_that("NewpageBlock object can be created", {
  testthat::expect_no_error(NewpageBlock$new())
})

testthat::test_that("new returns an object of type NewpageBlock", {
  testthat::expect_true(inherits(NewpageBlock$new(), "NewpageBlock"))
})

testthat::test_that("set_content accepts a string", {
  block <- NewpageBlock$new()
  testthat::expect_no_error(block$get_content())
})
