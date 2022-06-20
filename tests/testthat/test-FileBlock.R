testthat::test_that("FileBlock object can be created", {
  testthat::expect_error(FileBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type FileBlock", {
  testthat::expect_true(inherits(FileBlock$new(), "FileBlock"))
})

testthat::test_that("to_list returns a named list with a one field", {
  block <- FileBlock$new()
  testthat::expect_equal(block$to_list(), list(path = character(0)))
})

testthat::test_that("to_list with base_path arg", {
  block <- TableBlock$new()
  testthat::expect_identical(
    block$to_list(dirname(block$get_content())),
    list(path = character(0))
  )
})
