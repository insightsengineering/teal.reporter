testthat::test_that("TableBlock object can be created", {
  testthat::expect_error(TableBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type TableBlock", {
  testthat::expect_true(inherits(TableBlock$new(), "TableBlock"))
})

testthat::test_that("set_content accepts a table object", {
  block <- TableBlock$new()
  testthat::expect_error(block$set_content(iris), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a plot", {
  block <- TableBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must inherit from class 'data.frame'/'rtables'")
})

testthat::test_that("set_content returns the TableBlock object", {
  block <- TableBlock$new()
  testthat::expect_identical(block$set_content(iris), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized TableBlock", {
  testthat::expect_equal(TableBlock$new()$get_content(), character(0))
})

testthat::test_that("to_list returns a named list with a one field, a proper path", {
  block <- TableBlock$new()$set_content(iris)
  temp_dir <- tempdir()
  testthat::expect_equal(block$to_list(temp_dir), list(basename = basename(block$get_content())))
})

testthat::test_that("to_list with base_path arg", {
  block <- TableBlock$new()$set_content(iris)
  temp_dir <- tempdir()
  testthat::expect_identical(
    block$to_list(temp_dir),
    list(basename = basename(block$get_content()))
  )
  testthat::expect_true(file.exists(file.path(temp_dir, basename(block$get_content()))))
})

testthat::test_that("from_list returns the same object as set_content", {
  block <- TableBlock$new()$set_content(iris)
  temp_dir <- tempdir()
  testthat::expect_equal(
    file.size(block$get_content()),
    file.size(TableBlock$new()$from_list(list(basename = basename(block$get_content())),
                               temp_dir)$get_content())
  )
  testthat::expect_true(file.exists(file.path(temp_dir, basename(block$get_content()))))
})
