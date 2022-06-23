testthat::test_that("TableBlock object can be created", {
  testthat::expect_error(teal.reporter::TableBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type TableBlock", {
  testthat::expect_true(inherits(teal.reporter::TableBlock$new(), "TableBlock"))
})

testthat::test_that("set_content accepts a table object", {
  block <- teal.reporter::TableBlock$new()
  testthat::expect_error(block$set_content(iris), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a plot", {
  block <- teal.reporter::TableBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must inherit from class 'data.frame'/'rtables'")
})

testthat::test_that("set_content returns the TableBlock object", {
  block <- teal.reporter::TableBlock$new()
  testthat::expect_identical(block$set_content(iris), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized TableBlock", {
  testthat::expect_equal(teal.reporter::TableBlock$new()$get_content(), character(0))
})

temp_dir <- tempdir()

testthat::test_that("to_list returns a named list with a one field, a proper file name", {
  block <- TableBlock$new()$set_content(iris)
  testthat::expect_equal(block$to_list(temp_dir), list(basename = basename(block$get_content())))
})

# to_list
testthat::test_that("to_list returns a named list with a one field, a proper path", {
  tblock <- teal.reporter::TableBlock$new()$set_content(iris)
  testthat::expect_identical(tblock$to_list(temp_dir), list(basename = basename(tblock$get_content())))
})

# from_list
testthat::test_that("from_list after to_list to save and retrive", {
  tblock <- teal.reporter::TableBlock$new()$set_content(iris)
  testthat::expect_identical(
    file.size(teal.reporter::TableBlock$new()$from_list(
      tblock$to_list(temp_dir),
      dirname(tblock$get_content())
    )$get_content()),
    file.size(tblock$get_content())
  )
})
