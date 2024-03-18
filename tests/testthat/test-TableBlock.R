testthat::test_that("TableBlock object can be created", {
  testthat::expect_no_error(TableBlock$new())
})

testthat::test_that("new returns an object of type TableBlock", {
  testthat::expect_true(inherits(TableBlock$new(), "TableBlock"))
})

testthat::test_that("set_content accepts a table object", {
  block <- TableBlock$new()
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_no_error(block$set_content(iris))
  )
})

testthat::test_that("set_content asserts the argument is a plot", {
  block <- TableBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must inherit from class 'data.frame'/'rtables'")
})

testthat::test_that("set_content returns the TableBlock object", {
  block <- TableBlock$new()
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_identical(block$set_content(iris), block)
  )
})

testthat::test_that("get_content returns character(0) on a newly initialized TableBlock", {
  testthat::expect_equal(TableBlock$new()$get_content(), character(0))
})

temp_dir <- tempdir()

testthat::test_that("to_list returns a named list with a one field, a proper file name", {
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    block <- TableBlock$new()$set_content(iris)
  )
  testthat::expect_equal(block$to_list(temp_dir), list(basename = basename(block$get_content())))
})

# to_list
testthat::test_that("to_list returns a named list with a one field, a proper path", {
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    tblock <- TableBlock$new()$set_content(iris)
  )
  testthat::expect_identical(tblock$to_list(temp_dir), list(basename = basename(tblock$get_content())))
})

# from_list
testthat::test_that("from_list after to_list to save and retrive", {
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    tblock <- TableBlock$new()$set_content(iris)
  )
  testthat::expect_identical(
    file.size(TableBlock$new()$from_list(
      tblock$to_list(temp_dir),
      dirname(tblock$get_content())
    )$get_content()),
    file.size(tblock$get_content())
  )
})

testthat::test_that("set_content supports data.frame object", {
  block <- TableBlock$new()
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_no_error(block$set_content(iris))
  )
})

testthat::test_that("set_content supports rtables object", {
  block <- TableBlock$new()
  l <- rtables::basic_table() %>%
    rtables::split_cols_by("Species") %>%
    rtables::analyze("Sepal.Length", afun = function(x) {
      list(
        "mean (sd)" = rtables::rcell(c(mean(x), sd(x)), format = "xx.xx (xx.xx)"),
        "range" = diff(range(x))
      )
    })
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_no_error(block$set_content(rtables::build_table(l, iris)))
  )
})
