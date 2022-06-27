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

testthat::test_that("set_content supports data.frame object", {
  block <- TableBlock$new()
  testthat::expect_error(block$set_content(iris), NA)
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
  testthat::expect_error(block$set_content(rtables::build_table(l, iris)), NA)
})
