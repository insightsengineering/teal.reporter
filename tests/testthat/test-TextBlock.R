testthat::test_that("TextBlock object can be created", {
  testthat::expect_error(TextBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type TextBlock", {
  testthat::expect_true(inherits(TextBlock$new(), "TextBlock"))
})

testthat::test_that("set_content accepts a string", {
  block <- TextBlock$new()
  testthat::expect_error(block$set_content("test"), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a string", {
  block <- TextBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must be of type 'character'")
})

testthat::test_that("set_content returns the TextBlock object", {
  block <- TextBlock$new()
  testthat::expect_identical(block$set_content("test"), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized TextBlock", {
  testthat::expect_equal(TextBlock$new()$get_content(), character(0))
})

testthat::test_that("get_content returns previously set string", {
  testthat::expect_equal(TextBlock$new()$set_content("test")$get_content(), "test")
})

testthat::test_that("get_available_styles returns an array of character", {
  testthat::expect_true(checkmate::test_character(TextBlock$new()$get_available_styles(), any.missing = FALSE))
})

testthat::test_that("set_style accepts one of the styles returned by get_available_styles", {
  for (style in TextBlock$new()$get_available_styles()) {
    testthat::expect_error(TextBlock$new()$set_style(!!style), regexp = NA)
  }
})

testthat::test_that("set_style asserts the argument is one of styles in get_available_styles", {
  testthat::expect_error(
    TextBlock$new()$set_style("test"),
    regexp = "'arg' should be one"
  )
})

testthat::test_that("set_style returns the TextBlock object", {
  block <- TextBlock$new()
  testthat::expect_identical(block$set_style(block$get_available_styles()[1]), block)
})

testthat::test_that("get_style returns the set style", {
  testthat::expect_equal(
    TextBlock$new()$set_style(TextBlock$new()$get_available_styles()[1])$get_style(),
    TextBlock$new()$get_available_styles()[1]
  )
})
