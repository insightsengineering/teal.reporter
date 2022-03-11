testthat::test_that("ContentBlock object can be created", {
  testthat::expect_error(ContentBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type ContentBlock", {
  testthat::expect_true(inherits(ContentBlock$new(), "ContentBlock"))
})

testthat::test_that("set_content accepts a character object", {
  block <- ContentBlock$new()
  testthat::expect_error(block$set_content("test"), regexp = NA)
})

testthat::test_that("set_content asserts the argument is character", {
  block <- ContentBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must be of type 'character'")
})

testthat::test_that("set_content returns the ContentBlock object", {
  block <- ContentBlock$new()
  testthat::expect_identical(block$set_content("test"), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized ContentBlock", {
  testthat::expect_equal(ContentBlock$new()$get_content(), character(0))
})
