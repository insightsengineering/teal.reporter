testthat::test_that("PictureBlock object can be created", {
  testthat::expect_error(PictureBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type PictureBlock", {
  testthat::expect_true(inherits(PictureBlock$new(), "PictureBlock"))
})

testthat::test_that("set_content accepts a plot object", {
  block <- PictureBlock$new()
  testthat::expect_error(block$set_content(ggplot2::ggplot(iris)), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a plot", {
  block <- PictureBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must inherit from class 'ggplot")
})

testthat::test_that("set_content returns the PictureBlock object", {
  block <- PictureBlock$new()
  testthat::expect_identical(block$set_content(ggplot2::ggplot(iris)), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized PictureBlock", {
  testthat::expect_equal(PictureBlock$new()$get_content(), character(0))
})
