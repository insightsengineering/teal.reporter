# Constructor
testthat::test_that("PictureBlock object can be created", {
  testthat::expect_error(PictureBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type PictureBlock", {
  testthat::expect_true(inherits(PictureBlock$new(), "PictureBlock"))
})

# set_content
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

# get_content
testthat::test_that("get_content returns character(0) on a newly initialized PictureBlock", {
  testthat::expect_equal(PictureBlock$new()$get_content(), character(0))
})

# set_title
testthat::test_that("set_title accepts a string", {
  testthat::expect_error(PictureBlock$new()$set_title("Test"), NA)
})


testthat::test_that("set_title asserts the argument is string", {
  testthat::expect_error(PictureBlock$new()$set_title(8), regexp = "Must be of type 'string'")
})

testthat::test_that("set_title returns self", {
  picture_block <- PictureBlock$new()
  testthat::expect_identical(picture_block$set_title("test"), picture_block)
})

# get_title
testthat::test_that("get_title returns the set title", {
  picture_block <- PictureBlock$new()
  picture_block$set_title("test")
  testthat::expect_equal(picture_block$get_title(), "test")

  picture_block$set_title("New title")
  testthat::expect_equal(picture_block$get_title(), "New title")
})

# set_dim
testthat::test_that("set_dim accepts an array of two numeric values", {
  testthat::expect_error(PictureBlock$new()$set_dim(c(0, 0)), regexp = NA)
})

testthat::test_that("set_dim asserts the argument is an array of two numeric values", {
  testthat::expect_error(PictureBlock$new()$set_dim("test"), regexp = "Must be of type 'numeric'")
  testthat::expect_error(PictureBlock$new()$set_dim(c(8, 1, 1)), regexp = "Must have length 2")
})

testthat::test_that("set_dim returns self", {
  picture_block <- PictureBlock$new()
  testthat::expect_identical(picture_block$set_dim(c(0, 0)), picture_block)
})

# set_content
testthat::test_that("set_content throws if the content is not of the supported type", {
  testthat::expect_error(
    PictureBlock$new()$set_content("unsupported content"),
    regexp = "Must inherit from class 'ggplot'/'grob'/'trellis'"
  )
})

testthat::test_that("set_content accepts a `ggplot` object", {
  testthat::expect_error(PictureBlock$new()$set_content(ggplot2::ggplot(iris)), regexp = NA)
})

testthat::test_that("set_content accepts a `grob` object", {
  testthat::expect_error(PictureBlock$new()$set_content(ggplot2::ggplotGrob(ggplot2::ggplot(iris))), regexp = NA)
})

testthat::test_that("set_content accepts a `trellis` object", {
  testthat::expect_error(PictureBlock$new()$set_content(lattice::bwplot(1)), regexp = NA)
})

# to_list
testthat::test_that("to_list returns a named list with a one field, a proper path", {
  pblock <- PictureBlock$new()$set_content(ggplot2::ggplot(iris))
  temp_dir <- tempdir()
  expect_identical(pblock$to_list(temp_dir), list(basename = basename(pblock$get_content())))
})

# from_list
testthat::test_that("from_list after to_list to save and retrive", {
  pblock <- PictureBlock$new()$set_content(ggplot2::ggplot(iris))
  temp_dir <- tempdir()
  expect_identical(file.size(PictureBlock$new()$from_list(pblock$to_list(temp_dir), dirname(pblock$get_content()))$get_content()),
                   file.size(pblock$get_content()))
})
