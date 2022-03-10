testthat::test_that("PictureBlock object can be created", {
  testthat::expect_error(PictureBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type PictureBlock", {
  testthat::expect_true(inherits(PictureBlock$new(), "PictureBlock"))
})

testthat::test_that("set_content accepts an exists file", {
  block <- PictureBlock$new()
  img_path <- system.file("img", "Rlogo.png", package = "png")
  testthat::expect_error(block$set_content(img_path), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a string", {
  block <- PictureBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must be of type 'string'")
})

testthat::test_that("set_content asserts the file exists", {
  testthat::expect_error(PictureBlock$new()$set_content("test")$get_content(), "File does not exist: 'test'")
})

testthat::test_that("set_content returns the PictureBlock object", {
  block <- PictureBlock$new()
  img_path <- system.file("img", "Rlogo.png", package = "png")
  testthat::expect_identical(block$set_content(img_path), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized PictureBlock", {
  testthat::expect_equal(PictureBlock$new()$get_content(), character(0))
})
