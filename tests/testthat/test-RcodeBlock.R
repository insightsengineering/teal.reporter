testthat::test_that("RcodeBlock object can be created", {
  testthat::expect_error(RcodeBlock$new(), regexp = NA)
})

testthat::test_that("new returns an object of type RcodeBlock", {
  testthat::expect_true(inherits(RcodeBlock$new(), "RcodeBlock"))
})

testthat::test_that("set_content accepts a string", {
  block <- RcodeBlock$new()
  testthat::expect_error(block$set_content("test"), regexp = NA)
})

testthat::test_that("set_content asserts the argument is a string", {
  block <- RcodeBlock$new()
  testthat::expect_error(block$set_content(7), regexp = "Must be of type 'character'")
})

testthat::test_that("set_content returns the RcodeBlock object", {
  block <- RcodeBlock$new()
  testthat::expect_identical(block$set_content("test"), block)
})

testthat::test_that("get_content returns character(0) on a newly initialized RcodeBlock", {
  testthat::expect_equal(RcodeBlock$new()$get_content(), character(0))
})

testthat::test_that("get_content returns previously set string", {
  testthat::expect_equal(RcodeBlock$new()$set_content("test")$get_content(), "test")
})

testthat::test_that("get_available_params returns an array of character", {
  testthat::expect_true(checkmate::test_character(RcodeBlock$new()$get_available_params(), any.missing = FALSE))
})

testthat::test_that("set_params accepts one of the styles returned by get_available_params", {
  for (param in RcodeBlock$new()$get_available_params()) {
    input <- list()
    input[[param]] <- NULL
    testthat::expect_error(RcodeBlock$new()$set_params(input), regexp = NA)
  }
})

testthat::test_that("set_params returns the RcodeBlock object", {
  block <- RcodeBlock$new()
  input <- list()
  input[[block$get_available_params()[1]]] <- NULL
  testthat::expect_identical(block$set_params(input), block)
})

testthat::test_that("to_list returns a two field named list", {
  testthat::expect_identical(
    RcodeBlock$new()$set_content("test")$to_list(),
    list(text = "test", params = list())
  )
})

testthat::test_that("from_list returns a similar output to set_content", {
  testthat::expect_equal(
    RcodeBlock$new()$from_list(list(text = "test", params = list(echo = TRUE))),
    RcodeBlock$new()$set_content("test")$set_params(list(echo = TRUE))
  )
})

testthat::test_that("from_list after to_list to save and retrive", {
  testthat::expect_equal(
    RcodeBlock$new()$from_list(RcodeBlock$new()$set_content("test")$to_list()),
    RcodeBlock$new()$set_content("test")
  )
})
