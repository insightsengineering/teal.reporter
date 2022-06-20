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

testthat::test_that("to_list", {
  block <- TableBlock$new()$set_content(iris)
  testthat::expect_identical(block$to_list(NULL), list(path = basename(block$get_content())))
})

testthat::test_that("to_list", {
  block <- TableBlock$new()$set_content(iris)
  testthat::expect_identical(block$to_list(dirname(block$get_content())),
                             list(path = block$get_content()))
})

testthat::test_that("from_list", {
  block <- TableBlock$new()$set_content(iris)
  testthat::expect_equal(block,
                         TableBlock$new()$from_list(list(path = block$get_content()), dirname(block$get_content())))

})
