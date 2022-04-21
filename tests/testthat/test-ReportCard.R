testthat::test_that("ReportCard object can be created", {
  testthat::expect_error(ReportCard$new(), regexp = NA)
})

testthat::test_that("new returns an object of type ReportCard", {
  testthat::expect_true(inherits(ReportCard$new(), "ReportCard"))
})

testthat::test_that("append_text accepts a character", {
  testthat::expect_error(ReportCard$new()$append_text("test"), regexp = NA)
})

testthat::test_that("append_text returns self", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_text("test"), card)
})

testthat::test_that("append_table accepts a data.frame", {
  testthat::expect_error(ReportCard$new()$append_table(iris), regexp = NA)
})

testthat::test_that("append_table returns self", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_table(iris), card)
})

testthat::test_that("append_plot returns self", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_plot(ggplot2::ggplot(iris)), card)
})

testthat::test_that("append_plot accepts a ggplot", {
  testthat::expect_error(ReportCard$new()$append_plot(ggplot2::ggplot(iris)), regexp = NA)
})

testthat::test_that("get_content returns a list of ContentBlock objects", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_table(iris)
  testthat::expect_true(checkmate::test_list(card$get_content(), types = "ContentBlock"))
})

testthat::test_that("append_meta_data returns an object of type ReportCard", {
  card <- ReportCard$new()
  testthat::expect_true(inherits(ReportCard$new()$append_meta_data("key1", "value1"), "ReportCard"))
})

testthat::test_that("append_meta_data accepts a key and a value", {
  testthat::expect_error(ReportCard$new()$append_meta_data("key1", "value1"), regexp = NA)
})

testthat::test_that("append_meta_data throws error if value if missing", {
  testthat::expect_error(
    ReportCard$new()$append_meta_data(key = "key1"),
    regexp = "argument \"value\" is missing, with no default")
})

testthat::test_that("append_meta_data throws error if key if missing", {
  testthat::expect_error(
    ReportCard$new()$append_meta_data(value = "value"),
    regexp = "missing subscript")
})

testthat::test_that("get_meta_data renders a named list in meta_data", {
  card <- ReportCard$new()$append_meta_data("key1", "value1")
  expect_true(length(names(card$get_meta_data())) > 0)
})

testthat::test_that("The deep copy constructor copies the file in the content blocks", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_table(iris)
  card_copy <- card$clone(deep = TRUE)
  original_filepath <- card$get_content()[[2]]$get_content()
  copied_filepath <- card_copy$get_content()[[2]]$get_content()
  testthat::expect_true(original_filepath != copied_filepath)
})
