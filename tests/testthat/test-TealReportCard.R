testthat::test_that("TealReportCard object can be initialized", {
  testthat::expect_error(TealReportCard$new(), regexp = NA)
})

testthat::test_that("TealReportCard inherits from ReportCard", {
  testthat::expect_true(inherits(TealReportCard$new(), "ReportCard"))
})

testthat::test_that("TealReportCard$new returns an object of type TealReportCard", {
  testthat::expect_true(inherits(TealReportCard$new(), "TealReportCard"))
})

testthat::test_that("TealReportCard$get_content returns a list of ContentBlock objects", {
  card <- TealReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_table(iris)
  testthat::expect_true(checkmate::test_list(card$get_content(), types = "ContentBlock"))
})

testthat::test_that("TealReportCard$get_content returns content with metadata", {
  card <- TealReportCard$new()$append_text("test")$append_src("test_src")$append_encodings(list(data = "test"))
  testthat::expect_equal(length(card$get_content()), 3)
  testthat::expect_equal(length(card$get_metadata()), 2)
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "test")
  testthat::expect_identical(
    card$get_content()[[2]]$get_content(),
    "test_src"
  )
  testthat::expect_identical(card$get_content()[[3]]$get_content(), "data: test\n")
})

testthat::test_that("TealReportCard$append_src accepts a character", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_src("test"), regexp = NA)
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_src("test")$get_content()[[1]]$get_content(), "test")
})

testthat::test_that("TealReportCard$append_src returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_src("test"), card)
})

testthat::test_that("TealReportCard$append_encodings accepts list of character", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_encodings(list("test")), regexp = NA)
})

testthat::test_that("TealReportCard$append_encodings returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_encodings(list("test_encodings")), card)
})

fs <- R6::R6Class("FilteredData",
  public = list(
    get_filter_state = function() list(a = 1, b = 3),
    get_formatted_filter_state = function() "TEST"
  )
)
fs_inst <- fs$new()

testthat::test_that("TealReportCard$append_fs accepts only a FilteredData", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_fs(list(a = 1)),
    regexp = "Must inherit from class 'FilteredData'"
  )
  testthat::expect_error(card$append_fs(fs_inst), regexp = NA)
})

testthat::test_that("TealReportCard$append_fs returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_fs(fs_inst), card)
})
