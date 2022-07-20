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
  testthat::expect_error(card$append_encodings(list(a = "test")), regexp = NA)
})

testthat::test_that("TealReportCard$append_encodings returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_encodings(list(a = "test_encodings")), card)
})

testthat::test_that("TealReportCard$append_fs accepts only a FilteredData", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_fs(c(a = 1, b = 2)),
    regexp = "Assertion on 'fs' failed: Must be of type 'list', not 'double'."
  )
  testthat::expect_error(card$append_fs(list(a = 1, b = 2)), regexp = NA)
})

testthat::test_that("TealReportCard$append_fs returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_fs(list(a = 1, b = 2)), card)
})

testthat::test_that("TealReportCard$append_fs appends throws error if attribute is not character or NULL", {
  # NULL
  card <- TealReportCard$new()
  testthat::expect_error(card$append_fs(list(a = 1)), NA)

  # character
  card <- TealReportCard$new()
  fs <- list(a = 1, b = 2)
  attr(fs, "formatted") <- "attr is not NULL"
  testthat::expect_error(fs, NA)

  # non-character, non-NULL
  card <- TealReportCard$new()
  fs <- list(a = 1, b = 2)
  attr(fs, "formatted") <- 1
  testthat::expect_error(card$append_fs(fs), "Assertion on 'attr_fs' failed")
})

testthat::test_that("TealReportCard$append_fs appends text according to the attribute of the input list", {
  # attribute is NULL
  card <- TealReportCard$new()
  card$append_fs(list(a = 1))
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "a: 1.0\n")

  # attribute is not NULL
  card <- TealReportCard$new()
  fs <- list(a = 1, b = 2)
  attr(fs, "formatted") <- "attr is not NULL"
  card$append_fs(fs)
  testthat::expect_identical(card$get_content()[[1]]$get_content(), "attr is not NULL")
})
