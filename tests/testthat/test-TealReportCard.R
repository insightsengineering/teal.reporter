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

testthat::test_that("TealReportCard$get_content returns a list with content only when include_metadata = FALSE", {
  card <- TealReportCard$new()$append_text("test")$append_src("a <- plot()")
  testthat::expect_true(checkmate::test_list(card$get_content(include_metadata = FALSE), types = "ContentBlock"))
})

testthat::test_that("TealReportCard$get_content returns a list with content
                    and metadata only when include_metadata = TRUE", {
  card <- TealReportCard$new()$append_text("test")$append_src("a <- plot()")
  testthat::expect_true(checkmate::test_list(card$get_content(include_metadata = TRUE), types = "ContentBlock"))
})

testthat::test_that("TealReportCard$get_content returns content with
                    metadata header as a ContentBlock before the metadata ContentBlock", {
  card <- TealReportCard$new()$append_text("test")$append_src("test_src")$append_encodings(list("data = test"))
  testthat::expect_equal(length(card$get_content(include_metadata = TRUE)), 5)
  testthat::expect_identical(card$get_content(include_metadata = TRUE)[[2]]$get_content(), "SRC")
  testthat::expect_identical(card$get_content(include_metadata = TRUE)[[4]]$get_content(), "Encodings")
})

testthat::test_that("get_content throws error when include_metadata is not logical", {
  card <- TealReportCard$new()$append_text("test")$append_src("test_src")$append_encodings(list("data = test"))
  testthat::expect_error(
    card$get_content(include_metadata = "TRUE"),
    "Assertion on 'include_metadata' failed: Must be of type 'logical', not 'character'."
  )
})

testthat::test_that("TealReportCard$append_src accepts a character", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_src("test"), regexp = NA)
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

testthat::test_that("TealReportCard$append_fs accepts a list of character", {
  card <- TealReportCard$new()
  testthat::expect_error(card$append_fs(list("test")), regexp = NA)
})

testthat::test_that("TealReportCard$append_fs returns self", {
  card <- TealReportCard$new()
  testthat::expect_identical(card$append_fs(list("test_fs")), card)
})
