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

testthat::test_that("get_content accepts logical raw argument", {
  card <- ReportCard$new()$append_text("test")
  testthat::expect_error(card$get_content(raw = FALSE), regexp = NA)
  testthat::expect_error(card$get_content(raw = TRUE), regexp = NA)
})

testthat::test_that("get_content throws error when raw argument is not logical", {
  card <- ReportCard$new()$append_text("test")
  testthat::expect_error(card$get_content(raw = "FALSE"), regexp = "Assertion on 'raw' failed")
  testthat::expect_error(card$get_content(raw = 1), regexp = "Assertion on 'raw' failed")
})

testthat::test_that("get_content returns a list of ContentBlock objects when raw = FALSE", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_true(checkmate::test_list(card$get_content(), types = "ContentBlock"))
})

testthat::test_that("get_content returns a list of mixed objects when raw = TRUE", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_false(checkmate::test_list(card$get_content(raw = TRUE), types = "ContentBlock"))
})

testthat::test_that("get_content returns a named list when raw = TRUE", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_equal(c("", "", "SRC"), names(card$get_content(raw = TRUE)))
})

testthat::test_that("append_metadata returns an object of type ReportCard", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_metadata("key1", "value1"), card)
})

testthat::test_that("append_metadata accepts a character key and a character or list value", {
  testthat::expect_error(ReportCard$new()$append_metadata("key1", "value1"), regexp = NA)
  testthat::expect_error(ReportCard$new()$append_metadata("key1", list("value1")), regexp = NA)
})

testthat::test_that("append_metadata accepts a function deparse argument", {
  testthat::expect_error(ReportCard$new()$append_metadata("key1", "value1", deparse = deparse), regexp = NA)
})

testthat::test_that("append_metadata throws error when deparse argument is not a function", {
  testthat::expect_error(
    ReportCard$new()$append_metadata("key1", "value1", deparse = "deparse"),
    regexp = "Assertion on 'deparse' failed: Must be a function, not 'character'."
  )
})

testthat::test_that("append_metadata throws error if value is not character", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = 1, value = "value1"),
    regexp = "Must be of type 'character', not 'double'."
  )
})

testthat::test_that("append_metadata throws error if key is not character", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = 1, value = "value1"),
    regexp = "Must be of type 'character', not 'double'."
  )
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = factor("A"), value = "value1"),
    regexp = "Must be of type 'character', not 'factor'."
  )
})

testthat::test_that("append_metadata throws error if deparse is not a function", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = 1, value = "value1"),
    regexp = "Must be of type 'character', not 'double'."
  )
})
testthat::test_that("append_metadata throws error if value if missing", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = "key1"),
    regexp = "argument \"value\" is missing, with no default"
  )
})

testthat::test_that("append_metadata throws error if key if missing", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(value = "value"),
    regexp = "argument \"key\" is missing, with no default"
  )
})

testthat::test_that("get_deparsers returns an empty list when append_metadata is not used prior", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))#$append_metadata("SRC", "A <- plot()")
  testthat::expect_true(length(card$get_deparsers()) == 0)
})

testthat::test_that("get_deparsers returns a character list when append_metadata is used prior", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_true(length(card$get_deparsers()) != 0)
})

testthat::test_that("The deep copy constructor copies the file in the content blocks", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot(1)")
  card_copy <- card$clone(deep = TRUE)
  original_filepath <- card$get_content()[[2]]$get_content()
  copied_filepath <- card_copy$get_content()[[2]]$get_content()
  testthat::expect_true(original_filepath != copied_filepath)
})

testthat::test_that("The deep copy constructor copies the non ContentBlock objects", {
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot(1)")
  card_copy <- card$clone(deep = TRUE)
  testthat::expect_equal(card_copy$get_content()[[1]], card$get_content()[[1]])
  testthat::expect_equal(card_copy$get_content()[[3]], card$get_content()[[3]])
})
