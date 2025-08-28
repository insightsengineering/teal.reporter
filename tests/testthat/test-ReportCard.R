testthat::test_that("ReportCard object can be created", {
  testthat::expect_no_error(ReportCard$new())
})

testthat::test_that("new returns an object of type ReportCard", {
  testthat::expect_true(inherits(ReportCard$new(), "ReportCard"))
})

testthat::test_that("append_text accepts a character", {
  testthat::expect_no_error(ReportCard$new()$append_text("test"))
})

testthat::test_that("append_text returns self", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_text("test"), card)
})

testthat::test_that("append_table accepts a data.frame", {
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_no_error(
      ReportCard$new()$append_table(iris)
    )
  )
})

testthat::test_that("append_table returns self", {
  card <- ReportCard$new()
  # https://github.com/davidgohel/flextable/issues/600
  withr::with_options(
    opts_partial_match_old,
    testthat::expect_identical(card$append_table(iris), card)
  )
})

testthat::test_that("append_plot returns self", {
  testthat::skip_if_not_installed("ggplot2")

  card <- ReportCard$new()
  testthat::expect_identical(card$append_plot(ggplot2::ggplot(iris)), card)
})

testthat::test_that("append_plot accepts a ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::expect_no_error(
    ReportCard$new()$append_plot(ggplot2::ggplot(iris))
  )
})

testthat::test_that("append_plot accepts a ggplot with a dim", {
  testthat::skip_if_not_installed("ggplot2")

  testthat::expect_no_error(
    ReportCard$new()$append_plot(ggplot2::ggplot(iris), c(1000L, 100L))
  )
})

testthat::test_that("append_rcode accepts a character", {
  testthat::expect_no_error(
    ReportCard$new()$append_rcode("x <- 2")
  )
})

testthat::test_that("append_rcode returns self", {
  testthat::expect_no_error(
    ReportCard$new()$append_rcode("x <- 2")
  )
})

testthat::test_that("get_content returns a list of objects", {
  testthat::skip_if_not_installed("ggplot2")

  card <- ReportCard$new()
  card$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_s3_class(card$get_content(), "teal_card")
  checkmate::expect_list(card$get_content())
})

testthat::test_that("get_metadata returns a list of mixed objects", {
  testthat::skip_if_not_installed("ggplot2")

  card <- ReportCard$new()
  card$append_metadata("sth", "test")$append_metadata("sth2", ggplot2::ggplot(iris))
  testthat::expect_failure(testthat::expect_s3_class(card$get_metadata(), "teal_card"))
})

testthat::test_that("get_metadata returns a named list", {
  testthat::skip_if_not_installed("ggplot2")

  card <- ReportCard$new()
  card$append_metadata("sth", "test")$append_metadata("sth2", ggplot2::ggplot(iris))
  testthat::expect_equal(c("sth", "sth2"), names(card$get_metadata()))
})

testthat::test_that("append_metadata returns an object of type ReportCard", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_metadata("key1", "value1"), card)
})

testthat::test_that("append_metadata accepts a character key and a character or list value", {
  testthat::expect_no_error(ReportCard$new()$append_metadata("key1", "value1"))
  testthat::expect_no_error(ReportCard$new()$append_metadata("key1", list("value1")))
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

testthat::test_that("append_metadata throws error if value if missing", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = "key1"),
    regexp = "argument \"value\" is missing, with no default"
  )
})

testthat::test_that("append_metadata throws error if key is missing", {
  testthat::expect_error(
    ReportCard$new()$append_metadata(value = "value"),
    regexp = "argument \"key\" is missing, with no default"
  )
})

testthat::test_that("append_metadata throws error if keys are duplicated", {
  card <- ReportCard$new()
  testthat::expect_error(
    card$append_metadata(key = "key", value = "value")$append_metadata(key = "key", value = "value")
  )
})

testthat::test_that("The deep copy constructor copies the plot object", {
  testthat::skip_if_not_installed("ggplot2")
  card <- ReportCard$new()
  card$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot(1)")
  card_copy <- card$clone(deep = TRUE)
  testthat::expect_identical(card$get_content()[[2]], card_copy$get_content()[[2]])
})

testthat::test_that("The deep copy constructor copies the objects", {
  testthat::skip_if_not_installed("ggplot2")

  card <- ReportCard$new()
  card$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot(1)")
  card_copy <- card$clone(deep = TRUE)
  testthat::expect_equal(card_copy$get_content()[[1]], card$get_content()[[1]])
  testthat::expect_equal(card_copy$get_metadata()[[1]], card$get_metadata()[[1]])
})

testthat::test_that("setting and getting a name to the ReportCard", {
  testthat::expect_identical(
    ReportCard$new()$set_name("NAME")$get_name(),
    "NAME"
  )
  testthat::expect_identical(
    ReportCard$new()$get_name(),
    character(0)
  )
})
