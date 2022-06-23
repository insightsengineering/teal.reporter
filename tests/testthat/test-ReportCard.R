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
  card <- ReportCard$new()$append_text("test")$append_plot(ggplot2::ggplot(iris))$append_metadata("SRC", "A <- plot()")
  testthat::expect_true(checkmate::test_list(card$get_content(), types = "ContentBlock"))
})

testthat::test_that("get_metadata returns a list of mixed objects", {
  card <- ReportCard$new()$append_metadata("sth", "test")$append_metadata("sth2", ggplot2::ggplot(iris))
  testthat::expect_false(checkmate::test_list(card$get_metadata(), types = "ContentBlock"))
})

testthat::test_that("get_metadata returns a named list", {
  card <- ReportCard$new()$append_metadata("sth", "test")$append_metadata("sth2", ggplot2::ggplot(iris))
  testthat::expect_equal(c("sth", "sth2"), names(card$get_metadata()))
})

testthat::test_that("append_metadata returns an object of type ReportCard", {
  card <- ReportCard$new()
  testthat::expect_identical(card$append_metadata("key1", "value1"), card)
})

testthat::test_that("append_metadata accepts a character key and a character or list value", {
  testthat::expect_error(ReportCard$new()$append_metadata("key1", "value1"), regexp = NA)
  testthat::expect_error(ReportCard$new()$append_metadata("key1", list("value1")), regexp = NA)
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
  testthat::expect_error(
    ReportCard$new()$append_metadata(key = "key", value = "value")$append_metadata(key = "key", value = "value")
  )
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
  testthat::expect_equal(card_copy$get_metadata()[[1]], card$get_metadata()[[1]])
})

testthat::test_that("setting and getting a name to the ReportCard", {
  testthat::expect_identical(ReportCard$new()$set_name("NAME")$get_name(), "NAME")
  testthat::expect_identical(ReportCard$new()$get_name(), character(0))
})

card <- teal.reporter::ReportCard$new()

card$append_text("Header 2 text", "header2")
card$append_text("A paragraph of default text", "header2")
card$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

picture_filename <- basename(card$get_content()[[3]]$get_content())
temp_dir <- file.path(tempdir(), "test")
dir.create(temp_dir)

testthat::test_that("to_list internally triggers to_list on each Block", {
  testthat::expect_identical(
    card$to_list(temp_dir),
    list(blocks = list(
      TextBlock = list(text = "Header 2 text", style = "header2"),
      TextBlock = list(text = "A paragraph of default text", style = "header2"),
      PictureBlock = list(basename = picture_filename)
    ), metadata = list())
  )
  testthat::expect_true(picture_filename %in% list.files(temp_dir))
})

testthat::test_that("from_list", {
  cardf <- ReportCard$new()$from_list(
    list(blocks = list(
      TextBlock = list(text = "Header 2 text", style = "header2"),
      TextBlock = list(text = "A paragraph of default text", style = "header2"),
      PictureBlock = list(basename = picture_filename)
    ), metadata = list()),
    temp_dir
  )
  testthat::expect_true(inherits(cardf, "ReportCard"))
  testthat::expect_length(cardf$get_content(), 3L)
})

unlink(temp_dir, recursive = TRUE)
