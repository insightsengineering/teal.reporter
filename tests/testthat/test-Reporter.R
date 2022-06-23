testthat::test_that("Reporter object can be created", {
  testthat::expect_error(Reporter$new(), regexp = NA)
})

testthat::test_that("new returns an object of type Reporter", {
  testthat::expect_true(inherits(Reporter$new(), "Reporter"))
})

card1 <- teal.reporter::ReportCard$new()

card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

card2 <- teal.reporter::ReportCard$new()

card2$append_text("Header 2 text", "header2")
card2$append_text("A paragraph of default text", "header2")
lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
table_res2 <- rtables::build_table(lyt, airquality)
card2$append_table(table_res2)
card2$append_table(iris)

reporter <- Reporter$new()
reporter$append_cards(list(card1, card2))

testthat::test_that("get_cards returns the same cards which was added to reporter", {
  expect_identical(reporter$get_cards(), list(card1, card2))
})

testthat::test_that("get_blocks returns the same blocks which was added to reporter, sep = NULL", {
  expect_identical(reporter$get_blocks(sep = NULL), append(card1$get_content(), card2$get_content()))
})

reporter_blocks <- reporter$get_blocks()
reporter_blocks2 <- append(reporter_blocks[1:3], NewpageBlock$new())
reporter_blocks2 <- append(reporter_blocks2, reporter_blocks[5:8])

testthat::test_that("get_blocks by default adds NewpageBlock$new() between cards", {
  expect_equal(reporter$get_blocks(), reporter_blocks2)
})

reporter2 <- Reporter$new()

testthat::test_that("get_blocks and get_cards return empty list by default", {
  expect_identical(reporter2$get_blocks(), list())
  expect_identical(reporter2$get_cards(), list())
})

testthat::test_that("The deep copy constructor copies the content files to new files", {
  card <- teal.reporter::ReportCard$new()$append_plot(ggplot2::ggplot(iris))
  reporter <- Reporter$new()$append_cards(list(card))
  reporter_copy <- reporter$clone(deep = TRUE)
  original_content_file <- reporter$get_blocks()[[1]]$get_content()
  copied_content_file <- reporter_copy$get_blocks()[[1]]$get_content()

  testthat::expect_false(original_content_file == copied_content_file)
})


testthat::test_that("swap_cards", {
  reporter1a <- reporter$clone()
  reporter1b <- reporter$clone()
  testthat::expect_equal(reporter1a$swap_cards(1L, 2L), reporter1b$swap_cards(2L, 1L))
})

testthat::test_that("reactive_add_card", {
  reporter <- Reporter$new()
  testthat::expect_error(reporter$get_reactive_add_card())
  testthat::expect_identical(shiny::isolate(reporter$get_reactive_add_card()), 0)
  reporter$append_cards(list(card1))
  testthat::expect_identical(shiny::isolate(reporter$get_reactive_add_card()), 1L)
})

testthat::test_that("append_metadata accept only named list", {
  reporter <- Reporter$new()
  testthat::expect_error(reporter$append_metadata(list(sth = "sth")), NA)
  testthat::expect_error(reporter$append_metadata("sth"), "'list', not 'character'")
  testthat::expect_error(reporter$append_metadata(list("sth")), "Must have names")
})

testthat::test_that("append_metadata accept only unique names which could not be repeated", {
  reporter <- Reporter$new()
  testthat::expect_error(reporter$append_metadata(list(sth = "sth", sth = 2)), "but element 2 is duplicated")
  reporter <- Reporter$new()
  testthat::expect_error(reporter$append_metadata(list(sth = "sth")), NA)
  testthat::expect_error(reporter$append_metadata(list(sth = "sth")), "failed: Must be TRUE")
})

testthat::test_that("get_metadata", {
  reporter <- Reporter$new()
  testthat::expect_error(reporter$append_metadata(list(sth = "sth")), NA)
  testthat::expect_identical(reporter$get_metadata(), list(sth = "sth"))
})
