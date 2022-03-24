testthat::test_that("Reporter object can be created", {
  testthat::expect_error(Reporter$new(), regexp = NA)
})

testthat::test_that("new returns an object of type Reporter", {
  testthat::expect_true(inherits(Reporter$new(), "Reporter"))
})

card1 <- ReportCard$new()

card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

card2 <- ReportCard$new()

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

reporter <- Reporter$new()

testthat::test_that("get_blocks and get_cards return empty list by default", {
  expect_identical(reporter$get_blocks(), list())
  expect_identical(reporter$get_cards(), list())
})
