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

testthat::test_that("", {

})

