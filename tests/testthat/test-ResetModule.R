testthat::skip_if_not_installed("ggplot2")

card_fun <- function(card = ReportCard$new(), comment = NULL) {
  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
      ggplot2::geom_histogram()
  )
  card
}

card1 <- card_fun()
reporter <- Reporter$new()
reporter$append_cards(list(card1))

testthat::test_that("simple_reporter_srv - reset a reporter", {
  shiny::testServer(
    simple_reporter_srv,
    args = list(reporter = reporter, card_fun = card_fun),
    expr = {
      testthat::expect_identical(unname(reporter$get_cards()), list(card1))
      session$setInputs(`reset_button_simple-reset_reporter` = 0)
      session$setInputs(`reset_button_simple-reset_reporter_ok` = 0)
      testthat::expect_identical(reporter$get_blocks(), list())
    }
  )
})
