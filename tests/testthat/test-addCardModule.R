card_fun <- function(card = ReportCard$new(),
                     comment = NULL) {
  card$append_text("Header 2 text", "header2")
  card$append_text("A paragraph of default text", "header2")
  card$append_plot(
    ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
      ggplot2::geom_histogram()
  )
  card
}


reporter <- Reporter$new()

testthat::test_that("add_card_button_srv - add a Card to the Reporter", {
  shiny::testServer(
    add_card_button_srv,
    args = list(reporter = reporter, card_fun = card_fun),
    expr = {
      card_len <- length(card_fun()$get_content())
      session$setInputs(`addReportCardButton` = 0)
      session$setInputs(comment = "Comment Body")
      session$setInputs(`addCardOk` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len
      )
    }
  )
})

testthat::test_that("add_card_button_ui - returns a tagList", {
  testthat::expect_true(
    inherits(add_card_button_ui("sth"), c("shiny.tag.list", "list"))
  )
})
