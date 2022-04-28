card1 <- ReportCard$new()
card1$append_text("Header 2 text", "header2")
card1$append_text("A paragraph of default text", "header2")
card1$append_plot(
  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) +
    ggplot2::geom_histogram()
)

reporter <- Reporter$new()

testthat::test_that("add_card_srv", {
  shiny::testServer(
    add_card_srv,
    args = list(reporter = reporter, card = reactive(card1)),
    expr = {
      card_len <- length(card()$get_content())
      session$setInputs(`addReportCardButton` = 0)
      session$setInputs(comment = "Comment Body")
      session$setInputs(`addCardOk` = 0)

      testthat::expect_identical(
        length(reporter$get_blocks()),
        card_len + 2L
      )

      testthat::expect_identical(
        tail(reporter$get_blocks(), 1)[[1]]$get_content(),
        "Comment Body"
      )

      testthat::expect_identical(
        tail(reporter$get_blocks(), 2)[[1]]$get_content(),
        "Comment"
      )
    }
  )
})
