testthat::test_that("previewer_card_ui returns a shiny tag or tagList", {
  checkmate::expect_multi_class(
    previewer_card_ui("an_id", "a_card_id"),
    c("shiny.tag", "shiny.tag.list")
  )
})

testthat::test_that("previewer_card_srv renders the empty title output", {
  reporter <- Reporter$new()
  reporter$append_cards(teal_card("## Header", "A paragraph."))
  shiny::testServer(
    previewer_card_srv,
    args = list(
      id = "id",
      card_r = reactive(reporter$get_cards()[[1]]),
      card_id = names(reporter$get_cards()),
      reporter = reporter
    ),
    {
      testthat::expect_match(output$title$html, "(Empty title)", fixed = TRUE)
    }
  )
})

testthat::test_that("previewer_card_srv renders the title output", {
  reporter <- Reporter$new()
  card <- teal_card("## Header", "A paragraph.")
  metadata(card, "title") <- "Custom title"
  reporter$append_cards(card)
  shiny::testServer(
    previewer_card_srv,
    args = list(
      id = "id",
      card_r = reactive(reporter$get_cards()[[1]]),
      card_id = names(reporter$get_cards()),
      reporter = reporter
    ),
    {
      testthat::expect_match(output$title$html, "Custom title", fixed = TRUE)
    }
  )
})

testthat::test_that("previewer_card_srv renders the content", {
  reporter <- Reporter$new()
  card <- teal_card("## Header", "A paragraph.")
  metadata(card, "title") <- "Custom title"
  reporter$append_cards(card)
  shiny::testServer(
    previewer_card_srv,
    args = list(
      id = "id",
      card_r = reactive(reporter$get_cards()[[1]]),
      card_id = names(reporter$get_cards()),
      reporter = reporter
    ),
    {
      testthat::expect_match(output$card_content$html, "Header", fixed = TRUE)
      testthat::expect_match(output$card_content$html, "A paragraph.", fixed = TRUE)
    }
  )
})
