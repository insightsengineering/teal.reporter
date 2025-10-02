testthat::test_that("preview_report_button_ui returns a shiny tag or tagList", {
  checkmate::expect_multi_class(
    preview_report_button_ui("an_id"),
    c("shiny.tag", "shiny.tag.list")
  )
})

testthat::test_that("preview_report_button_srv renders the card count with 1", {
  reporter <- Reporter$new()
  reporter$append_cards(teal_card("## Header", "A paragraph."))
  shiny::testServer(
    preview_report_button_srv,
    args = list(id = "id", reporter = reporter),
    testthat::expect_match(output$preview_button_counter$html, ">1</", fixed = TRUE)
  )
})

testthat::test_that("preview_report_button_srv renders empty card count", {
  shiny::testServer(
    preview_report_button_srv,
    args = list(id = "id", reporter = Reporter$new()),
    testthat::expect_match(output$preview_button_counter$html, ">0</", fixed = TRUE)
  )
})

testthat::describe("preview_report_button_srv triggers a shiny modal", {
  it("with input click", {
    testthat::expect_warning(
      testthat::with_mocked_bindings(
        modalDialog = function(...) warning("Modal was created"),
        .package = "shiny",
        code = shiny::testServer(
          preview_report_button_srv,
          args = list(id = "id", reporter = Reporter$new()),
          {
            session$flushReact()
            session$setInputs(preview_button = 1)
            session$flushReact()
          }
        )
      ),
      "Modal was created"
    )
  })

  it("from reporter (when in editor)", {
    testthat::expect_warning(
      testthat::with_mocked_bindings(
        modalDialog = function(...) warning("Modal was shown"),
        .package = "shiny",
        code = shiny::testServer(
          preview_report_button_srv,
          args = list(id = "id", reporter = Reporter$new()),
          {
            session$flushReact()
            reporter$open_previewer(Sys.time())
            session$flushReact()
          }
        )
      ),
      "Modal was shown"
    )
  })
})
