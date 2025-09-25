create_test_reporter <- function(n_cards = 2) {
  cards <- lapply(seq_len(n_cards), function(i) {
    new_doc <- teal_card(sprintf("Card %d", i))
    metadata(new_doc, "title") <- sprintf("Card %d Title", i)
    new_doc
  })

  reporter <- teal.reporter::Reporter$new()
  reporter$append_cards(cards)
  reporter
}

start_reporter_preview_app <- function(name) {
  skip_if_too_deep(5)
  testthat::skip_if_not_installed("chromote")

  reporter <- create_test_reporter(2)

  # suppressWarnings is necessary to avoid warning "'package:teal.reporter' may not be available when loading"
  testapp <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      preview_report_button_ui("preview")
    ),
    server = function(input, output, session) {
      preview_report_button_srv(
        "preview",
        reporter = reporter
      )
    }
  )

  app <- tryCatch(
    suppressWarnings(
      shinytest2::AppDriver$new(
        testapp,
        name = name,
        options = list(
          chromePath = NULL,
          windowSize = c(1000, 800),
          browserOptions = list(position = NULL, debug = FALSE)
        ),
        seed = 123,
        timeout = default_idle_timeout
      )
    ),
    error = function(e) skip(paste("Could not initialize AppDriver:", e$message))
  )

  withr::defer_parent(try(app$stop(), silent = TRUE))

  app$wait_for_idle()
  app
}
