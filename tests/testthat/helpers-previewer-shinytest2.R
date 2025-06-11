create_test_reporter <- function(n_cards = 2) {
  cards <- lapply(seq_len(n_cards), function(i) {
    new_doc <- teal.reporter::teal_card(sprintf("Card %d", i))
    teal.reporter::metadata(new_doc, "title") <- sprintf("Card %d Title", i)
    new_doc
  })

  reporter <- teal.reporter::Reporter$new()
  reporter$append_cards(cards)
  reporter
}

start_reporter_preview_app <- function(name) {
  skip_if_too_deep(5)
  skip_if_not(requireNamespace("chromote", quietly = TRUE), "chromote is not available")

  reporter <- create_test_reporter(2)

  # Prefix is necessary to avoid warning "'package:teal.reporter' may not be available when loading"
  testapp <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      teal.reporter::reporter_previewer_ui("preview")
    ),
    server = function(input, output, session) {
      teal.reporter::reporter_previewer_srv(
        "preview",
        reporter = reporter,
        rmd_output = c("html" = "html_document"),
        rmd_yaml_args = list(
          author = "TEST",
          title = "Test Report",
          date = as.character(Sys.Date()),
          output = "html_document",
          toc = FALSE
        )
      )
    }
  )

  app <- tryCatch(
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
    ),
    error = function(e) skip(paste("Could not initialize AppDriver:", e$message))
  )

  withr::defer(
    try(app$stop(), silent = TRUE),
    envir = parent.frame()
  )

  app$wait_for_idle()
  app
}
