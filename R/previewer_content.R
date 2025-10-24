# reporter_previewer_content --------------------------------------------------------------------------------------

#' @keywords internal
reporter_previewer_content_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    .custom_css_dependency(),
    bslib::accordion(
      id = ns("reporter_cards"),
      class = "teal-reporter report-previewer-accordion"
    ),
    sortable::sortable_js(
      css_id = ns("reporter_cards"),
      options = sortable::sortable_options(
        onSort = sortable::sortable_js_capture_input(ns("reporter_cards_order")),
        handle = ".accordion-icon"
      )
    )
  )
}

#' @keywords internal
reporter_previewer_content_srv <- function(id, reporter) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude("card_remove_id")

    session$onRestored(function(state) {
      if (is.null(state$dir)) {
        return(NULL)
      }
      reporterdir <- file.path(state$dir, "reporter")
      reporter$from_jsondir(reporterdir)
    })

    shiny::exportTestValues(cards = reporter$get_cards())
    current_ids_rv <- shiny::reactiveVal()
    queues_rv <- list(insert = shiny::reactiveVal(), remove = shiny::reactiveVal())

    shiny::observeEvent(reporter$get_cards(), {
      all_cards <- reporter$get_cards()
      reporter_ids <- names(all_cards)
      current_ids <- current_ids_rv()

      to_add <- !reporter_ids %in% current_ids
      to_remove <- !current_ids %in% reporter_ids
      if (any(to_add)) queues_rv$insert(reporter_ids[to_add])
      if (any(to_remove)) queues_rv$remove(current_ids[to_remove])

      shinyjs::toggle("empty_reporters", condition = length(all_cards) == 0L)
    })

    shiny::observeEvent(queues_rv$insert(), {
      lapply(queues_rv$insert(), function(card_id) {
        current_ids_rv(c(current_ids_rv(), card_id))
      })
    })

    shiny::observeEvent(queues_rv$remove(), {
      lapply(queues_rv$remove(), bslib::accordion_panel_remove, id = "reporter_cards")
    })

    shiny::observeEvent(input$card_remove_id, {
      reporter$remove_cards(ids = input$card_remove_id)
    })

    shiny::observeEvent(input$reporter_cards_order, {
      reporter$reorder_cards(input$reporter_cards_order)
    })
  })
}
