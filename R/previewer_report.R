#' Show report previewer button module
#'
#' @description `r lifecycle::badge("experimental")`
#' Provides a button that triggers showing the report preview in a modal.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' @name reporter_previewer
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param label (`character(1)`) label of the button. By default it is "Preview Report".
#' @param reporter (`Reporter`) instance.
#'
#' @return `NULL`.
NULL

#' @rdname reporter_previewer
#' @export
preview_report_button_ui <- function(id, label = "Preview Report") {
  checkmate::assert_string(label, null.ok = TRUE)
  ns <- shiny::NS(id)
  shiny::tagList(
    .outline_button(
      ns("preview_button"),
      label = shiny::tags$span(
        label,
        shiny::uiOutput(ns("preview_button_counter"))
      ),
      icon = "file-earmark-text"
    ),
    shinyjs::hidden(uiOutput(ns("preview_hidden")))
  )
}

#' @rdname reporter_previewer
#' @export
preview_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("preview_button"))

    shiny::observeEvent(reporter$get_cards(), {
      shinyjs::toggleClass(
        id = "preview_button", condition = length(reporter) == 0, class = "disabled"
      )
    })

    output$preview_button_counter <- shiny::renderUI({
      shiny::tags$span(
        class = "position-absolute badge rounded-pill bg-primary",
        length(reporter)
      )
    })

    preview_modal <- function(cached_content) {
      shiny::tags$div(
        class = "teal-reporter reporter-previewer-modal",
        .custom_css_dependency(),
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          title = "Report Preview",
          reporter_previewer_content_ui(session$ns("preview_content")),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              "data-bs-dismiss" = "modal",
              NULL,
              "Dismiss"
            )
          )
        )
      )
    }

    reporter_previewer_content_srv(id = "preview_content", reporter = reporter)
    shiny::observeEvent(input$preview_button, {
      shiny::showModal(preview_modal())

      panel_ns <- shiny::NS(shiny::NS("preview_content", "reporter_cards"))
      lapply(
        names(reporter$get_cards()),
        function(card_id) {
          bslib::accordion_panel_insert(
            id = panel_ns(NULL),
            previewer_card_ui(id = session$ns(panel_ns(card_id)), card_id = card_id)
          )
          previewer_card_srv(
            id = panel_ns(card_id),
            card_r = shiny::reactive(reporter$get_cards()[[card_id]]),
            card_id = card_id,
            reporter = reporter
          )
        }
      )
    })
  })
}
