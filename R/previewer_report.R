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
    .action_button_busy(
      ns("preview_button"),
      label = shiny::tags$span(
        label,
        shiny::uiOutput(ns("preview_button_counter"))
      ),
      icon = "file-earmark-text",
      outline = TRUE
    )
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
        id = "preview_button", condition = length(reporter$get_cards()) == 0, class = "disabled"
      )
    })

    output$preview_button_counter <- shiny::renderUI({
      shiny::tags$span(
        class = "badge rounded-pill bg-primary",
        length(reporter$get_cards())
      )
    })

    preview_modal <- function() {
      shiny::tags$div(
        class = "teal-reporter reporter-previewer-modal",
        .custom_css_dependency(),
        .accordion_toggle_js_dependency(),
        shinyjs::extendShinyjs(text = "", functions = c("jumpToFocus", "enterToSubmit", "autoFocusModal")),
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
              "Dismiss"
            )
          )
        )
      )
    }

    reporter_previewer_content_srv(id = "preview_content", reporter = reporter)

    srv_list <- shiny::reactiveValues()
    shiny::observeEvent(
      list(input$preview_button, reporter$open_previewer()),
      ignoreInit = TRUE,
      {
        shiny::req(input$preview_button != 0 || !is.null(reporter$open_previewer())) # prevent unnecessary triggering.
        shiny::showModal(preview_modal())

        panel_ns <- shiny::NS(shiny::NS("preview_content", "reporter_cards"))
        lapply(
          names(reporter$get_cards()),
          function(card_id) {
            # Only show loading placeholder for cards that are being initialized for the first time
            first_run <- is.null(srv_list[[card_id]])

            bslib::accordion_panel_insert(
              id = panel_ns(NULL),
              previewer_card_ui(id = session$ns(panel_ns(card_id)), card_id = card_id, show_loading = first_run)
            )

            if (first_run) { # Only initialize srv once per card_id
              previewer_card_srv(
                id = panel_ns(card_id),
                card_r = shiny::reactive(reporter$get_cards()[[card_id]]),
                card_id = card_id,
                reporter = reporter
              )
              srv_list[[card_id]] <- card_id
            }
          }
        )
      }
    )
  })
}
