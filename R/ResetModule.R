#' Reset report button module
#'
#' @description
#'
#' Provides a button that triggers resetting the report content.
#'
#' For more information, refer to the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @name reset_report_button
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param label (`character(1)`) label of the button. By default `NULL`.
#' @param reporter (`Reporter`) instance.
#' @return `NULL`.
NULL

#' @rdname reset_report_button
#' @export
reset_report_button_ui <- function(id, label = NULL) {
  checkmate::assert_string(label, null.ok = TRUE)
  .outline_button(
    shiny::NS(id, "reset_reporter"),
    label = label,
    icon = "x-lg",
    type = "danger",
    outline = TRUE
  )
}

#' @rdname reset_report_button
#' @export
reset_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("reset_reporter"))

    shiny::observeEvent(reporter$get_cards(), {
      shinyjs::toggleClass(
        id = "reset_reporter", condition = length(reporter$get_cards()) == 0, class = "disabled"
      )
    })

    shiny::observeEvent(input$reset_reporter, {
      shiny::tags$div(
        class = "teal-reporter reporter-modal",
        .custom_css_dependency(),
        shiny::showModal(
          shiny::modalDialog(
            easyClose = TRUE,
            shiny::tags$h3("Reset the Report"),
            shiny::tags$hr(),
            shiny::tags$strong(
              shiny::tags$p(
                "Are you sure you want to reset the report? (This will remove ALL previously added cards)."
              )
            ),
            footer = shiny::tagList(
              shiny::tags$button(
                type = "button",
                class = "btn btn-outline-secondary",
                `data-bs-dismiss` = "modal",
                NULL,
                "Dismiss"
              ),
              shiny::actionButton(session$ns("reset_reporter_ok"), "Reset", class = "btn btn-primary")
            )
          )
        )
      )
    })

    shiny::observeEvent(reporter$get_cards(), {
      if (length(reporter$get_cards())) {
        shinyjs::enable("reset_reporter")
      } else {
        shinyjs::disable("reset_reporter")
      }
    })

    shiny::observeEvent(input$reset_reporter_ok, {
      reporter$reset()
      shiny::removeModal()
    })
  })
}
