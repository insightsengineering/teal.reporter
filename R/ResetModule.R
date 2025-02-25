#' Reset report button module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Provides a button that triggers resetting the report content.
#'
#' For more information, refer to the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @name reset_report_button
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param label (`character(1)`) label before the icon.
#' By default `NULL`.
#' @param reporter (`Reporter`) instance.
#' @return `NULL`.
NULL

#' @rdname reset_report_button
#' @export
reset_report_button_ui <- function(id, label = NULL) {
  checkmate::assert_string(label, null.ok = TRUE)

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    actionButton(
      ns("reset_reporter"),
      class = "teal-reporter simple_report_button clear-report btn-warning",
      title = "Reset",
      `data-val` = shiny::restoreInput(id = ns("reset_reporter"), default = NULL),
      shiny::tags$span(
        if (!is.null(label)) label,
        shiny::icon("xmark")
      )
    )
  )
}

#' @rdname reset_report_button
#' @export
reset_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("reset_reporter"))

    ns <- session$ns
    nr_cards <- length(reporter$get_cards())


    shiny::observeEvent(input$reset_reporter, {
      div(
        class = "teal-widgets reporter-modal",
        shiny::showModal(
          shiny::modalDialog(
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
                class = "btn btn-secondary",
                `data-dismiss` = "modal",
                `data-bs-dismiss` = "modal",
                NULL,
                "Cancel"
              ),
              shiny::actionButton(ns("reset_reporter_ok"), "Reset", class = "btn-danger")
            )
          )
        )
      )
    })

    shiny::observeEvent(input$reset_reporter_ok, {
      reporter$reset()
      shiny::removeModal()
    })
  })
}
