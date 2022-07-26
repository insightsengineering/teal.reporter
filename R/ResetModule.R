#' Reset Button Reporter User Interface
#' @description `r lifecycle::badge("experimental")`
#' button for resetting the report content.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param label `character(1)` label before the icon, if used then dynamic hover label is not available.
#' By default `NULL` so a dynamic hover label is used.
#' @return `shiny::tagList`
#' @export
reset_report_button_ui <- function(id, label = NULL) {
  checkmate::assert_string(label, null.ok = TRUE)

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$button(
      id = ns("reset_reporter"),
      class = "reset_report--hover",
      type = "button",
      class = "btn btn-warning action-button",
      `data-val` = shiny::restoreInput(id = ns("reset_reporter"), default = NULL),
      NULL,
      shiny::tags$span(
        class = if (is.null(label)) "reset_report--before",
        if (!is.null(label)) label,
        shiny::icon("xmark")
      )
    )
  )
}

#' Reset Button Server
#' @description `r lifecycle::badge("experimental")`
#' server for resetting the Report content.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#' @return `shiny::moduleServer`
#' @export
reset_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      nr_cards <- length(reporter$get_cards())


      shiny::observeEvent(input$reset_reporter, {
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
                class = "btn btn-danger",
                `data-dismiss` = "modal",
                `data-bs-dismiss` = "modal",
                NULL,
                "Cancel"
              ),
              shiny::actionButton(ns("reset_reporter_ok"), "Reset", class = "btn-warning")
            )
          )
        )
      })

      shiny::observeEvent(input$reset_reporter_ok, {
        reporter$reset()
        shiny::removeModal()
      })
    }
  )
}
