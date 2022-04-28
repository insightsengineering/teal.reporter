#' Add Card Button User Interface
#' @description button for adding views/cards to the Report. Part of the simple Reporter user interface.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character`
#' @return `shiny::tagList`
#' @export
add_card_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$button(
      id = ns("addReportCardButton"),
      type = "button",
      class = "btn btn-primary action-button",
      `data-val` = shiny::restoreInput(id = ns("addReportCardButton"), default = NULL),
      NULL,
      "Add Card"
    )
  )
}

#' Add Card Button Server
#' @description server for adding views/cards the Report. Part of the simple Reporter.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character`
#' @param reporter `Reporter` instance.
#' @param card `ReportCard` instance
#' @return `shiny::moduleServer`
#' @export
#' @export
add_card_button_srv <- function(id, reporter, card) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      add_modal <- function(failed = FALSE) {
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Add the Card to the Report"),
          shiny::tags$hr(),
          shiny::textInput(
            ns("comment"),
            "Comment",
            value = "The idea behind",
            width = "100%"
          ),
          if (failed) {
            shiny::tags$div(
              shiny::tags$b("Invalid", style = "color: red;")
            )
          },
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-primary",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::tags$button(
              id = ns("addCardOk"),
              type = "button",
              class = "btn btn-primary action-button",
              `data-val` = shiny::restoreInput(id = ns("addCardOk"), default = NULL),
              NULL,
              "Add Card"
            )
          )
        )
      }

      shiny::observeEvent(input$addReportCardButton, {
        shiny::showModal(add_modal())
      })

      shiny::observeEvent(input$addCardOk, {
        checkmate::assert_class(card(), "ReportCard")
        card()$append_text("Comment", "header3")
        card()$append_text(input$comment)
        reporter$append_cards(list(card()))
        shiny::removeModal()
      })
    }
  )
}
