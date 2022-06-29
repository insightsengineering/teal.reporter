#' Add Card Button User Interface
#' @description `r lifecycle::badge("experimental")`
#' button for adding views/cards to the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny::tagList`
#' @export
add_card_button_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$button(
      id = ns("add_report_card_button"),
      type = "button",
      class = "btn btn-primary action-button",
      `data-val` = shiny::restoreInput(id = ns("add_report_card_button"), default = NULL),
      NULL,
      "Add Card"
    )
  )
}

#' Add Card Button Server
#' @description `r lifecycle::badge("experimental")`
#' server for adding views/cards to the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details
#' This module allows using a child of [`ReportCard`] instead of [`ReportCard`].
#' To properly support this, an instance of the child class must be passed
#' as the default value of the `card` argument in the `card_fun` function.
#' See below:
#' ```{r}
#' CustomReportCard <- R6::R6Class( # nolint: object_name_linter.
#'   classname = "CustomReportCard",
#'   inherit = teal.reporter::ReportCard
#' )
#'
#' custom_function <- function(card = CustomReportCard$new()) {
#'   card
#' }
#' ```
#'
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#' @param card_fun `function` which returns a [`ReportCard`] instance,
#' the function have at`card`argument and optional `comment`.
#' @return `shiny::moduleServer`
#' @export
add_card_button_srv <- function(id, reporter, card_fun) {
  checkmate::assert_function(card_fun)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(formals(card_fun)), c("card", "comment"), empty.ok = FALSE)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      add_modal <- function(failed = FALSE) {
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Add a card to the Report"),
          shiny::tags$hr(),
          shiny::textAreaInput(
            ns("comment"),
            "Comment",
            value = "",
            placeholder = "Add a comment here...",
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
              class = "btn btn-danger",
              `data-dismiss` = "modal",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            ),
            shiny::tags$button(
              id = ns("add_card_ok"),
              type = "button",
              class = "btn btn-primary action-button",
              `data-val` = shiny::restoreInput(id = ns("add_card_ok"), default = NULL),
              NULL,
              "Add Card"
            )
          )
        )
      }

      shiny::observeEvent(input$add_report_card_button, {
        shiny::showModal(add_modal())
      })

      shiny::observeEvent(input$add_card_ok, {
        card_fun_args_nams <- names(formals(card_fun))
        # The default_card is defined here because formals() returns a pairedlist object
        # of formal parameter names and their default values. The values are missing
        # if not defined and the missing check does not work if supplied formals(card_fun)[[1]]
        default_card <- formals(card_fun)[[1]]
        card <- `if`(
          missing(default_card),
          ReportCard$new(),
          eval(default_card, envir = environment(card_fun))
        )
        has_own_comment <- length(card_fun_args_nams) == 2
        if (has_own_comment) {
          card <- try(card_fun(card, input$comment))
        } else {
          card <- try(card_fun(card))
          if (!inherits(card, "try-error")) {
            if (length(input$comment) > 0 && input$comment != "") {
              card$append_text("Comment", "header3")
              card$append_text(input$comment)
            }
          }
        }

        if (inherits(card, "try-error")) {
          msg <- paste0(
            "The card could not be added to the report. ",
            "Have the outputs for the report been created yet? If not please try again when they ",
            "are ready. Otherwise contact your application developer" 
          )
          warning(msg)
          shiny::showNotification(
            msg,
            type = "error"
          )
        } else {
          checkmate::assert_class(card, "ReportCard")
          reporter$append_cards(list(card))
          shiny::showNotification(sprintf("The card added successfully."), type = "message")
          shiny::removeModal()
        }
      })
    }
  )
}
