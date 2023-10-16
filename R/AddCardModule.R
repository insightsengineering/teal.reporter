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

  # Buttons with custom css and
  # js code to disable the add card button when clicked to prevent multi-clicks
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(
          shiny::HTML(
            sprintf(
              '
              $(document).ready(function(event) {
                $("body").on("click", "#%s", function() {
                  $(this).addClass("disabled");
                })
              })',
              ns("add_card_ok")
            )
          )
        )
      )
    ),
    shiny::tags$button(
      id = ns("add_report_card_button"),
      type = "button",
      class = "simple_report_button btn btn-primary action-button",
      title = "Add Card",
      `data-val` = shiny::restoreInput(id = ns("add_report_card_button"), default = NULL),
      NULL,
      shiny::tags$span(
        shiny::icon("plus")
      )
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
#' @param card_fun `function` which returns a [`ReportCard`] instance. It can have optional `card`, `comment` and
#' `label` parameters. If `card` parameter is added, then the `ReportCard` instance is created for the user.
#' Use `comment` parameter to pass it's value whenever you prefer with `card$append_text()` - if `card_fun` does not
#' have `comment` parameter, then `comment` from `Add Card UI` module will be added at the end of the content of the
#' card. If `label` parameter is provided, you can use it to customize appearance of the `card name` and use if to
#' specify `card` content with `card$append_text()` - if `card_fun` does not have `label` parameter, then `card name`
#' will be set to the name passed in `Add Card UI` module, but no text will be added to the content of the `card`.
#'
#' @return `shiny::moduleServer`
#' @export
add_card_button_srv <- function(id, reporter, card_fun) {
  checkmate::assert_function(card_fun)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(formals(card_fun)), c("card", "comment", "label"), empty.ok = TRUE)

  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      add_modal <- function() {
        shiny::modalDialog(
          easyClose = TRUE,
          shiny::tags$h3("Add a Card to the Report"),
          shiny::tags$hr(),
          shiny::textInput(
            ns("label"),
            "Card Name",
            value = "",
            placeholder = "Add the card title here",
            width = "100%"
          ),
          shiny::textAreaInput(
            ns("comment"),
            "Comment",
            value = "",
            placeholder = "Add a comment here...",
            width = "100%"
          ),
          shiny::tags$script(
            shiny::HTML(
              sprintf(
                "
                $('#shiny-modal').on('shown.bs.modal', () => {
                  $('#%s').focus()
                })
                ",
                ns("label")
              )
            )
          ),
          footer = shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-secondary",
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

      # the add card button is disabled when clicked to prevent multi-clicks
      # please check the ui part for more information
      shiny::observeEvent(input$add_card_ok, {
        card_fun_args_nams <- names(formals(card_fun))
        has_card_arg <- "card" %in% card_fun_args_nams
        has_comment_arg <- "comment" %in% card_fun_args_nams
        has_label_arg <- "label" %in% card_fun_args_nams

        arg_list <- list()

        if (has_comment_arg) {
          arg_list <- c(arg_list, list(comment = input$comment))
        }
        if (has_label_arg) {
          arg_list <- c(arg_list, list(label = input$label))
        }

        if (has_card_arg) {
          # The default_card is defined here because formals() returns a pairedlist object
          # of formal parameter names and their default values. The values are missing
          # if not defined and the missing check does not work if supplied formals(card_fun)[[1]]
          default_card <- formals(card_fun)$card
          card <- `if`(
            missing(default_card),
            ReportCard$new(),
            eval(default_card, envir = environment(card_fun))
          )
          arg_list <- c(arg_list, list(card = card))
        }

        card <- try(do.call(card_fun, arg_list))

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
          if (!has_comment_arg && length(input$comment) > 0 && input$comment != "") {
            card$append_text("Comment", "header3")
            card$append_text(input$comment)
          }

          if (!has_label_arg && length(input$label) == 1 && input$label != "") {
            card$set_name(input$label)
          }

          reporter$append_cards(list(card))
          shiny::showNotification(sprintf("The card added successfully."), type = "message")
          shiny::removeModal()
        }
      })
    }
  )
}
