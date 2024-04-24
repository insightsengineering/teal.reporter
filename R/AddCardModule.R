#' Add card button module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Provides a button to add views/cards to a report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details
#' The `card_fun` function is designed to create a new `ReportCard` instance and optionally customize it:
#' - The `card` parameter allows for specifying a custom or default `ReportCard` instance.
#' - Use the `comment` parameter to add a comment to the card via `card$append_text()` - if `card_fun` does not
#' have the `comment` parameter, then `comment` from `Add Card UI` module will be added at the end of the content of the
#' card.
#' - The `label` parameter enables customization of the card's name and its content through `card$append_text()`-
#' if `card_fun` does not have the `label` parameter, then card name will be set to the name passed in
#' `Add Card UI` module, but no text will be added to the content of the `card`.
#'
#' This module supports using a subclass of [`ReportCard`] for added flexibility.
#' A subclass instance should be passed as the default value of
#' the `card` argument in the `card_fun` function.
#' See below:
#' ```{r}
#' CustomReportCard <- R6::R6Class(
#'   classname = "CustomReportCard",
#'   inherit = teal.reporter::ReportCard
#' )
#'
#' custom_function <- function(card = CustomReportCard$new()) {
#'   card
#' }
#' ```
#' @name add_card_button
#'
#' @param id (`character(1)`) this `shiny` module's id.
#' @param reporter (`Reporter`) instance.
#' @param card_fun (`function`) which returns a [`ReportCard`] instance. See `Details`.
#'
#' @return `NULL`.
NULL

#' @rdname add_card_button
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

#' @rdname add_card_button
#' @export
add_card_button_srv <- function(id, reporter, card_fun, env) {
  checkmate::assert_function(card_fun)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(formals(card_fun)), c("card", "comment", "label", "env"), empty.ok = TRUE)
  checkmate::assert_environment(env, null.ok = TRUE)

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c(
      "add_report_card_button", "download_button", "reset_reporter",
      "add_card_ok", "download_data", "reset_reporter_ok",
      "label", "comment"
    ))

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
      has_env_arg <- "label" %in% card_fun_args_nams

      arg_list <- list()

      if (has_comment_arg) {
        arg_list <- c(arg_list, list(comment = input$comment))
      }
      if (has_label_arg) {
        arg_list <- c(arg_list, list(label = input$label))
      }
      if (has_env_arg && !is.null(env)) {
        arg_list <- c(arg_list, env = env)
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
  })
}
