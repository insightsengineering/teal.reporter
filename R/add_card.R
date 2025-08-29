#' Add card button module
#'
#' @description
#'
#' Provides a button to add views/cards to a report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details
#' The `card_fun` function is designed to create a new `ReportCard` instance and optionally customize it:
#' - The `teal_card` parameter allows for specifying a custom or default `ReportCard` instance.
#' - Use the `comment` parameter to add a comment to the card via `card$append_text()` - if `card_fun` does not
#' have the `comment` parameter, then `comment` from `Add Card UI` module will be added at the end of the content of the
#' card.
#' - The `label` parameter enables customization of the card's name and its content through `card$append_text()`-
#' if `card_fun` does not have the `label` parameter, then card name will be set to the name passed in
#' `Add Card UI` module, but no text will be added to the content of the `teal_card`.
#'
#' This module supports using a subclass of [`ReportCard`] for added flexibility.
#' A subclass instance should be passed as the default value of
#' the `teal_card` argument in the `card_fun` function.
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
#' @param label (`character(1)`) label of the button. By default it is empty.
#' @param card_fun (`function`) which returns a [`ReportCard`] instance. See `Details`.
#'
#' @return `NULL`.
NULL

#' @rdname add_card_button
#' @export
add_card_button_ui <- function(id, label = NULL) {
  checkmate::assert_string(label, null.ok = TRUE)
  .outline_button(
    shiny::NS(id, "add_report_card_button"),
    icon = "plus-lg",
    label = label
  )
}

#' @rdname add_card_button
#' @export
add_card_button_srv <- function(id, reporter, card_fun) {
  checkmate::assert_function(card_fun)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(formals(card_fun)), c("card", "comment", "label"), empty.ok = TRUE)

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c(
      "add_report_card_button", "download_button", "reset_reporter",
      "add_card_ok", "download_data", "reset_reporter_ok",
      "label", "comment"
    ))

    ns <- session$ns

    add_modal <- function() {
      shiny::div(
        class = "teal-reporter reporter-modal",
        .custom_css_dependency(),
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
              sprintf("shinyjs.autoFocusModal('%s');", ns("label")), # See extendShinyJs.js
              sprintf("shinyjs.enterToSubmit('%s', '%s');", ns("label"), ns("add_card_ok")) # See extendShinyJs.js
            )
          ),
          footer = shiny::div(
            shiny::tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              `data-bs-dismiss` = "modal",
              NULL,
              "Dismiss"
            ),
            shiny::tags$button(
              id = ns("add_card_ok"),
              type = "button",
              class = "btn btn-primary action-button",
              NULL,
              "Add Card"
            )
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

      shinyjs::disable("add_card_ok")

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
        shinyjs::enable("add_card_ok")
      } else {
        checkmate::assert_multi_class(card, c("ReportCard", "teal_card"))
        if (inherits(card, "ReportCard")) {
          if (!has_comment_arg && length(input$comment) > 0 && input$comment != "") {
            card$append_text("Comment", "header3")
            card$append_text(input$comment)
          }

          if (!has_label_arg && length(input$label) == 1 && input$label != "") {
            card$set_name(input$label)
          }
        } else if (inherits(card, "teal_card")) {
          if (!has_comment_arg && length(input$comment) > 0 && input$comment != "") {
            card <- c(card, "### Comment", input$comment)
          }
          if (!has_label_arg && length(input$label) == 1 && input$label != "") {
            metadata(card, "title") <- input$label
          }
        }

        reporter$append_cards(list(card))
        shiny::showNotification(sprintf("The card added successfully."), type = "message")
        shiny::removeModal()
      }
    })
  })
}
