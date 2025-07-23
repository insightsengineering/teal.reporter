#' Report previewer module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Module offers functionalities to visualize, manipulate,
#' and interact with report cards that have been added to a report.
#' It includes a previewer interface to see the cards and options to modify the report before downloading.
#'
#' Cards are saved by the `shiny` bookmarking mechanism.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name reporter_previewer
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param label (`character(1)`) label of the button. By default it is "Preview Report".
#' @param reporter (`Reporter`) instance.
#' @param global_knitr (`list`) of `knitr` parameters (passed to `knitr::opts_chunk$set`)
#'  for customizing the rendering process.
#' @param previewer_buttons (`character`) set of modules to include with `c("download", "load", "reset")` possible
#' values and `"download"` is required.
#' Default `c("download", "load", "reset")`
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
NULL

#' @rdname reporter_previewer
#' @export
preview_report_button_ui <- function(id, label = "Preview Report") {
  checkmate::assert_string(label, null.ok = TRUE)
  ns <- shiny::NS(id)
  .outline_button(
    ns("preview_button"),
    label = shiny::tags$span(
      label,
      shiny::uiOutput(ns("preview_button_counter"))
    ),
    icon = "file-earmark-text"
  )
}

#' @rdname reporter_previewer
#' @export
preview_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("preview_button"))

    shiny::observeEvent(reporter$get_reactive_add_card(), {
      shinyjs::toggleClass(
        id = "preview_button", condition = reporter$get_reactive_add_card() == 0, class = "disabled"
      )
    })

    output$preview_button_counter <- shiny::renderUI({
      shiny::tags$span(
        class = "position-absolute badge rounded-pill bg-primary",
        style = "top: 5px; right: 5px;",
        reporter$get_reactive_add_card()
      )
    })

    preview_modal <- function() {
      shiny::tags$div(
        class = "teal-reporter reporter-previewer-modal",
        .custom_css_dependency(),
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          reporter_previewer_content_ui(session$ns("preview_content")),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              `data-bs-dismiss` = "modal",
              NULL,
              "Dismiss"
            )
          )
        )
      )
    }

    shiny::observeEvent(input$preview_button, {
      shiny::showModal(preview_modal())
    })
    reporter_previewer_content_srv(id = "preview_content", reporter = reporter)
  })
}

#' @rdname reporter_previewer
#' @description `r lifecycle::badge("deprecated")`
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  lifecycle::deprecate_soft(
    when = "",
    what = "reporter_previewer_ui()",
    details = paste(
      "Calling `reporter_previewer_ui()` is deprecated and will be removed in the next release.\n",
      "Please use `report_load_ui()`, `download_report_button_ui()`, `reset_report_button_ui()`,",
      "and `preview_report_button_ui()` instead."
    )
  )
  bslib::page_fluid(
    shiny::tagList(
      shinyjs::useShinyjs(),
      shiny::tags$div(
        class = "well",
        style = "display: inline-flex; flex-direction: row; gap: 10px;",
        shiny::tags$span(id = ns("load_span"), report_load_ui(ns("load"), label = "Load Report")),
        shiny::tags$span(
          id = ns("download_span"), download_report_button_ui(ns("download"), label = "Download Report")
        ),
        shiny::tags$span(id = ns("reset_span"), reset_report_button_ui(ns("reset"), label = "Reset Report"))
      ),
      shiny::tags$div(
        reporter_previewer_content_ui(ns("previewer"))
      )
    )
  )
}

#' @rdname reporter_previewer
#' @description `r lifecycle::badge("deprecated")`
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = getOption("teal.reporter.rmd_output"),
                                   rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
                                   previewer_buttons = c("download", "load", "reset")) {
  lifecycle::deprecate_soft(
    when = "",
    what = "reporter_previewer_srv()",
    details = paste(
      "Calling `reporter_previewer_srv()` is deprecated and will be removed in the next release.\n",
      "Please use `report_load_srv()`, `download_report_button_srv()`, `reset_report_button_srv()`,",
      "and `preview_report_button_srv()` instead."
    )
  )
  checkmate::assert_subset(previewer_buttons, c("download", "load", "reset"), empty.ok = FALSE)
  checkmate::assert_true("download" %in% previewer_buttons)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c("html_document", "pdf_document", "powerpoint_presentation", "word_document"),
    empty.ok = FALSE
  )
  checkmate::assert_list(rmd_yaml_args, names = "named")
  checkmate::assert_names(
    names(rmd_yaml_args),
    subset.of = c("author", "title", "date", "output", "toc"),
    must.include = "output"
  )
  checkmate::assert_true(rmd_yaml_args[["output"]] %in% rmd_output)

  shiny::moduleServer(id, function(input, output, session) {
    if (!"load" %in% previewer_buttons) {
      shinyjs::hide(id = "load_span")
    }
    if (!"download" %in% previewer_buttons) {
      shinyjs::hide(id = "download_span")
    }
    if (!"reset" %in% previewer_buttons) {
      shinyjs::hide(id = "reset_span")
    }
    report_load_srv("load", reporter = reporter)
    download_report_button_srv(
      "download",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    )
    reset_report_button_srv("reset", reporter = reporter)
    reporter_previewer_content_srv("previewer", reporter = reporter)
  })
}

#' @keywords internal
reporter_previewer_content_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "pcards"))
}

#' @keywords internal
reporter_previewer_content_srv <- function(id, reporter) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude("card_remove_id")
    report_cards <- shiny::reactive({
      shiny::req(reporter$get_reactive_add_card())
      input$reporter_cards_order
      reporter$get_cards()
    })
    output$pcards <- shiny::renderUI({
      cards <- report_cards()

      if (length(cards)) {
        shiny::tags$div(
          .custom_css_dependency(),
          bslib::accordion(
            id = session$ns("reporter_cards"),
            class = "teal-reporter report-previewer-accordion",
            lapply(names(cards), function(card_id) {
              htmltools::tagAppendChildren(
                tag = shiny::tags$div(
                  id = card_id,
                  `data-rank-id` = card_id,
                  bslib::accordion_panel(
                    title = cards[[card_id]]$get_name(),
                    icon = bslib::tooltip(
                      bsicons::bs_icon("arrows-move"),
                      "Move card"
                    ),
                    shiny::tags$div(
                      id = paste0("card", card_id),
                      lapply(
                        cards[[card_id]]$get_content(),
                        function(b) {
                          block_to_html(b)
                        }
                      )
                    )
                  )
                ),
                .cssSelector = ".accordion-button",
                bslib::tooltip(
                  shiny::tags$a(
                    class = "action-button",
                    role = "button",
                    style = "text-decoration: none;",
                    onclick = sprintf(
                      "Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                      session$ns("card_remove_id"),
                      card_id
                    ),
                    bsicons::bs_icon("x-circle", class = "text-danger")
                  ),
                  "Remove card"
                )
              )
            })
          ),
          sortable::sortable_js(
            css_id = session$ns("reporter_cards"),
            options = sortable::sortable_options(
              onSort = sortable::sortable_js_capture_input(session$ns("reporter_cards_order")),
              handle = ".accordion-icon"
            )
          )
        )
      } else {
        shiny::tags$div(
          shiny::tags$br(),
          shiny::tags$p(
            class = "text-danger",
            shiny::tags$strong("No Cards added")
          )
        )
      }
    })

    shiny::observeEvent(input$card_remove_id, {
      reporter$remove_cards(ids = input$card_remove_id)
    })

    shiny::observeEvent(input$reporter_cards_order, {
      reporter$reorder_cards(input$reporter_cards_order)
    })
  })
}

#' @noRd
#' @keywords internal
block_to_html <- function(b) {
  b_content <- b$get_content()
  if (inherits(b, "TextBlock")) {
    switch(b$get_style(),
      header1 = shiny::tags$h1(b_content),
      header2 = shiny::tags$h2(b_content),
      header3 = shiny::tags$h3(b_content),
      header4 = shiny::tags$h4(b_content),
      verbatim = shiny::tags$pre(b_content),
      shiny::tags$pre(b_content)
    )
  } else if (inherits(b, "RcodeBlock")) {
    panel_item("R Code", shiny::tags$pre(b_content))
  } else if (inherits(b, "PictureBlock")) {
    shiny::tags$img(src = knitr::image_uri(b_content))
  } else if (inherits(b, "TableBlock")) {
    b_table <- readRDS(b_content)
    shiny::tags$pre(
      flextable::htmltools_value(b_table)
    )
  } else if (inherits(b, "NewpageBlock")) {
    shiny::tags$br()
  } else if (inherits(b, "HTMLBlock")) {
    b_content
  } else {
    stop("Unknown block class")
  }
}

#' @noRd
#' @keywords internal
previewer_collapse_item <- function(idx, card_name, card_blocks) {
  htmltools::tagAppendChildren(
    tag = bslib::accordion_panel(
      title = paste0("Card ", idx, ": ", card_name),
      shiny::tags$div(
        id = paste0("card", idx),
        lapply(
          card_blocks,
          function(b) {
            block_to_html(b)
          }
        )
      )
    ),
    .cssSelector = ".accordion-header",
    shiny::actionButton(
      inputId = paste0("card_remove_id_", idx),
      label = "Remove card",
      class = "card_remove_id",
      `data-cardid` = idx
    )
  )
}
