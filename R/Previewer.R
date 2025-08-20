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


# deprecated ------------------------------------------------------------------------------------------------------


#' Report previewer module
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#' Module offers functionalities to visualize, manipulate,
#' and interact with report cards that have been added to a report.
#' It includes a previewer interface to see the cards and options to modify the report before downloading.
#'
#' Cards are saved by the `shiny` bookmarking mechanism.
#'
#' For more details see the vignette: `vignette("previewerReporter", "teal.reporter")`.
#'
#' This function is deprecated and will be removed in the next release.
#' Please use `preview_report_button_ui()` and `preview_report_button_srv()`
#' to create a preview button that opens a modal with the report preview.
#'
#' @details `r global_knitr_details()`
#'
#' @name reporter_previewer_deprecated
#'
#' @param id (`character(1)`) `shiny` module instance id.
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

#' @rdname reporter_previewer_deprecated
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  lifecycle::deprecate_soft(
    when = "0.5.0",
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
            shiny::singleton(
        shiny::tags$head(
          shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")),
          shiny::includeScript(system.file("js/extendShinyJs.js", package = "teal.reporter"))
        )
      ),
      # # TODO: averissimo (implement sortable)
      # sortable::sortable_js(
      #   css_id = ns("cards-reporter_cards"),
      #   options = sortable::sortable_options(
      #     handle = ".accordion-item > .accordion-header",
      #     onSort = sortable::sortable_js_capture_input(ns("reporter_cards_order"))
      #   )
      # ),
      # # END of TODO

      # Extend shinyjs::js to include function defined in extendShinyJs.js
      shinyjs::extendShinyjs(text = "", functions = c("jumpToFocus", "enterToSubmit", "autoFocusModal")),
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

#' @rdname reporter_previewer_deprecated
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = getOption("teal.reporter.rmd_output"),
                                   rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
                                   previewer_buttons = c("download", "load", "reset")) {
  lifecycle::deprecate_soft(
    when = "0.5.0",
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
    # # TODO: averissimo (check if bookmars exclude is needed)
    # shiny::setBookmarkExclude(c(
    #   "showrcode", "download_data_prev",
    #   "load_reporter_previewer", "load_reporter"
    # ))
    # # END OF TODO
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

# reporter_previewer_content --------------------------------------------------------------------------------------

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

    session$onRestored(function(state) {
      if (is.null(state$dir)) {
        return(NULL)
      }
      reporterdir <- file.path(state$dir, "reporter")
      reporter$from_jsondir(reporterdir)
    })

    shiny::exportTestValues(cards = reporter$get_cards())

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

reporter_previewer_cards_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    id = "reporter_previewer",
    shiny::tags$div(
      id = ns("empty_reporters"),
      shiny::tags$h4(
        class = "text-muted",
        shiny::icon("circle-info"),
        "No reports have been added yet."
      )
    ),
    bslib::accordion(id = ns("reporter_cards"), open = FALSE)
  )
}

reporter_previewer_cards_srv <- function(id, reporter) {
  shiny::moduleServer(id, function(input, output, session) {
    current_ids_rv <- shiny::reactiveVal()
    queues_rv <- list(insert = shiny::reactiveVal(), remove = shiny::reactiveVal())

    shiny::observeEvent(reporter$get_cards(), {
      all_cards <- reporter$get_cards()
      reporter_ids <- names(all_cards)
      current_ids <- current_ids_rv()

      to_add <- !reporter_ids %in% current_ids
      to_remove <- !current_ids %in% reporter_ids
      if (any(to_add)) queues_rv$insert(reporter_ids[to_add])
      if (any(to_remove)) queues_rv$remove(current_ids[to_remove])

      shinyjs::toggle("empty_reporters", condition = length(all_cards) == 0L)
    })

    shiny::observeEvent(queues_rv$insert(), {
      lapply(queues_rv$insert(), function(card_id) {
        bslib::accordion_panel_insert(
          id = "reporter_cards",
          reporter_previewer_card_ui(id = session$ns(card_id), card_id = card_id)
        )
        current_ids_rv(c(current_ids_rv(), card_id))
        reporter_previewer_card_srv(
          id = card_id,
          card_r = shiny::reactive(reporter$get_cards()[[card_id]]),
          card_id = card_id,
          reporter = reporter
        )
      })
    })

    shiny::observeEvent(queues_rv$remove(), {
      lapply(queues_rv$remove(), bslib::accordion_panel_remove, id = "reporter_cards")
    })
  })
}

reporter_previewer_card_ui <- function(id, card_id) {
  ns <- shiny::NS(id)
  accordion_item <- bslib::accordion_panel(
    value = card_id,
    title = shiny::tags$label(shiny::uiOutput(ns("title"))),
    shiny::tags$h6(id = ns("loading_placeholder"), class = "text-muted", "Loading the report..."),
    shiny::uiOutput(ns("card_content"))
  )
  accordion_item <- shiny::tagAppendAttributes(accordion_item, "data-rank-id" = card_id)

  accordion_item <- shiny::tagAppendAttributes(
    tag = accordion_item,
    .cssSelector = ".accordion-header",
    class = "d-flex",
  )
  accordion_item <- shiny::tagAppendChildren(
    tag = accordion_item,
    .cssSelector = ".accordion-header",
    ui_previewer_card_actions(ns("actions"))
  )
}

# @param id (`character(1)`) card name
reporter_previewer_card_srv <- function(id, card_r, card_id, reporter) {
  # todo: card_name should be only on the server side
  shiny::moduleServer(id, function(input, output, session) {
    output$title <- shiny::renderUI({
      title <- metadata(shiny::req(card_r()), "title")
      if (is.null(title) || isFALSE(nzchar(title))) {
        title <- shiny::tags$span("(Empty title)", class = "text-muted")
      }
      title
    })
    output$card_content <- shiny::renderUI({
      result <- tools::toHTML(shiny::req(card_r()))
      shiny::removeUI(sprintf("#%s", session$ns("loading_placeholder")))
      result
    })

    srv_previewer_card_actions("actions", card_r, card_id, reporter)
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
