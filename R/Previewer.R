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
preview_report_button_ui <- function(id, label = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    .outline_button(
      ns("preview_button"),
      label = uiOutput(ns("preview_button_label")),
      icon = "file-earmark-text"
    )
  )
}

#' @rdname reporter_previewer
#' @export
preview_report_button_srv <- function(id, reporter) {
  checkmate::assert_class(reporter, "Reporter")

  shiny::moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude(c("preview_button"))

    observeEvent(reporter$get_reactive_add_card(), {
      shinyjs::toggleClass(
        id = "preview_button", condition = reporter$get_reactive_add_card() == 0, class = "disabled"
      )
    })

    output$preview_button_label <- renderUI({
      tags$span(
        "Preview",
        tags$span(
          class = "position-absolute badge rounded-pill bg-primary",
          style = "top: 5px; right: 5px;",
          reporter$get_reactive_add_card()
        )
      )
    })

    preview_modal <- function() {
      shiny::tags$div(
        class = "teal-reporter reporter-previewer-modal",
        shiny::modalDialog(
          easyClose = TRUE,
          size = "xl",
          reporter_previewer_only_ui(session$ns("preview_content")),
          footer = shiny::tagList(
            shiny::tags$button(
              type = "button",
              class = "btn btn-outline-secondary",
              `data-bs-dismiss` = "modal",
              NULL,
              "Cancel"
            )
          )
        )
      )
    }

    shiny::observeEvent(input$preview_button, {
      shiny::showModal(preview_modal())
    })
    reporter_previewer_only_srv(id = "preview_content", reporter = reporter) # todo: change the name but keep private
  })
}

#' @rdname reporter_previewer
#' @export
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  # lifecycle::deprecate_soft() todo:
  # lifecycle::deprecate_soft() todo:
  bslib::page_fluid(
    add_previewer_css(),
    shiny::tagList(
      shiny::tags$div(
        class = "well",
        style = "display: inline-flex; flex-direction: row; gap: 10px;",
        report_load_ui(ns("load")),
        download_report_button_ui(ns("download")),
        reset_report_button_ui(ns("reset"))
      ),
      shiny::tags$div(
        reporter_previewer_only_ui(ns("previewer"))
      )
    )
  )
}

#' @rdname reporter_previewer
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = getOption("teal.reporter.rmd_output"),
                                   rmd_yaml_args = getOption("teal.reporter.rmd_yaml_args"),
                                   previewer_buttons = c("download", "load", "reset")) {
  # lifecycle::deprecate_soft() # todo:
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
    # todo: utilize previewer_buttons to show/hide buttons
    report_load_srv("load", reporter = reporter)
    download_report_button_srv(
      "download",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    )
    reset_report_button_srv("reset", reporter = reporter)
    reporter_previewer_only_srv("previewer", reporter = reporter)
  })
}

reporter_previewer_only_ui <- function(id) {
  tags$div(
    htmltools::htmlDependency(
      name = "previewer",
      version = utils::packageVersion("teal.reporter"),
      package = "teal.reporter",
      src = "css",
      stylesheet = "Previewer.css"
    ),
    shiny::uiOutput(shiny::NS(id, "pcards"))
  )
}

reporter_previewer_only_srv <- function(id, reporter) {
  moduleServer(id, function(input, output, session) {
    shiny::setBookmarkExclude("card_remove_id")
    report_cards <- reactive({
      req(reporter$get_reactive_add_card())
      input$reporter_cards_order
      reporter$get_cards()
    })
    output$pcards <- shiny::renderUI({
      cards <- report_cards()

      if (length(cards)) {
        tags$div(
          bslib::accordion(
            id = session$ns("reporter_cards"),
            class = "teal-reporter report-previewer-accordion",
            lapply(names(cards), function(card_id) {
              htmltools::tagAppendChildren(
                tag = tags$div(
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
                  tags$a(
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
          shiny::tags$p(
            class = "text-danger mt-4",
            shiny::tags$strong("No Cards added")
          )
        )
      }
    })

    observeEvent(input$card_remove_id, {
      reporter$remove_cards(ids = input$card_remove_id)
    })

    observeEvent(input$reporter_cards_order, {
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
add_previewer_css <- function() {
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/Previewer.css", package = "teal.reporter")))
    ),
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    )
  )
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
    actionButton(
      inputId = paste0("card_remove_id_", idx),
      label = "Remove card",
      class = "card_remove_id",
      `data-cardid` = idx
    )
  )
}
