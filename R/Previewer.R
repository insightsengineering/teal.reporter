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
reporter_previewer_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fluid(
    shiny::tagList(
      shiny::singleton(
        shiny::tags$head(
          shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")),
          shiny::includeScript(system.file("js/extendShinyJs.js", package = "teal.reporter"))
        )
      ),
      # Extend shinyjs::js to include function defined in extendShinyJs.js
      shinyjs::extendShinyjs(text = "", functions = c("jumpToFocus", "enterToSubmit", "autoFocusModal")),
      sortable::sortable_js(
        css_id = ns("cards-reporter_cards"),
        options = sortable::sortable_options(
          onSort = sortable::sortable_js_capture_input(ns("reporter_cards_order"))
        )
      ),
      shiny::tags$div(
        class = "block mb-4 p-1",
        shiny::tags$div(
          class = "simple_reporter_container",
          download_report_button_ui(ns("download"), label = "Download Report"),
          report_load_ui(ns("load"), label = "Load Report"),
          reset_report_button_ui(ns("reset"), label = "Reset Report")
        )
      ),
      reporter_previewer_cards_ui(ns("cards"))
    )
  )
}

#' @rdname reporter_previewer
#' @export
reporter_previewer_srv <- function(id,
                                   reporter,
                                   global_knitr = getOption("teal.reporter.global_knitr"),
                                   rmd_output = c(
                                     "html" = "html_document", "pdf" = "pdf_document",
                                     "powerpoint" = "powerpoint_presentation",
                                     "word" = "word_document"
                                   ),
                                   rmd_yaml_args = list(
                                     author = "NEST", title = "Report",
                                     date = as.character(Sys.Date()), output = "html_document",
                                     toc = FALSE
                                   ),
                                   previewer_buttons = c("download", "load", "reset")) {
  checkmate::assert_subset(previewer_buttons, c("download", "load", "reset"), empty.ok = FALSE)
  checkmate::assert_true("download" %in% previewer_buttons)
  checkmate::assert_class(reporter, "Reporter")
  checkmate::assert_subset(names(global_knitr), names(knitr::opts_chunk$get()))
  checkmate::assert_subset(
    rmd_output,
    c(
      "html_document", "pdf_document",
      "powerpoint_presentation", "word_document"
    ),
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
    shiny::setBookmarkExclude(c(
      "showrcode", "download_data_prev",
      "load_reporter_previewer", "load_reporter"
    ))

    session$onBookmark(function(state) {
      reporterdir <- file.path(state$dir, "reporter")
      dir.create(reporterdir)
      reporter$to_jsondir(reporterdir)
    })
    session$onRestored(function(state) {
      reporterdir <- file.path(state$dir, "reporter")
      reporter$from_jsondir(reporterdir)
    })

    ns <- session$ns

    download_report_button_srv(
      "download",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    )
    report_load_srv("load", reporter = reporter)
    reset_report_button_srv("reset", reporter = reporter)
    reporter_previewer_cards_srv("cards", reporter)

    shiny::observeEvent(input$reporter_cards_order, {
      reporter$reorder_cards(setdiff(input$reporter_cards_order, ""))
    })
  })
}

reporter_previewer_cards_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    id = "reporter_previewer",
    tags$div(
      id = ns("empty_reporters"),
      tags$h4(
        class = "text-muted",
        shiny::icon("circle-info"),
        "No reports have been added yet."
      )
    ),
    bslib::accordion(id = ns("reporter_cards"), open = FALSE)
  )
}

reporter_previewer_cards_srv <- function(id, reporter) {
  moduleServer(id, function(input, output, session) {
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
          card_r = reactive(reporter$get_cards()[[card_id]]),
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
    tags$h6(id = ns("loading_placeholder"), class = "text-muted", "Loading the report..."),
    shiny::uiOutput(ns("card_content"))
  )
  accordion_item <- htmltools::tagAppendAttributes(accordion_item, "data-rank-id" = card_id)

  accordion_item <- htmltools::tagAppendAttributes(
    tag = accordion_item,
    .cssSelector = ".accordion-header",
    class = "d-flex",
  )
  accordion_item <- htmltools::tagAppendChildren(
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
      title <- metadata(req(card_r()), "title")
      if (isFALSE(nzchar(title))) {
        title <- tags$span("(empty title)", class = "text-muted")
      }
      title
    })
    output$card_content <- shiny::renderUI({
      result <- toHTML(req(card_r()))
      shiny::removeUI(sprintf("#%s", session$ns("loading_placeholder")))
      result
    })

    srv_previewer_card_actions("actions", card_r, card_id, reporter)
  })
}

#' @importFrom tools toHTML
#' @keywords internal
#' @export
toHTML.ReportCard <- function(x, ...) {
  lapply(x$get_content(), toHTML)
}

#' @keywords internal
#' @export
toHTML.ReportDocument <- function(x, ...) {
  lapply(x, toHTML)
}

#' @keywords internal
#' @export
toHTML.default <- function(x, ...) {
  shiny::HTML(commonmark::markdown_html(x, extensions = TRUE))
}

#' @keywords internal
#' @export
toHTML.ContentBlock <- function(x, ...) {
  UseMethod("toHTML", x$get_content()) # Further dispatch for subclasses
}

#' @keywords internal
#' @export
toHTML.TextBlock <- function(x, ...) {
  b_content <- x$get_content()
  switch(x$get_style(),
    header1 = shiny::tags$h1(b_content),
    header2 = shiny::tags$h2(b_content),
    header3 = shiny::tags$h3(b_content),
    header4 = shiny::tags$h4(b_content),
    verbatim = shiny::tags$pre(b_content),
    shiny::tags$pre(b_content)
  )
}

#' @keywords internal
#' @export
toHTML.RcodeBlock <- function(x, ...) {
  panel_item("R Code", shiny::tags$pre(x$get_content()))
}

#' @keywords internal
#' @export
toHTML.PictureBlock <- function(x, ...) {
  shiny::tags$img(src = knitr::image_uri(x$get_content()))
}

#' @keywords internal
#' @export
toHTML.TableBlock <- function(x, ...) {
  b_table <- readRDS(x$get_content())
  shiny::tags$pre(flextable::htmltools_value(b_table))
}

#' @keywords internal
#' @export
toHTML.NewpageBlock <- function(x, ...) {
  shiny::tags$br()
}

#' @keywords internal
#' @export
toHTML.HTMLBlock <- function(x, ...) {
  x$get_content()
}

#' @keywords internal
#' @export
toHTML.rtables <- function(x, ...) {
  shiny::tags$pre(flextable::htmltools_value(to_flextable(x)))
}

#' @keywords internal
#' @export
toHTML.gg <- function(x, ...) {
  tmpfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(tmpfile, plot = x, width = 5, height = 4, dpi = 100)
  shiny::tags$img(src = knitr::image_uri(tmpfile))
}

#' @keywords internal
#' @export
toHTML.code_chunk <- function(x, ...) {
  shiny::tags$pre(x)
}

#' @keywords internal
#' @export
toHTML.TableTree <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.ElementaryTable <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.rlisting <- toHTML.rtables

#' @keywords internal
#' @export
toHTML.data.frame <- toHTML.rtables
