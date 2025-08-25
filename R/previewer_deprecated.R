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
