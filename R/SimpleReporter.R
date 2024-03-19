#' Simple reporter module
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Module provides compact UI and server functions for managing a report in a `shiny` app.
#' This module combines functionalities for [adding cards to a report][add_card_button],
#'  [downloading the report][download_report_button], and [resetting report content][reset_report_button].
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#'
#' @details `r global_knitr_details()`
#'
#' @name simple_reporter
#'
#' @param id (`character(1)`) `shiny` module instance id.
#' @param reporter (`Reporter`) instance.
#' @param card_fun (`function`) which returns a [`ReportCard`] instance,
#' the function has a `card` argument and an optional `comment` argument.
#' @param global_knitr (`list`) a global `knitr` parameters for customizing the rendering process.
#' @param modules (`character`) set of modules to include with `c("add", "download", "load", "reset")` possible values.
#' Can be set by `teal.reporter.simple_reporter_modules` R option. Default `c("add", "download", "load", "reset")`
#' @inheritParams reporter_download_inputs
#'
#' @return `NULL`.
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'
#'   shinyApp(
#'     ui = fluidPage(simple_reporter_ui("simple")),
#'     server = function(input, output, session) {
#'       simple_reporter_srv("simple", Reporter$new(), function(card) card)
#'     }
#'   )
#' }
NULL

#' @rdname simple_reporter
#' @export
simple_reporter_ui <- function(
    id,
    modules = getOption("teal.reporter.simple_reporter_modules", c("add", "download", "load", "reset"))
) {
  checkmate::assert_subset(modules, c("add", "download", "load", "reset"), empty.ok = FALSE)
  ns <- shiny::NS(id)
  mods <- list(
    add = add_card_button_ui(ns("add_report_card_simple")),
    download = download_report_button_ui(ns("download_button_simple")),
    load = report_load_ui(ns("archive_load_simple")),
    reset = reset_report_button_ui(ns("reset_button_simple"))
  )
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$div(
      class = "block mb-4 p-1",
      shiny::tags$label(class = "text-primary block -ml-1", shiny::tags$strong("Reporter")),
      shiny::tags$div(
        class = "simple_reporter_container",
        mods[modules]
      )
    )
  )
}

#' @rdname simple_reporter
#' @export
simple_reporter_srv <- function(
    id,
    reporter,
    card_fun,
    global_knitr = getOption("teal.reporter.global_knitr"),
    rmd_output = c(
      "html" = "html_document", "pdf" = "pdf_document",
      "powerpoint" = "powerpoint_presentation", "word" = "word_document"
    ),
    rmd_yaml_args = list(
      author = "NEST", title = "Report",
      date = as.character(Sys.Date()), output = "html_document",
      toc = FALSE
    ),
    modules = getOption("teal.reporter.simple_reporter_modules", c("add", "download", "load", "reset"))
) {
  checkmate::assert_subset(modules, c("add", "download", "load", "reset"), empty.ok = FALSE)
  mods <- list(
    add = function() add_card_button_srv("add_report_card_simple", reporter = reporter, card_fun = card_fun),
    download = function() download_report_button_srv(
      "download_button_simple",
      reporter = reporter,
      global_knitr = global_knitr,
      rmd_output = rmd_output,
      rmd_yaml_args = rmd_yaml_args
    ),
    load = function() report_load_srv("archive_load_simple", reporter = reporter),
    reset = function() reset_report_button_srv("reset_button_simple", reporter = reporter)
  )
  shiny::moduleServer(
    id,
    function(input, output, session) {
      for (mod in modules) {
        mods[[mod]]()
      }
    }
  )
}
