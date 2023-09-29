#' Simple Reporter User Interface
#' @description `r lifecycle::badge("experimental")`
#' three buttons for adding cards, downloading and resetting the Report.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @return `shiny.tag`
#' @export
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(simple_reporter_ui("simple")),
#'     server = function(input, output, session) {
#'       simple_reporter_srv("simple", Reporter$new(), function(card) card)
#'     }
#'   )
#' }
simple_reporter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$div(
      class = "block mb-4 p-1",
      shiny::tags$label(class = "text-primary block -ml-1", shiny::tags$strong("Reporter")),
      shiny::tags$div(
        class = "simple_reporter_container",
        add_card_button_ui(ns("add_report_card_simple")),
        download_report_button_ui(ns("download_button_simple")),
        reset_report_button_ui(ns("reset_button_simple"))
      )
    )
  )
}

#' Simple Reporter Server
#' @description `r lifecycle::badge("experimental")`
#' three buttons for adding cards, downloading and resetting the Report.
#' The add module has `add_report_card_simple` id, the download module the `download_button_simple` id
#' and the reset module the `reset_button_simple` id.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#' @param card_fun `function` which returns a [`ReportCard`] instance,
#' the function has a `card` argument and an optional `comment` argument.
#' @param global_knitr `list` a global `knitr` parameters for customizing the rendering process.
#' Defaults to `list(echo = TRUE, tidy.opts = list(width.cutoff = 60), tidy = TRUE)`.
#' @inheritParams reporter_download_inputs
#' @return `shiny::moduleServer`
#' @export
simple_reporter_srv <- function(id,
                                reporter,
                                card_fun,
                                global_knitr = list(echo = TRUE, tidy.opts = list(width.cutoff = 60), tidy = TRUE),
                                rmd_output = c(
                                  "html" = "html_document", "pdf" = "pdf_document",
                                  "powerpoint" = "powerpoint_presentation", "word" = "word_document"
                                ),
                                rmd_yaml_args = list(
                                  author = "NEST", title = "Report",
                                  date = as.character(Sys.Date()), output = "html_document",
                                  toc = FALSE
                                )) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      add_card_button_srv("add_report_card_simple", reporter = reporter, card_fun = card_fun)
      download_report_button_srv(
        "download_button_simple",
        reporter = reporter,
        global_knitr = global_knitr,
        rmd_output = rmd_output,
        rmd_yaml_args = rmd_yaml_args
      )
      reset_report_button_srv("reset_button_simple", reporter = reporter)
    }
  )
}
