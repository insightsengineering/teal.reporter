#' Simple Archiver User Interface
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
simple_archiver_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeCSS(system.file("css/custom.css", package = "teal.reporter")))
    ),
    shiny::tags$div(
      class = "block mb-4 p-1",
      shiny::tags$label(class = "text-primary block -ml-1", shiny::tags$strong("Archiver")),
      shiny::tags$div(
        class = "simple_reporter_container",
        archiver_load_ui(ns("archive_load_simple")),
        archiver_save_ui(ns("archive_save_simple"))
      )
    )
  )
}

#' Simple Archiver Server
#' @description `r lifecycle::badge("experimental")`
#' three buttons for adding cards, downloading and resetting the Report.
#' The add module has `add_report_card_simple` id, the download module the `download_button_simple` id
#' and the reset module the `reset_button_simple` id.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param id `character(1)` this `shiny` module's id.
#' @param reporter [`Reporter`] instance.
#' @return `shiny::moduleServer`
#' @export
simple_archiver_srv <- function(id, reporter) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      archiver_load_srv("archive_load_simple", reporter = reporter)
      archiver_save_srv("archive_save_simple", reporter = reporter)
    }
  )
}
