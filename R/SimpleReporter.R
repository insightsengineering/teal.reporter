#' Simple Reporter UI
#' @description two buttons for adding views and downloading the Report
#' @param id character
#' @return shiny `tagList`
#' @export
simple_reporter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    add_card_button_ui(ns("addReportCard")),
    download_report_button_ui(ns("downloadButton")),
  )
}

#' Simple Reporter Server
#' @description two buttons for adding views and downloading the Report
#' @param id character
#' @param reporter `Reporter` instance.
#' @param card `ReportCard` instance
#' @param notification logical if to add shiny notification about the download process.
#' @return shiny `moduleServer`
#' @export
simple_reporter_srv <- function(id, reporter, card, notification = TRUE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      add_card_button_srv("addReportCard", reporter = reporter, card = card)
      download_report_button_srv("downloadButton", reporter = reporter, notification = notification)
    }
  )
}
