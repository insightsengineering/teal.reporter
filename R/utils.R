#' Extract Add Card Button input
#' @description extract Add Card Button input, looks for an id with a suffix `addReportCardButton`.
#' It is needed to identify when to trigger the reactivity cycle for the `ReportCard`.
#'
#' For more details see the vignette: `vignette("simpleReporter", "teal.reporter")`.
#' @param input shiny input, `reactivevalues`.
#' @return value
#' @note has to be invoked inside the active reactive context.
#' @export
extract_addcard_input <- function(input) {
  checkmate::assert_class(input, "reactivevalues")
  nams <- names(input)
  which_addcard <- grep("addReportCardButton$", nams)
  res <- nams[which_addcard]
  val <-
    shiny::reactive(
      if (length(res) == 1) {
        input[[res]]
      } else {
        NULL
      }
    )
  val()
}
