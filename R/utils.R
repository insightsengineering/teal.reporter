#' Extract Add Card Button input
#' @description extract Add Card Button input.
#' It is needed to know when trigger the reactivity cycle for the `ReportCard`.
#' @param input shiny input, `reactivevalues`.
#' @note has to be invoked inside the reactive call.
#' @export
extract_addcard_input <- function(input) {
  checkmate::assert_class(input, "reactivevalues")
  nams <- names(input)
  which_addcard <- grep("addReportCardButton$", nams)
  res <- nams[which_addcard]
  val <-
    reactive(
      if (length(res) == 1) {
        input[[res]]
      } else {
        NULL
      }
    )
  val()
  }
