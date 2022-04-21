#' Extract Add Card Button id
#' @description extract Add Card Button id.
#' It is needed to know when trigger the reactivity cycle for the `ReportCard`.
#' @param input shiny input, `reactivevalues`.
#' @note has to be invoked inside the reactive call.
#' @export
extract_addcard_id <- function(input) {
  checkmate::assert_class(input, "reactivevalues")
  nams <- names(input)
  which_addcard <- grep("addReportCardButton$", nams)
  res <- nams[which_addcard]
  val <- if (length(res) == 1) {
    res
  } else {
    "not_exists_id"
  }
}
