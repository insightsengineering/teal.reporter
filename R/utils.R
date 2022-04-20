#' Extract Add Card Button id
#' @description extract Add Card Button id.
#' It is needed to know when trigger the reactivity cycle for the `ReportCard`.
#' @param input shiny input
#' @note has to be invoked inside the reactive call.
#' @export
extract_addcard_id <- function(input) {
  nams <- names(input)
  which_addcard <- grep("addReportCard-addReportCard$", nams)
  res <- nams[which_addcard]
  if (length(res) == 1) {
    res
  } else {
    "NULL"
  }
}
