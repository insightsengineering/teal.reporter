#' Extract report from `teal_reportable`
#'
#' @param x (`teal_reportable`)
#' @return `teal_reportable`
#' @export
report <- function(x) {
  checkmate::assert_class(x, "teal_reportable")
  x@report
}

#' Replace a report in `teal_reportable`
#'
#' @param x (`teal_reportable`)
#' @param value (`ReportDocument`)
#' @return `teal_reportable`
#' @export
`report<-` <- function(x, value) {
  checkmate::assert_class(x, "teal_reportable")
  checkmate::assert_class(value, classes = "ReportDocument")
  x@report <- value
  x
}
