#' Extract report from `teal_report`
#'
#' @param x (`teal_report`)
#' @return `teal_report`
#' @export
report <- function(x) {
  checkmate::assert_class(x, "teal_report")
  x@report
}

#' Replace a report in `teal_report`
#'
#' @param x (`teal_report`)
#' @param value (`doc`)
#' @return `teal_report`
#' @export
`report<-` <- function(x, value) {
  checkmate::assert_class(x, "teal_report")
  checkmate::assert_class(value, classes = "doc")
  x@report <- value
  x
}
