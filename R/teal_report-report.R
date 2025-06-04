#' Get or set the document of a `teal_report` object
#'
#' @name document
#' @param x (`teal_report`)
#' @param value (`teal_document`)
#'
#' @return The document of the `teal_report` object.
#'
#' @export
document <- function(x) {
  x@document
}

#' @rdname document
#' @export
`document<-` <- function(x, value) {
  x@document <- value
  x
}
