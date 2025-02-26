#' @title `ReportDocument`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This `S3` class is designed to store, manage, edit and adjust report cards.
#' It facilitates the creation, manipulation, and serialization of report-related data.
#'
#' @return An `S3` `list` of class `ReportDocument`.
#' @param ... elements included in `ReportDocument`
#' @param x `ReportDocument` object
#' @param values objects to be included in the modified `ReportDocument`
#' @inheritParams base::append
#'
#' @examples
#' report <- report_document()
#' class(report)
#' attr(report, "name") <- "Report Name"
#' report <- c(report, list("## Headline"), list("## Table"), list(summary(iris)))
#' report <- report[1:2]
#' report <- append(report, c(list("## Table 2"), list(summary(mtcars))), after = 1)
#' class(report)
#'
#' report_document("Report Name", 5)
#'
#' @aliases ReportDocument
#' @name report_document
#'
#' @export
report_document <- function(...){
  objects <- list(...)
  # stopifnot("All input objects must be of length 1." = all(unlist(lapply(objects, length)) == 1))
  # Above is not needed, as ggplot has length 11.
  structure(objects, class = c('ReportDocument'))
}

#' @rdname report_document
#' @export
c.ReportDocument <- function(...){
  # Regular c() drops classes and attributes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.

  # Does not work, if ReportDocument is the second element, and not the first.
  # teal.reporter::report_document() -> x
  # class(c(list(), x)) # list
  # class(c(x, list())) # ReportDocument
  # append(x, list(), after = 1) # ReportDocument
  # append(x, list(), after = 0) # list()

  input_objects <- list(...)
  attrs <- attributes(input_objects[[1]])
  objects <- do.call(c, lapply(input_objects, unclass))
  attributes(objects) <- attrs
  objects
}

#' @rdname report_document
#' @export
`[.ReportDocument` <- function(x, i) {
  # Regular [] drops classes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.
  attrs <- attributes(x)
  xi <- unclass(x)[i]
  attributes(xi) <- attrs
  xi
}
