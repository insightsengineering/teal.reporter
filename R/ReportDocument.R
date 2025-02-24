#' @title `ReportDocument`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This `S3` class is designed to store, manage, edit and adjust report cards.
#' It facilitates the creation, manipulation, and serialization of report-related data.
#'
#' @return An `S3` `list` of class `ReportDocument`.
#' @param ... objects passed to `c()` function
#' @param x `ReportDocument` object
#' @param values objects to be included in the modified `ReportDocument`
#' @inheritParams base::append
#'
#' @examples
#' report <- report_document()
#' class(report)
#' report <- c(report, list("## Headline"), list("## Table"), list(summary(iris)))
#' report <- report[1:2]
#' report <- append(report, c(list("## Table 2"), list(summary(mtcars))), after = 1)
#' class(report)
#'
#' @aliases ReportDocument
#' @name report_document
#'
#' @export
report_document <- function(){
  structure(list(), class = c('ReportDocument'))
}

#' @rdname report_document
#' @export
c.ReportDocument <- function(...){
  # Regular c() drops classes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.
  objects <- do.call(c, lapply(list(...), unclass))
  structure(objects, class = 'ReportDocument')
}

#' @rdname report_document
#' @export
`[.ReportDocument` <- function(x, i) {
  # Regular [] drops classes, so we either overwrite the method
  # or we do not use ReportDocument class, but list class.
  structure(unclass(x)[i], class = 'ReportDocument')
}
