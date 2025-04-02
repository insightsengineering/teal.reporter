#' @title `ReportDocument`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `ReportDocument` `S3` class provides functionality to store, manage, edit, and adjust report contents.
#' It enables users to create, manipulate, and serialize report-related data efficiently.
#'
#' @return An `S3` `list` of class `ReportDocument`.
#' @param ... elements included in `ReportDocument`
#' @param x `ReportDocument` object
#' @param values objects to be included in the modified `ReportDocument`
#' @inheritParams base::append
#'
#' @details The `ReportDocument` class supports `c()` and `x[i]` methods for combining and subsetting elements.
#' However, these methods only function correctly when the first element is a `ReportDocument`.
#' To prepend, reorder, or modify a `ReportDocument`, use the `edit_report_document()` function.
#'
#'
#' @examples
#' # Create a new ReportDocument
#' report <- report_document()
#' class(report)  # Check the class of the object
#'
#' # Add elements to the report
#' report <- c(report, list("## Headline"), list("## Table"), list(summary(iris)))
#'
#' # Subset the report to keep only the first two elements
#' report <- report[1:2]
#'
#' # Append new elements after the first element
#' report <- append(report, c(list("## Table 2"), list(summary(mtcars))), after = 1)
#'
#' # Verify that the object remains a ReportDocument
#' class(report)
#'
#' @aliases ReportDocument
#' @name report_document
#'
#' @export
report_document <- function(...) {
  objects <- list(...)
  # stopifnot("All input objects must be of length 1." = all(unlist(lapply(objects, length)) == 1))
  # Above is not needed, as ggplot has length 11.
  structure(objects, class = c("ReportDocument"))
}

#' @rdname report_document
#' @export
c.ReportDocument <- function(...) {
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

#' @rdname report_document
#' @param x `ReportDocument`
#' @param modify An integer vector specifying element indices to extract and reorder.
#' If `NULL`, no modification is applied.
#' @param append An object to be added to the `ReportDocument` using `append()`.
#' The `after` parameter determines the insertion position.
#'
#' @examples
#' #### edit_report_document examples ###
#' report <- report_document(1, 2, "c")
#'
#' # Modify and append to the report
#' new_report <- edit_report_document(report, modify = c(3, 1), append = "d")
#' new_report
#' class(new_report)
#'
#' @export
edit_report_document <- function(x, modify = NULL, append = NULL, after = length(x)) {
  checkmate::assert_class(x, "ReportDocument")
  checkmate::assert_class(modify, "numeric", null.ok = TRUE)

  attrs <- attributes(x)

  if (!is.null(modify)) {
    x <- x[modify]
  }

  if (!is.null(append)) {
    x <- append(x, append, after)
  }

  attributes(x) <- attrs
  x
}

#' Generate an R Markdown code chunk
#'
#' This function takes a character string as input and formats it as an R Markdown code chunk.
#' Additional named parameters passed via `...` will be included inside `{r}`.
#'
#' @param code A character string containing the R code to be wrapped in the chunk.
#' @param ... Additional named parameters to be included inside `{r}`.
#'
#' @return A formatted character string representing an R Markdown code chunk.
#' @examples
#' code_chunk("x <- 1:10", echo = TRUE, message = FALSE)
#' @export
#' @rdname code_output
code_chunk <- function(code, ...) {
  params <- list(...)
  structure(
    code,
    params = params,
    class = "code_chunk"
  )
}

#' @export
#' @rdname code_output
code_output <- function(code) {
  sprintf("```\n%s\n```", code)
}

#' @title Keep Objects In Report
#' @description Utility function to change behavior of `report_document()` elements to be
#' kept (`keep = TRUE`) or discarded (keep = `FALSE`) from the final `.Rmd` file containing downloaded report.
#' @details By default all R objects are only printed in the output document, but not kept in the `.Rmd` report.
#' By defaulf all text elements and `code_chunk` objects are kep both in the output document and `.Rmd` report.
#'
#' @export
#' @rdname keep_in_report
keep_in_report <- function(object, keep = TRUE) {
  attr(object, "keep") <- keep
  object
}

#' #' @export
#' #' @rdname code_output
#' link_output <- function(object, output) {
#'   attr(object, "output") <- output
#'   object
#' }
