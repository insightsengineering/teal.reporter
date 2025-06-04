#' @title `teal_document`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `teal_document` `S3` class provides functionality to store, manage, edit, and adjust report contents.
#' It enables users to create, manipulate, and serialize report-related data efficiently.
#'
#' The `teal_document()` function serves two purposes:
#' 1. When called with a `teal_report` object, it acts as a getter and returns the document slot
#' 2. When called with other arguments, it creates a new `teal_document` object from those arguments
#'
#' @return An `S3` `list` of class `teal_document`.
#' @param x A `teal_report` object to extract document from, or any other object to include in a new `teal_document`
#' @param ... Additional elements to include when creating a new `teal_document`
#' @inheritParams base::append
#'
#' @details The `teal_document` class supports `c()` and `x[i]` methods for combining and subsetting elements.
#' However, these methods only function correctly when the first element is a `teal_document`.
#' To prepend, reorder, or modify a `teal_document`, use the `edit_teal_document()` function.
#'
#' @examples
#' # Create a new empty teal_document
#' report <- teal_document()
#' class(report) # Check the class of the object
#'
#' # Create a teal_document with content
#' report <- teal_document("## Headline", "Some text", summary(iris))
#'
#' # Extract document from a teal_report
#' tr <- teal_report(document = teal_document("## Title"))
#' doc <- teal_document(tr)
#'
#' # Add elements to the report
#' report <- c(report, list("## Table"), list(summary(mtcars)))
#'
#' # Subset the report to keep only the first two elements
#' report <- report[1:2]
#'
#' # Append new elements after the first element
#' report <- append(report, c(list("## Table 2"), list(summary(mtcars))), after = 1)
#'
#' # Verify that the object remains a teal_document
#' class(report)
#'
#' @aliases teal_document
#' @name teal_document
#'
#' @export
teal_document <- function(x, ...) {
  if (inherits(x, "teal_report")) {
    x@document
  } else {
    objects <- list(x, ...)
    structure(objects, class = "teal_document")
  }
}

#' @rdname teal_document
#' @export
`teal_document<-` <- function(x, value) {
  checkmate::assert_class(x, "teal_report")
  x@document <- as.teal_document(value)
  x
}

#' Create or coerce to a teal_document
#'
#' This function ensures that input is converted to a teal_document object.
#' It accepts various input types and converts them appropriately.
#'
#' @param x Object to convert to teal_document
#' @return A teal_document object
#' @export
as.teal_document <- function(x) {
  if (inherits(x, "teal_document")) {
    return(x)
  }
  if (is.list(x)) {
    return(do.call(teal_document, x))
  }
  teal_document(x)
} 

#' @rdname teal_document
#' @export
c.teal_document <- function(...) {
  dots <- list(...)
  structure(
    Reduce(
      f = function(u, v) append(u, if (inherits(v, "teal_document")) v else list(v)),
      x = dots[-1],
      init = unclass(dots[[1]]) # unclass to avoid infinite recursion
    ),
    class = "teal_document"
  )
}

#' @param i index specifying elements to extract or replace
#' @rdname teal_document
#' @export
`[.teal_document` <- function(x, i) {
  out <- NextMethod()
  class(out) <- "teal_document"
  out
}

#' Access metadata from a `teal_document` or `ReportCard`
#'
#' This function retrieves metadata from a `teal_document` or `ReportCard` object.
#' When `which` is `NULL`, it returns all metadata fields as a list.
#' @param object (`teal_document` or `ReportCard`) The object from which to extract metadata.
#' @param which (`character` or `NULL`) The name of the metadata field to extract.
#' @return A list of metadata fields or a specific field if `which` is provided.
#' @export
metadata <- function(object, which = NULL) {
  checkmate::assert_string(which, null.ok = TRUE)
  UseMethod("metadata", object)
}

#' @rdname metadata
#' @export
metadata.teal_document <- function(object, which = NULL) {
  metadata <- attr(object, which = "metadata", exact = TRUE)
  result <- metadata %||% list()
  if (is.null(which)) {
    return(result)
  }
  result[[which]]
}

#' @rdname metadata
#' @export
metadata.ReportCard <- function(object, which = NULL) {
  # TODO: soft deprecate
  result <- list(title = object$get_name())
  if (is.null(which)) {
    return(result)
  }
  result[[which]]
}

#' Set metadata for a `teal_document` or `ReportCard`
#'
#' This function allows you to set or modify metadata fields in a `teal_document` or `ReportCard` object.
#' It can be used to add new metadata or update existing fields.
#' @param object (`teal_document` or `ReportCard`) The object to modify.
#' @param which (`character`) The name of the metadata field to set.
#' @param value The value to assign to the specified metadata field.
#' @return The modified object with updated metadata.
#' @export
`metadata<-` <- function(object, which, value) {
  checkmate::assert_string(which)
  UseMethod("metadata<-", object)
}

#' @rdname metadata-set
#' @export
`metadata<-.teal_document` <- function(object, which, value) {
  attr(object, which = "metadata") <- utils::modifyList(
    metadata(object), structure(list(value), names = which)
  )
  object
}

#' @rdname metadata-set
#' @details
#' The `ReportCard` class only supports the `title` field in metadata.
#' @export
`metadata<-.ReportCard` <- function(object, which, value) {
  if (which != "title") {
    warning("ReportCard class only supports `title` in metadata.")
  } else {
    object$set_name(value)
  }
  object
}

#' @rdname teal_document
#' @param x `teal_document`
#' @param modify An integer vector specifying element indices to extract and reorder.
#' If `NULL`, no modification is applied.
#' @param append An object to be added to the `teal_document` using `append()`.
#' The `after` parameter determines the insertion position.
#'
#' @examples
#' #### edit_teal_document examples ###
#' report <- teal_document(1, 2, "c")
#'
#' # Modify and append to the report
#' new_report <- edit_teal_document(report, modify = c(3, 1), append = "d")
#' new_report
#' class(new_report)
#'
#' @export
edit_teal_document <- function(x, modify = NULL, append = NULL, after = length(x)) {
  checkmate::assert_class(x, "teal_document")
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
#' This function creates a `code_chunk` object, which represents an R Markdown
#' code chunk. It stores the R code and any specified chunk options (e.g., `echo`, `eval`).
#' These objects are typically processed later to generate the final R Markdown text.
#'
#' @param code A character string containing the R code.
#' @param ... Additional named parameters to be included as chunk options (e.g., `echo = TRUE`).
#'
#' @return An object of class `code_chunk`.
#' @examples
#' my_chunk <- code_chunk("x <- 1:10", echo = TRUE, message = FALSE)
#' class(my_chunk)
#' attributes(my_chunk)$param
#' @export
code_chunk <- function(code, ...) {
  checkmate::assert_character(code)
  params <- list(...)
  structure(
    paste(code, collapse = "\n"),
    params = params,
    class = "code_chunk"
  )
}

#' @title Keep Objects In Report
#' @description Utility function to change behavior of `teal_document` elements to be
#' kept (`keep = TRUE`) or discarded (`keep = FALSE`) from the final `.Rmd` file containing the downloaded report.
#' @details By default, R objects like `summary` outputs are only printed in the output document but their
#'   code is not included in the `.Rmd` report source. Text elements (character strings) and `code_chunk`
#'   objects are, by default, kept both in the output document and the `.Rmd` report source.
#'   This function allows overriding the default behavior for specific objects.
#' @param object An R object, typically an element intended for a `teal_document`.
#' @param keep (`logical`) If `TRUE` (default), the object is marked to be kept in the `.Rmd` source;
#'   if `FALSE`, it's marked for printing only in the output document (and not in the `.Rmd` source,
#'   though its print output will be in the rendered document).
#'
#' @return The input `object` with its "keep" attribute modified.
#' @examples
#' item <- summary(iris)
#' item <- keep_in_report(item, TRUE)
#' attributes(item)$keep
#'
#' @export
keep_in_report <- function(object, keep = TRUE) {
  attr(object, "keep") <- keep
  object
}
