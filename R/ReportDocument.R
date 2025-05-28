#' @title `doc`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `doc` `S3` class provides functionality to store, manage, edit, and adjust report contents.
#' It enables users to create, manipulate, and serialize report-related data efficiently.
#'
#' @return An `S3` `list` of class `doc`.
#' @param ... elements included in `doc`
#' @param x `doc` object
#' @inheritParams base::append
#'
#' @details The `doc` class supports `c()` and `x[i]` methods for combining and subsetting elements.
#' However, these methods only function correctly when the first element is a `doc`.
#' To prepend, reorder, or modify a `doc`, use the `edit_doc()` function.
#'
#'
#' @examples
#' # Create a new doc
#' report <- doc()
#' class(report) # Check the class of the object
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
#' # Verify that the object remains a doc
#' class(report)
#'
#' @aliases doc
#' @name doc
#'
#' @export
doc <- function(...) {
  objects <- list(...)
  structure(objects, class = c("doc"))
}

#' @rdname doc
#' @export
c.doc <- function(...) {
  out <- c(list(), list(...)[[1]], list(...)[-1])
  class(out) <- "doc"
  out
}

#' @param i index specifying elements to extract or replace
#' @rdname doc
#' @export
`[.doc` <- function(x, i) {
  out <- NextMethod()
  class(out) <- "doc"
  out
}

#' @export
metadata <- function(object, which = NULL) {
  checkmate::assert_string(which, null.ok = TRUE)
  UseMethod("metadata", object)
}

#' @export
metadata.doc <- function(object, which = NULL) {
  metadata <- attr(object, which = "metadata", exact = TRUE)
  result <- metadata %||% list()
  if (is.null(which)) {
    return(result)
  }
  result[[which]]
}

#' @export
metadata.ReportCard <- function(object, which = NULL) {
  # TODO: soft deprecate
  result <- list(title = object$get_name())
  if (is.null(which)) {
    return(result)
  }
  result[[which]]
}

#' @export
`metadata<-` <- function(object, which, value) {
  checkmate::assert_string(which)
  UseMethod("metadata<-", object)
}

#' @export
`metadata<-.doc` <- function(object, which, value) {
  attr(object, which = "metadata") <- modifyList(
    metadata(object), structure(list(value), names = which)
  )
  object
}

#' @export
`metadata<-.ReportCard` <- function(object, which, value) {
  if (which != "title") {
    warning("ReportCard class only supports `title` in metadata.")
  } else {
    object$set_name(value)
  }
  object
}

#' @rdname doc
#' @param x `doc`
#' @param modify An integer vector specifying element indices to extract and reorder.
#' If `NULL`, no modification is applied.
#' @param append An object to be added to the `doc` using `append()`.
#' The `after` parameter determines the insertion position.
#'
#' @examples
#' #### edit_doc examples ###
#' report <- doc(1, 2, "c")
#'
#' # Modify and append to the report
#' new_report <- edit_doc(report, modify = c(3, 1), append = "d")
#' new_report
#' class(new_report)
#'
#' @export
edit_doc <- function(x, modify = NULL, append = NULL, after = length(x)) {
  checkmate::assert_class(x, "doc")
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
#' @rdname code_output
code_chunk <- function(code, ...) {
  checkmate::assert_character(code)
  params <- list(...)
  structure(
    paste(code, collapse = "\n"),
    params = params,
    class = "code_chunk"
  )
}

#' Format R code as a simple Markdown code block string
#'
#' This function takes a character string of R code and wraps it in
#' Markdown's triple backticks for code blocks.
#'
#' @param code A character string containing the R code.
#' @return A character string representing a simple Markdown code block.
#' @seealso [code_chunk()] for creating structured code chunk objects with options.
#' @examples
#' code_output("y <- rnorm(5)")
#' @export
#' @rdname code_output
code_output <- function(code) {
  sprintf("```\n%s\n```", code)
}

#' @title Keep Objects In Report
#' @description Utility function to change behavior of `doc` elements to be
#' kept (`keep = TRUE`) or discarded (`keep = FALSE`) from the final `.Rmd` file containing the downloaded report.
#' @details By default, R objects like `summary` outputs are only printed in the output document but their
#'   code is not included in the `.Rmd` report source. Text elements (character strings) and `code_chunk`
#'   objects are, by default, kept both in the output document and the `.Rmd` report source.
#'   This function allows overriding the default behavior for specific objects.
#' @param object An R object, typically an element intended for a `doc`.
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
