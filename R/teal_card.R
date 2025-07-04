#' @title `teal_card`: An `S3` class for managing `teal` reports
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `teal_card` `S3` class provides functionality to store, manage, edit, and adjust report contents.
#' It enables users to create, manipulate, and serialize report-related data efficiently.
#'
#' The `teal_card()` function serves two purposes:
#' 1. When called with a `teal_report` object, it acts as a getter and returns the card slot
#' 2. When called with other arguments, it creates a new `teal_card` object from those arguments
#'
#' @return An `S3` `list` of class `teal_card`.
#' @param x A `teal_report` object to extract card from, or any other object to include in a new `teal_card`
#' @param ... Additional elements to include when creating a new `teal_card`
#'
#' @details The `teal_card` class supports `c()` and `x[i]` methods for combining and subsetting elements.
#' However, these methods only function correctly when the first element is a `teal_card`.
#'
#' @examples
#' # Create a new empty card
#' report <- teal_card()
#' class(report) # Check the class of the object
#'
#' # Create a card with content
#' report <- teal_card("## Headline", "Some text", summary(iris))
#'
#' # Extract card from a teal_report
#' tr <- teal_report(teal_card = teal_card("## Title"))
#' doc <- teal_card(tr)
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
#' # Verify that the object remains a teal_card
#' class(report)
#'
#' @aliases teal_card
#' @name teal_card
#'
#' @export
teal_card <- function(x, ...) {
  if (missing(x)) {
    structure(list(), class = "teal_card")
  } else if (inherits(x, "teal_report")) {
    x@teal_card
  } else if (inherits(x, "qenv")) {
    .code_to_card(x@code)
  } else {
    objects <- list(x, ...)
    names(objects) <- vapply(
      sample.int(.Machine$integer.max, size = length(objects)),
      function(x) substr(rlang::hash(list(Sys.time(), x)), 1, 8),
      character(1)
    )
    structure(objects, class = "teal_card")
  }
}

#' @rdname teal_card
#' @param value (`teal_card`) object to set in the `teal_report`.
#' @export
`teal_card<-` <- function(x, value) {
  x <- as.teal_report(x)
  checkmate::assert_class(x, "teal_report")
  x@teal_card <- as.teal_card(value)
  x
}

#' Create or coerce to a teal_card
#'
#' This function ensures that input is converted to a teal_card object.
#' It accepts various input types and converts them appropriately.
#'
#' @param x Object to convert to teal_card
#' @return A teal_card object
#' @rdname teal_card
#' @export
as.teal_card <- function(x) { # nolint: object_name.
  if (inherits(x, "teal_card")) {
    return(x)
  }
  if (identical(class(x), "list")) {
    return(do.call(teal_card, x))
  }
  teal_card(x)
}

#' @rdname teal_card
#' @export
c.teal_card <- function(...) {
  dots <- list(...)
  structure(
    Reduce(
      f = function(u, v) {
        v <- as.teal_card(v)
        if (length(names(u)) && length(names(v)) && any(names(u) %in% names(v))) { # when v stems from u
          if (all(names(u) %in% names(v))) { # nothing from `u` is removed in `v`
            v
          } else {
            warning(
              "Appended `teal_card` doesn't remove some of the elements from previous `teal_card`.\n",
              "Restoring original content and adding only new items to the end of the card."
            )
            utils::modifyList(u, v)
          }
        } else {
          attrs <- utils::modifyList(attributes(u) %||% list(), attributes(v))
          attrs$names <- union(names(u), names(v))
          result <- utils::modifyList(u, v)
          attributes(result) <- attrs
          result
        }
      },
      x = dots,
      init = list()
    ),
    class = "teal_card"
  )
}

#' @param i index specifying elements to extract or replace
#' @rdname teal_card
#' @export
`[.teal_card` <- function(x, i) {
  out <- NextMethod()
  class(out) <- "teal_card"
  attr(out, "metadata") <- metadata(x)
  out
}

#' Access metadata from a `teal_card` or `ReportCard`
#'
#' This function retrieves metadata from a `teal_card` or `ReportCard` object.
#' When `which` is `NULL`, it returns all metadata fields as a list.
#' @param object (`teal_card` or `ReportCard`) The object from which to extract metadata.
#' @param which (`character` or `NULL`) The name of the metadata field to extract.
#' @return A list of metadata fields or a specific field if `which` is provided.
#' @export
metadata <- function(object, which = NULL) {
  checkmate::assert_string(which, null.ok = TRUE)
  UseMethod("metadata", object)
}

#' @rdname metadata
#' @export
metadata.teal_card <- function(object, which = NULL) {
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

#' Set metadata for a `teal_card` or `ReportCard`
#'
#' This function allows you to set or modify metadata fields in a `teal_card` or `ReportCard` object.
#' It can be used to add new metadata or update existing fields.
#' @param object (`teal_card` or `ReportCard`) The object to modify.
#' @param which (`character`) The name of the metadata field to set.
#' @param value The value to assign to the specified metadata field.
#' @return The modified object with updated metadata.
#' @export
`metadata<-` <- function(object, which = NULL, value) {
  checkmate::assert_string(which, null.ok = TRUE)
  UseMethod("metadata<-", object)
}

#' @rdname metadata-set
#' @export
`metadata<-.teal_card` <- function(object, which = NULL, value) {
  if (missing(which)) {
    checkmate::assert_list(value, names = "named")
    attr(object, which = "metadata") <- value
    return(object)
  }
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
  if (missing(which)) {
    if (!is.null(value[["title"]])) {
      object$set_name(value[["title"]])
    }
    if (length(value) >= 2 || length(value) == 1 && is.null(value[["title"]])) {
      warning("ReportCard class only supports `title` in metadata.")
    }
    return(object)
  }

  if (isFALSE(identical(which, "title"))) {
    warning("ReportCard class only supports `title` in metadata.")
  } else {
    object$set_name(value)
  }
  object
}

#' Generate an R Markdown code chunk
#'
#' This function creates a `code_chunk` object, which represents an R Markdown
#' code chunk. It stores the R code and any specified chunk options (e.g., `echo`, `eval`).
#' These objects are typically processed later to generate the final R Markdown text.
#'
#' @param code A character string containing the R code.
#' @param ... Additional named parameters to be included as chunk options (e.g., `echo = TRUE`).
#' @param lang (`character(1)`) See [`knitr::knit_engines`].
#'
#' @return An object of class `code_chunk`.
#' @examples
#' my_chunk <- code_chunk("x <- 1:10", echo = TRUE, message = FALSE)
#' class(my_chunk)
#' attributes(my_chunk)$param
#' @export
code_chunk <- function(code, ..., lang = "R") {
  checkmate::assert_character(code)
  params <- list(...)
  structure(
    paste(code, collapse = "\n"),
    params = params,
    lang = lang,
    class = "code_chunk"
  )
}

#' Builds `teal_card` from code and outputs in `qenv` object
#'
#' Builds a `teal_card` from the code and outputs of a `teal_data`
#' object, preserving the order of code execution and output display.
#'
#' @inheritParams eval_code-teal_report
#' @param x (`list`) object from `qenv@code`.
#' @return A `teal_card` built from the code and outputs in a `qenv` object.
#' @keywords internal
.code_to_card <- function(x, code_block_opts = list()) {
  elems <- Reduce(
    function(items, code_elem) {
      this_chunk <- do.call(code_chunk, c(list(code = code_elem), code_block_opts))
      this_outs <- Filter( # intentionally remove warnings,messages from the generated report
        function(x) !inherits(x[[1]], "condition"),
        lapply(
          attr(code_elem, "outputs"),
          function(x) structure(list(x), class = c("chunk_output"))
        )
      )
      c(items, list(this_chunk), this_outs)
    },
    init = list(),
    x = x
  )
  do.call(teal_card, args = elems)
}
