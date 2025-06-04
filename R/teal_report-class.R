setOldClass("teal_document")

#' Reproducible report
#'
#' Reproducible report container class. Inherits code tracking behavior from [`teal.data::teal_data-class`].
#'
#' This class provides an isolated environment in which to store and process data with all code being recorded.
#' The environment, code, data set names, and data joining keys are stored in their respective slots.
#' These slots should never be accessed directly, use the provided get/set functions.
#'
#' As code is evaluated in `teal_data`, messages and warnings are stored in their respective slots.
#' If errors are raised, a `qenv.error` object is returned.
#'
#' @name teal_report-class
#' @rdname teal_report-class
#'
#' @slot .xData (`environment`) environment containing data sets and possibly
#'  auxiliary variables.
#'  Access variables with [get()], [`$`], [teal.code::get_var()] or [`[[`].
#'  No setter provided. Evaluate code to add variables into `@.xData`.
#' @slot code (`list` of `character`) representing code necessary to reproduce the contents of `qenv`.
#'  Access with [teal.code::get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in
#' `@.xData`.
#'  Access or modify with [join_keys()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been
#'  proven to yield contents of `@.xData`.
#'  Used internally. See [`verify()`] for more details.
#' @slot document (`teal_document`)
#'
#' @inheritSection teal.data::`teal_data-class` Code
#'
#' @import teal.data
#' @import teal.code
#' @keywords internal
setClass(
  Class = "teal_report",
  contains = "teal_data",
  slots = c(document = "teal_document")
)


#' It initializes the `teal_report` class
#'
#' Accepts .xData as a list and converts it to an environment before initializing
#' parent constructor (`teal_data`).
#' @noRd
setMethod(
  "initialize",
  "teal_report",
  function(.Object, document = teal_document(), ...) { # nolint: object_name.
    args <- list(...)
    checkmate::assert_class(document, "teal_document")
    checkmate::assert_list(args, names = "named")
    methods::callNextMethod(
      .Object,
      document = document,
      ...
    )
  }
)


#' Comprehensive data integration function for `teal` applications
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Initializes a reportable data for `teal` application.
#'
#' @inheritParams teal.data::teal_data
#' @param document (`teal_document`)
#' @return A `teal_report` object.
#'
#' @seealso [`teal.data::teal_data`]
#'
#' @export
#'
#' @examples
#' teal_report(x1 = iris, x2 = mtcars)
#'
teal_report <- function(...,
                        document = teal_document(),
                        code = character(0),
                        join_keys = teal.data::join_keys()) {
  methods::new(
    "teal_report",
    .xData = list2env(list(...)),
    document = document,
    join_keys = join_keys,
    code = code
  )
}

#' @export
as.teal_report <- function(x) {
  checkmate::assert_class(x, "qenv")
  if (inherits(x, "teal_report")) {
    return(x)
  }
  new_x <- teal_report()
  for (slot_name in slotNames(x)) {
    slot(new_x, slot_name) <- slot(x, slot_name)
  }
  document(new_x) <- c(
    document(new_x),
    code_chunk(teal.code::get_code(new_x))
  )

  new_x
}

#' Create or coerce to a teal_document
#'
#' This function ensures that input is converted to a teal_document object.
#' It accepts various input types and converts them appropriately.
#'
#' @param x Object to convert to teal_document
#' @return A teal_document object
#' @export
as_teal_document <- function(x) {
  if (inherits(x, "teal_document")) {
    return(x)
  }
  if (is.list(x)) {
    return(do.call(teal_document, x))
  }
  teal_document(x)
}

#' @rdname document
#' @export
`document<-` <- function(x, value) {
  x@document <- as_teal_document(value)
  x
}


#' Append elements to a teal_report document
#'
#' This function appends elements to the document slot of a teal_report object.
#' It follows the same pattern as base R's append() function.
#'
#' @param x A teal_report object
#' @param values (`teal_document`) object(s) to append
#' @param after Integer, the position after which the elements are to be appended.
#'   If negative or zero, the values are prepended to the document.
#'   If missing, the values are appended at the end.
#'
#' @return A teal_report object with updated document slot
#'
#' @examples
#' data <- teal_report()
#' data <- append_document(data, teal_document("Some text"))
#' data <- append_document(data, list(
#'   teal_document("More text"),
#'   teal_document("Even more text")
#' ))
#' data <- append_document(data, teal_document("First text"), after = 0)
#' document(data)
#' 
#' @export
append_document <- function(x, values, after = length(document(x))) {
  checkmate::assert_class(x, "teal_report")
  checkmate::assert_int(after, lower = 0, upper = length(document(x)), null.ok = TRUE)

  document(x) <- append(document(x), values, after = after)
  x
}