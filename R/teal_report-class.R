setOldClass("teal_card")

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
#'  Access variables with [get()], [`$`]  or [`[[`].
#'  No setter provided. Evaluate code to add variables into `@.xData`.
#' @slot code (`list` of `character`) representing code necessary to reproduce the contents of `qenv`.
#'  Access with [teal.code::get_code()].
#'  No setter provided. Evaluate code to append code to the slot.
#' @slot join_keys (`join_keys`) object specifying joining keys for data sets in
#' `@.xData`.
#'  Access or modify with [teal.data::join_keys()].
#' @slot verified (`logical(1)`) flag signifying that code in `@code` has been
#'  proven to yield contents of `@.xData`.
#'  Used internally. See [`teal.data::verify()`] for more details.
#' @slot card (`teal_card`)
#' @inheritSection teal.data::`teal_data-class` Code
#' @importFrom teal.data teal_data
#' @keywords internal
setClass(
  Class = "teal_report",
  contains = "teal_data",
  slots = c(teal_card = "teal_card")
)

#' It initializes the `teal_report` class
#'
#' Accepts .xData as a list and converts it to an environment before initializing
#' parent constructor (`teal_data`).
#' @noRd
setMethod(
  "initialize",
  "teal_report",
  function(.Object, teal_card = NULL, ...) { # nolint: object_name.
    args <- list(...)
    if (is.null(teal_card)) teal_card <- teal_card()
    checkmate::assert_class(teal_card, "teal_card")
    checkmate::assert_list(args, names = "named")
    methods::callNextMethod(
      .Object,
      teal_card = teal_card,
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
#' @param teal_card (`teal_card`) object containing the report content.
#' @return A `teal_report` object.
#'
#' @seealso [`teal.data::teal_data`]
#'
#' @export
#'
#' @examples
#' # Initialize teal_report with existing h2 header
#' report <- teal_report(teal_card = teal_card("## Analysis Report"))
#'
#' # Use within() to execute code and add code-chunk
#' report <- within(report, {
#'   data <- iris
#'   summary_stats <- summary(data)
#' })
#'
#' # Access objects created within the report
#' report$data
#' report$summary_stats
#'
#' # within() automatically captures code and outputs
#' report <- within(report, {
#'   head(iris)
#' })
#'
#' # Add markdown content to the card
#' teal_card(report) <- c(
#'   teal_card(report),
#'   teal_card("### Conclusion", "The analysis is complete.")
#' )
#'
#' # View the generated card with code chunks
#' teal_card(report)
#'
#' # View report in HTML format
#' tools::toHTML(report)
#'
#' if (interactive()) {
#'   # Render the report to various formats
#'   render(report, output_format = rmarkdown::html_document())
#'   render(report, output_format = rmarkdown::pdf_document())
#' }
#'
teal_report <- function(...,
                        teal_card = NULL,
                        code = character(0),
                        join_keys = teal.data::join_keys()) {
  if (is.null(teal_card)) teal_card <- teal_card()
  methods::new(
    "teal_report",
    .xData = list2env(list(...)),
    teal_card = teal_card,
    join_keys = join_keys,
    code = code
  )
}

#' Internal function to convert `qenv` or `teal_data` to `teal_report`
#' @noRd
coerce.teal_report <- function(from, to) { # nolint: object_name.
  if (inherits(from, "teal_report")) {
    return(from)
  }
  new_x <- teal_report()
  for (slot_name in methods::slotNames(from)) {
    methods::slot(new_x, slot_name) <- methods::slot(from, slot_name)
  }
  teal_card(new_x) <- .code_to_card(from@code)
  new_x
}

methods::setAs("qenv", "teal_report", coerce.teal_report)
methods::setAs("teal_data", "teal_report", coerce.teal_report)

#' @rdname teal_report
#' @param x (`qenv` or `teal_data`) object to convert to `teal_report`.
#' @export
as.teal_report <- function(x) { # nolint: object_name.
  checkmate::assert_class(x, "qenv")
  methods::as(x, "teal_report")
}
