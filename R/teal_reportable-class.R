setOldClass("ReportDocument")

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
#' @name teal_reportable-class
#' @rdname teal_reportable-class
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
#' @slot report (`ReportDocument`)
#'
#' @inheritSection teal.data::`teal_data-class` Code
#'
#' @import teal.data
#' @import teal.code
#' @keywords internal
setClass(
  Class = "teal_reportable",
  contains = "teal_data",
  slots = c(report = "ReportDocument")
)


#' It initializes the `teal_reportable` class
#'
#' Accepts .xData as a list and converts it to an environment before initializing
#' parent constructor (`teal_data`).
#' @noRd
setMethod(
  "initialize",
  "teal_reportable",
  function(.Object, report = report_document(), ...) { # nolint: object_name.
    print("init teal_reportable")
    args <- list(...)
    checkmate::assert_class(report, "ReportDocument")
    checkmate::assert_list(args, names = "named")
    methods::callNextMethod(
      .Object,
      report = report,
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
#' @param raport (`ReportDocument`)
#' @return A `teal_reportable` object.
#'
#' @seealso [`teal.data::teal_data`]
#'
#' @export
#'
#' @examples
#' teal_reportable(x1 = iris, x2 = mtcars)
#'
teal_reportable <- function(...,
                            report = report_document(),
                            code = character(0),
                            join_keys = teal.data::join_keys()) {
  methods::new(
    "teal_reportable",
    .xData = list2env(list(...)),
    report = report,
    join_keys = join_keys,
    code = code
  )
}

#' @export
as.reportable <- function(x) {
  checkmate::assert_class(x, "qenv")
  if (inherits(x, "teal_reportable")) {
    return(x)
  }
  new_x <- teal_reportable()
  for (slot_name in slotNames(x)) {
    slot(new_x, slot_name) <- slot(x, slot_name)
  }
  report(new_x) <- c(
    report(new_x),
    code_chunk(teal.code::get_code(new_x))
  )

  new_x
}
