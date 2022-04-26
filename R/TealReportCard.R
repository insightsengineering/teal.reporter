#' @title `TealReportCard`
#' @description A child of `ReportCard` that is used for teal specific applications.
#' In addition to the parent methods, it supports rendering teal specific elements such as
#' the source code, the encodings panel content and the filter panel content as part of the
#' meta data.
#' @export
#'
TealReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "TealReportCard",
  inherit = ReportCard,
  public = list(
    #' @description Appends the source code to the `metadata` of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) code as text
    #' @param deparse (`function`) to convert a value to a string.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   "ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()"
    #' )
    #'
    append_src = function(src, deparse = deparse1) {
      checkmate::assert_character(src, min.len = 0, max.len = 1)
      super$append_metadata("SRC", src, deparse)
      invisible(self)
    },
    #' @description Appends the filter state list to the `metadata` of this `TealReportCard`.
    #'
    #' @param fs (`list`) list of filter states.
    #' @param deparse (`function`) to convert a value to a string.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_fs(
    #'   list(data = list(X = list(selected = c(1, 10))))
    #' )
    #'
    append_fs = function(fs, deparse = deparse1) {
      checkmate::assert_list(fs)
      super$append_metadata("Filter state", fs, deparse)
      invisible(self)
    },
    #' @description Appends the encodings list to the `metadata` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app
    #' @param deparse (`function`) to convert a value to a string.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list("variable 1 is X"))
    #'
    append_encodings = function(encodings, deparse = deparse1) {
      checkmate::assert_list(encodings)
      super$append_metadata("Encodings", encodings, deparse)
      invisible(self)
    }
  ),
  private = list()
)
