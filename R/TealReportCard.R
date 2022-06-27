#' @title `TealReportCard`
#' @description `r lifecycle::badge("experimental")`
#' A child of [`ReportCard`] that is used for teal specific applications.
#' In addition to the parent methods, it supports rendering teal specific elements such as
#' the source code, the encodings panel content and the filter panel content as part of the
#' meta data.
#' @export
#'
TealReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "TealReportCard",
  inherit = ReportCard,
  public = list(
    #' @description Appends the source code to the `content` meta data of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) code as text.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   "ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()"
    #' )
    #' card$get_content()[[1]]$get_content()
    append_src = function(src) {
      checkmate::assert_character(src, min.len = 0, max.len = 1)
      super$append_text(src, "verbatim")
      super$append_metadata("SRC", src)
      invisible(self)
    },
    #' @description Appends the filter state list to the `content` meta data  of this `TealReportCard`.
    #'
    #' @param fs (`FilteredData`) with filter states.
    #' @return invisibly self
    #' @examples
    #' # Artificial FilteredData class
    #' fs <- R6::R6Class("FilteredData",
    #'   public = list(
    #'     get_filter_state = function() list(a = 1, b = 3),
    #'     get_formatted_filter_state = function() "a = 1 and b = 3"
    #'   )
    #' )
    #' fs_inst <- fs$new()
    #' card <- TealReportCard$new()$append_fs(fs_inst)
    #' card$get_content()[[1]]$get_content()
    #'
    append_fs = function(fs) {
      checkmate::assert_class(fs, "FilteredData")
      super$append_text(fs$get_formatted_filter_state(), "verbatim")
      super$append_metadata("FS", fs$get_filter_state())
      invisible(self)
    },
    #' @description Appends the encodings list to the `content` meta data of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list("variable 1 is X"))
    #' card$get_content()[[1]]$get_content()
    #'
    append_encodings = function(encodings) {
      checkmate::assert_list(encodings)
      super$append_text(yaml::as.yaml(encodings), "verbatim")
      super$append_metadata("Encodings", encodings)
      invisible(self)
    }
  ),
  private = list()
)
