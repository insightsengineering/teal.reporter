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
    #' @description Returns the content of this `TealReportCard`.
    #'
    #' @param include_metadata (`logical`) whether to include render `content` alone or with `metadata`
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock` and `metadata`
    #' @examples
    #' card <- TealReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #' card$get_content()
    #'
    get_content = function(include_metadata = FALSE) {
      checkmate::assert_logical(include_metadata)
      if (include_metadata) {
        text_metadata <- list()
        names_metadata <- names(private$metadata)
        for (x in seq_along(private$metadata)) {
          block <- private$metadata[[x]]
          text_metadata <- append(text_metadata, TextBlock$new(names_metadata[x], style = "header3"))
          if (inherits(block, "TextBlock")) {
            text_metadata <- append(text_metadata, block)
          } else {
            text_metadata <- append(text_metadata, TextBlock$new(deparse1(block)))
          }
        }
        appended_content <- append(private$content, text_metadata)
        appended_content
      } else {
        private$content
      }
    },
    #' @description Appends the source code to the `metadata` of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) code as text
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   "ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()"
    #' )
    #'
    append_src = function(src) {
      checkmate::assert_character(src, min.len = 0, max.len = 1)
      super$append_metadata("SRC", src)
      invisible(self)
    },
    #' @description Appends the filter state list to the `metadata` of this `TealReportCard`.
    #'
    #' @param fs (`list`) list of filter states.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_fs(
    #'   list(data = list(X = list(selected = c(1, 10))))
    #' )
    #'
    append_fs = function(fs) {
      checkmate::assert_list(fs)
      super$append_metadata("Filter state", fs)
      invisible(self)
    },
    #' @description Appends the encodings list to the `metadata` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list("variable 1 is X"))
    #'
    append_encodings = function(encodings) {
      checkmate::assert_list(encodings)
      super$append_metadata("Encodings", encodings)
      invisible(self)
    }
  ),
  private = list()
)
