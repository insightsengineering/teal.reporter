#' @title `TealReportCard`
#' @export
#'
TealReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "TealReportCard",
  inherit = ReportCard,
  public = list(
    #' @description Returns the content of this `TealReportCard`.
    #'
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock` and `meta_data`.
    #' @examples
    #' card <- TealReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #' card$get_content()
    #'
    get_content = function() {
      text_meta_data <- list()
      for (x in seq_along(private$meta_data)) {
        block <- private$meta_data[[x]]
        text_meta_data <- append(text_meta_data, TextBlock$new(names(private$meta_data)[x], style = "header3"))
        if (inherits(block, "TextBlock")) {
          text_meta_data <- append(text_meta_data, block)
        } else {
          text_meta_data <- append(text_meta_data, TextBlock$new(deparse1(block)))
        }
      }


      private$content <- append(private$content, text_meta_data)
      private$content
    },
    #' @description Appends the SRC to the `meta_data` of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) src as text.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_src(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    append_src = function(src) {
      super$append_meta_data("SRC", TextBlock$new(src))
      invisible(self)
    },
    #' @description Appends the filter state list to the `meta_data` of this `TealReportCard`.
    #'
    #' @param fs (`list`) list of filter states.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_fs(
    #'   list(data = list(X = list(selected = c(1, 10))))
    #' )
    #'
    append_fs = function(fs) {
      super$append_meta_data("Filter state", fs)
      invisible(self)
    },
    #' @description Appends the encodings list to the `meta_data` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app.
    #' @return invisibly self
    #' @examples
    #' card <- TealReportCard$new()$append_encodings(list("variable 1 is X"))
    #'
    append_encodings = function(encodings) {
      super$append_meta_data("Encodings", encodings)
      invisible(self)
    }
  ),
  private = list()
)
