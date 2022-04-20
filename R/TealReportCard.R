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
      text_meta_data <- sapply(private$meta_data, function(x) if (inherits(x, "TextBlock")) x else TextBlock$new(deparse1(x)))
      private$content <- append(private$content, text_meta_data)
      private$content
    },
    #' @description Appends the SRC to the `meta_data` of this `TealReportCard`.
    #'
    #' @param src (`character(1)`) src as text.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_src(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    insert_src = function(src) {
      super$insert_meta_data("SRC", TextBlock$new(src))
      invisible(self)
    },
    #' @description Appends the filter state list to the `meta_data` of this `TealReportCard`.
    #'
    #' @param fs (`list`) list of filter states.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_fs(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    insert_fs = function(fs) {
      super$insert_meta_data("Filter state", fs)
      invisible(self)
    },
    #' @description Appends the encodings list to the `meta_data` of this `TealReportCard`.
    #'
    #' @param encodings (`list`) list of encodings selections of the teal app.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_encodings(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    insert_encodings = function(encodings) {
      super$insert_meta_data("Encodings", encodings)
      invisible(self)
    }
  ),
  private = list()
)
