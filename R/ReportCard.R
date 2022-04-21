#' @title `ReportCard`
#' @export
#'
ReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "ReportCard",
  public = list(
    #' @description Returns a `ReportCard` object.
    #'
    #' @return a `ReportCard` object
    #' @examples
    #' card <- ReportCard$new()
    #'
    initialize = function() {
      private$content <- list()
      private$meta_data <- list()
      invisible(self)
    },
    #' @description Appends a table to this `ReportCard`.
    #'
    #' @param table the appended table
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_table(iris)
    #'
    append_table = function(table) {
      private$content <- append(private$content, TableBlock$new(table))
      invisible(self)
    },
    #' @description Appends a plot to this `ReportCard`.
    #'
    #' @param plot the appended plot
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    append_plot = function(plot) {
      private$content <- append(private$content, PictureBlock$new(plot))
      invisible(self)
    },
    #' @description Appends a paragraph of text to this `ReportCard`.
    #'
    #' @param text (`character(0)` or `character(1)`) the text
    #' @param style (`character(1)`) the style of the paragraph. One of: `default`, `header`, `verbatim`
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_text("A paragraph of default text")
    #'
    append_text = function(text, style = TextBlock$new()$get_available_styles()[1]) {
      private$content <- append(private$content, TextBlock$new(text, style))
      invisible(self)
    },
    #' @description Returns the content of this `ReportCard`.
    #'
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #' card$get_content()
    #'
    get_content = function() {
      private$content
    },
    #' @description Appends meta data elements to `meta_data` of this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value (`list`) content of meta data.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_meta_data(list("This is meta data"))
    #'
    append_meta_data = function(key, value) {
      private$meta_data[[key]] <- value
      invisible(self)
    },
    #' @description Returns the `meta_data` of this `ReportCard`.
    #'
    #' @return `list()` named list of `meta_data`
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #' card$get_meta_data()
    #'
    get_meta_data = function() {
      private$meta_data
    },
    #' @description The copy constructor.
    #'
    #' @param name the name of the field
    #' @param value the value of the field
    #' @return the new value of the field
    #'
    deep_clone = function(name, value) {
      if (name == "content") {
        lapply(value, function(content_block) content_block$clone(deep = TRUE))
      } else {
        value
      }
    }
  ),
  private = list(
    content = list(),
    meta_data = list()
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
