#' @title `ReportCard`
#' @description R6 class that supports creating a card containing different types of
#' blocks that can be appended and rendered to form a report output.
#' Content and meta data are rendered as separate entities.
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
      private$metadata <- list()
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
    #' @param include_metadata (`logical`) whether to include render `content` alone or with `metadata`
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #' card$get_content()
    #'
    get_content = function(include_metadata = FALSE) {
      checkmate::assert_logical(include_metadata)
      if (include_metadata) {
        append(private$content, private$metadata)
      } else {
        private$content
      }
    },
    #' @description Appends meta data elements to `metadata` of this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value (`list`) content of meta data.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_metadata(key = "meta1", value = list("This is meta data"))
    #'
    append_metadata = function(key, value) {
      checkmate::assert_character(key, min.len = 0, max.len = 1)
      checkmate::assert_multi_class(value, c("character", "list"))
      if (inherits(value, "character")) {
        value <- TextBlock$new(value)
      }
      private$metadata[[key]] <- value
      invisible(self)
    },
    #' @description Returns the `metadata` of this `ReportCard`.
    #'
    #' @return `list()` named list of `metadata`
    #' @examples
    #' card <- ReportCard$new()$append_metadata("meta1", list("This is meta data"))
    #' card$get_metadata()
    #'
    get_metadata = function(key) {
      private$metadata[key]
    }
  ),
  private = list(
    content = list(),
    metadata = list(),

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
  lock_objects = TRUE,
  lock_class = TRUE
)
