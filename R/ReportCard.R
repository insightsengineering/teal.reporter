#' @title `ReportCard`
#' @description `r lifecycle::badge("experimental")`
#' R6 class that supports creating a report card containing text, plot, table and
#' meta data blocks that can be appended and rendered to form a report output from a shiny app.
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
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_metadata()
    #'
    #'
    get_content = function() {
      private$content
    },
    #' @description Returns the metadata of this `ReportCard`.
    #'
    #' @return `named list` list of elements.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_metadata()
    #'
    get_metadata = function() {
      private$metadata
    },
    #' @description Appends content elements to this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value value of meta data.
    #' @param chr_converter (`function`) to convert a value to a character, by default `base::deparse1`.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality))
    #' card$get_content()
    #' card$get_metadata()
    #'
    append_metadata = function(key, value) {
      checkmate::assert_character(key, min.len = 0, max.len = 1)
      checkmate::assert_false(key %in% names(private$metadata))
      meta_list <- list()
      meta_list[[key]] <- value
      private$metadata <- append(private$metadata, meta_list)
      invisible(self)
    },
    #' @description get the Card name
    #'
    #' @return `character` a Card name
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    get_name = function() {
      private$name
    },
    #' @description set the Card name
    #'
    #' @param name `character` a Card name
    #' @return invisibly self
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    set_name = function(name) {
      checkmate::assert_string(name)
      private$name <- name
      invisible(self)
    }
  ),
  private = list(
    content = list(),
    metadata = list(),
    name = character(0),
    # @description The copy constructor.
    #
    # @param name the name of the field
    # @param value the value of the field
    # @return the new value of the field
    #
    deep_clone = function(name, value) {
      if (name == "content") {
        lapply(value, function(content_block) {
          if (inherits(content_block, "R6")) {
            content_block$clone(deep = TRUE)
          } else {
            content_block
          }
        })
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
