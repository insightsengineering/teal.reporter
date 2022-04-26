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
      private$deparsers <- list()
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
    #' @param raw (`logical`) whether to include a `content` as it was added or apply `deparse` functions on metadata.
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_content()
    #'
    get_content = function(raw = FALSE) {
      checkmate::assert_logical(raw)
      if (!raw) {
        lapply(
          seq_along(private$content),
          function(m) {
            if (inherits(private$content[[m]], "ContentBlock")) {
              private$content[[m]]
            } else {
              key <- names(private$content)[m]
              deparse_func <- private$deparsers[[key]]
              TextBlock$new(deparse_func(private$content[[m]]))
            }
          }
        )
      } else {
        private$content
      }
    },
    #' @description Appends content elements of this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value any value a meta data, by default `base::deparse1`.
    #' @param deparse (`function`) to convert a value to a string.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_metadata(key = "meta1", value = list("This is meta data"))
    #'
    #' custom_lm_deparse <- function(x) paste(capture.output(summary(x)), collapse = "\n")
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality),
    #'                   deparse = custom_lm_deparse)
    #'
    #' card$get_content()
    #' card$get_content(raw = TRUE)
    #'
    append_metadata = function(key, value, deparse = deparse1) {
      checkmate::assert_character(key, min.len = 0, max.len = 1)
      checkmate::assert_function(deparse)

      meta_list <- list()
      meta_list[[key]] <- value
      private$content <- append(private$content, meta_list)
      private$deparsers[[key]] <- deparse
      invisible(self)
    },
    #' @description get all deparse functions of this `ReportCard`.
    #' @return `named list`
    get_deparsers = function() {
      private$deparsers
    }
  ),
  private = list(
    content = list(),
    deparsers = list(),

    #' @description The copy constructor.
    #'
    #' @param name the name of the field
    #' @param value the value of the field
    #' @return the new value of the field
    #'
    deep_clone = function(name, value) {
      if (name == "content") {
        lapply(value, function(content_block) {
          if (inherits(content_block, "ContentBlock")) content_block$clone(deep = TRUE) else content_block
        })
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
