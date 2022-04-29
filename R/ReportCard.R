#' @title `ReportCard`
#' @description R6 class that supports creating a report card containing text, plot, table and
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
      private$chr_converters <- list()
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
    #' @param raw (`logical`) whether to include `content` as it was appended or apply `chr_converter`
    #' functions on meta data objects.
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`.
    #' If the `raw` argument is `TRUE`, meta data objects in the form they were added will be returned. Otherwise,
    #' given `chr_converter` function given will be applied on these objects.
    #' Only metadata elements are named.
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
          function(index) {
            if (inherits(private$content[[index]], "ContentBlock")) {
              private$content[[index]]
            } else {
              key <- names(private$content)[index]
              chr_converter_func <- private$chr_converters[[key]]
              TextBlock$new(chr_converter_func(private$content[[index]]))
            }
          }
        )
      } else {
        private$content
      }
    },
    #' @description Appends content elements to this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value value of meta data.
    #' @param chr_converter (`function`) to convert a value to a character, by default `base::deparse1`.
    #' @return invisibly self
    #' @examples
    #' custom_lm_chr_converter <- function(x) paste(capture.output(summary(x)), collapse = "\\n  ")
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality),
    #'                   chr_converter = custom_lm_chr_converter)
    #' card$get_content()
    #' card$get_content(raw = TRUE)
    #'
    append_metadata = function(key, value, chr_converter = deparse1) {
      checkmate::assert_character(key, min.len = 0, max.len = 1)
      checkmate::assert_false(checkmate::test_choice(key, names(private$chr_converters)))
      checkmate::assert_function(chr_converter)
      checkmate::assert_string(chr_converter(value))

      meta_list <- list()
      meta_list[[key]] <- value
      private$content <- append(private$content, meta_list)
      private$chr_converters[[key]] <- chr_converter
      invisible(self)
    },
    #' @description get all `chr_converter` functions of this `ReportCard`.
    #' @return `named list`
    #' @examples
    #' custom_lm_chr_converter <- function(x) paste(capture.output(summary(x)), collapse = "\\n  ")
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality),
    #'                   chr_converter = custom_lm_chr_converter
    #' )$append_metadata(key = "code", value = lm(Ozone ~ Solar.R, airquality))
    #' card$get_chr_converters()
    #'
    get_chr_converters = function() {
      private$chr_converters
    }
  ),
  private = list(
    content = list(),
    chr_converters = list(),

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
