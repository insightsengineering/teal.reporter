#' @title `ReportCard`: An `R6` class for building report elements
#' @docType class
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This `R6` class that supports creating a report card containing text, plot, table and
#' metadata blocks that can be appended and rendered to form a report output from a `shiny` app.
#'
#' @section Lifecycle:
#' This class is deprecated. Use `teal_report` class instead for new implementations.
#' See `vignette("teal-report-class", "teal.reporter")` for more information.
#'
#' @export
#'
ReportCard <- R6::R6Class( # nolint: object_name_linter.
  classname = "ReportCard",
  public = list(
    #' @description Initialize a `ReportCard` object.
    #'
    #' @return Object of class `ReportCard`, invisibly.
    #' @examples
    #' card <- ReportCard$new()
    #'
    initialize = function() {
      lifecycle::deprecate_warn(
        when = "0.5.1",
        what = "ReportCard$new()",
        with = "teal_card()",
        details = "Use teal_report class instead. See vignette('teal-report-class', 'teal.reporter') for more information."
      )
      private$content <- teal_card()
      invisible(self)
    },
    #' @description Appends a table to this `ReportCard`.
    #'
    #' @param table A (`data.frame` or `rtables` or `TableTree` or `ElementaryTable` or `listing_df`)
    #' that can be coerced into a table.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_table(iris)
    #'
    append_table = function(table) self$append_content(table),
    #' @description Appends a html content to this `ReportCard`.
    #'
    #' @param content An object that can be rendered as a HTML content.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_html(shiny::div("HTML Content"))
    #'
    append_html = function(content) self$append_content(content),
    #' @description Appends a plot to this `ReportCard`.
    #'
    #' @param plot (`ggplot` or `grob` or `trellis`) plot object.
    #' @param dim (`numeric(2)`) width and height in pixels.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #'
    #' card <- ReportCard$new()$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    append_plot = function(plot, dim = NULL) {
      checkmate::assert_numeric(dim, len = 2, any.missing = FALSE, null.ok = TRUE)
      if (!is.null(dim)) {
        if (!inherits(plot, "chunk_output")) {
          plot <- structure(list(plot), class = c("chunk_output"))
        }
        attr(plot, "dev.width") <- dim[1]
        attr(plot, "dev.height") <- dim[2]
      }
      self$append_content(plot)
    },
    #' @description Appends a text paragraph to this `ReportCard`.
    #'
    #' @param text (`character`) The text content to add.
    #' @param style (`character(1)`) the style of the paragraph.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_text("A paragraph of default text")
    #'
    append_text = function(text, style = c("default", "header2", "header3", "verbatim")) {
      styled <- switch(match.arg(style),
        "default" = text,
        "verbatim" = sprintf("\n```\n%s\n```\n", text),
        "header2" = paste0("## ", text),
        "header3" = paste0("### ", text),
        text
      )
      self$append_content(styled)
    },
    #' @description Appends an `R` code chunk to `ReportCard`.
    #'
    #' @param text (`character`) The `R` code to include.
    #' @param ... Additional  `rmarkdown` parameters for formatting the `R` code chunk.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_rcode("2+2", echo = FALSE)
    #'
    append_rcode = function(text, ...) {
      self$append_content(code_chunk(code = text, ...))
    },
    #' @description Appends a generic content to this `ReportCard`.
    #'
    #' @param content (Object.)
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_content(code_chunk("foo <- 2"))
    #'
    append_content = function(content) {
      private$content <- c(private$content, content)
      invisible(self)
    },
    #' @description Get all content blocks from this `ReportCard`.
    #'
    #' @return `teal_card()` containing appended elements.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_content()
    #'
    #'
    get_content = function() private$content,
    #' @description Clears all content and metadata from `ReportCard`.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      private$content <- teal_card()
      invisible(self)
    },
    #' @description Get the metadata associated with `ReportCard`.
    #'
    #' @return `named list` list of elements.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_metadata()
    #'
    get_metadata = function() {
      metadata(private$content)
    },
    #' @description Appends metadata to this `ReportCard`.
    #'
    #' @param key (`character(1)`) string specifying the metadata key.
    #' @param value value associated with the metadata key.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #'
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality))
    #' card$get_content()
    #' card$get_metadata()
    #'
    append_metadata = function(key, value) {
      checkmate::assert_character(key, min.len = 0, max.len = 1)
      checkmate::assert_false(key %in% names(metadata(private$content)))
      metadata(private$content, key) <- value
      invisible(self)
    },
    #' @description Get the name of the `ReportCard`.
    #'
    #' @return `character` a card name.
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    get_name = function() {
      metadata(private$content, "title") %||% character(0L)
    },
    #' @description Set the name of the `ReportCard`.
    #'
    #' @param name (`character(1)`) a card name.
    #' @return `self`, invisibly.
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    set_name = function(name) {
      metadata(private$content, "title") <- name
      invisible(self)
    },
    #' @description Set content block names for compatibility with newer `teal_card`
    #' @param new_names (`character`) vector of new names.
    set_content_names = function(new_names) {
      names(private$content) <- new_names
    },
    #' @description Convert the `ReportCard` to a list, including content and metadata.
    #' @param output_dir (`character`) with a path to the directory where files will be copied.
    #' @return (`named list`) a `ReportCard` representation.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #'
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality))
    #' card$get_content()
    #'
    #' card$to_list(tempdir())
    #'
    to_list = function(output_dir = lifecycle::deprecated()) {
      if (lifecycle::is_present(output_dir)) {
        lifecycle::deprecate_soft("0.5.0.9000", "ReportCard$to_list(output_dir)")
      }
      private$content
    },
    #' @description Reconstructs the `ReportCard` from a list representation.
    #' @param card (`named list`) a `ReportCard` representation.
    #' @param output_dir (`character`) with a path to the directory where a file will be copied.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #'
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )$append_text("Some text")$append_metadata(key = "lm",
    #'                   value = lm(Ozone ~ Solar.R, airquality))
    #' card$get_content()
    #'
    #' ReportCard$new()$from_list(card$to_list(tempdir()), tempdir())
    #'
    from_list = function(card, output_dir = lifecycle::deprecated()) {
      if (lifecycle::is_present(output_dir)) {
        lifecycle::deprecate_soft("0.5.0.9000", "ReportCard$to_list(output_dir)")
      }
      self$reset()
      private$content <- card
      invisible(self)
    }
  ),
  private = list(
    content = list(),
    name = character(0L),
    id = character(0L),
    # @description The copy constructor.
    #
    # @param name the name of the field
    # @param value the value of the field
    # @return the new value of the field
    #
    deep_clone = function(name, value) {
      if (name == "content") {
        content <- Reduce(
          f = function(result, this) {
            if (inherits(this, "R6")) {
              this <- this$clone(deep = TRUE)
            }
            c(result, this)
          },
          init = teal_card(),
          x = value
        )

        metadata(content) <- metadata(value)
        content
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)

#' @export
length.ReportCard <- function(x) {
  length(x$get_content())
}
