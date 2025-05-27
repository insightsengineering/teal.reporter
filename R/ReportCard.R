#' @title `ReportCard`: An `R6` class for building report elements
#' @docType class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This `R6` class that supports creating a report card containing text, plot, table and
#' metadata blocks that can be appended and rendered to form a report output from a `shiny` app.
#'
#' For more information about the various blocks, refer to the vignette:
#' `vignette("teal-reporter-blocks-overview", "teal.reporter")`.
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
      private$content <- list()
      private$metadata <- list()
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
    append_table = function(table) {
      self$append_content(TableBlock$new(table))
      invisible(self)
    },
    #' @description Appends a html content to this `ReportCard`.
    #'
    #' @param content An object that can be rendered as a HTML content.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_html(shiny::div("HTML Content"))
    #'
    append_html = function(content) {
      self$append_content(HTMLBlock$new(content))
      invisible(self)
    },
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
      pb <- PictureBlock$new()
      if (!is.null(dim) && length(dim) == 2) {
        pb$set_dim(dim)
      }
      pb$set_content(plot)
      self$append_content(pb)
      invisible(self)
    },
    #' @description Appends a text paragraph to this `ReportCard`.
    #'
    #' @param text (`character`) The text content to add.
    #' @param style (`character(1)`) the style of the paragraph. One of: `r TextBlock$new()$get_available_styles()`.
    #' @return `self`, invisibly.
    #' @examples
    #' card <- ReportCard$new()$append_text("A paragraph of default text")
    #'
    append_text = function(text, style = TextBlock$new()$get_available_styles()[1]) {
      self$append_content(TextBlock$new(text, style))
      invisible(self)
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
      self$append_content(RcodeBlock$new(text, ...))
      invisible(self)
    },
    #' @description Appends a generic `ContentBlock` to this `ReportCard`.
    #'
    #' @param content (`ContentBlock`) object.
    #' @return `self`, invisibly.
    #' @examples
    #' NewpageBlock <- getFromNamespace("NewpageBlock", "teal.reporter")
    #' card <- ReportCard$new()$append_content(NewpageBlock$new())
    #'
    append_content = function(content) {
      checkmate::assert_class(content, "ContentBlock")
      private$content <- append(private$content, content)
      invisible(self)
    },
    #' @description Get all content blocks from this `ReportCard`.
    #'
    #' @return `list()` list of `TableBlock`, `TextBlock` and `PictureBlock`.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_metadata("rc", "a <- 2 + 2")
    #'
    #' card$get_content()
    #'
    #'
    get_content = function() {
      private$content
    },
    #' @description Clears all content and metadata from `ReportCard`.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      private$content <- list()
      private$metadata <- list()
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
      private$metadata
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
      checkmate::assert_false(key %in% names(private$metadata))
      meta_list <- list()
      meta_list[[key]] <- value
      private$metadata <- append(private$metadata, meta_list)
      invisible(self)
    },
    #' @description Get the name of the `ReportCard`.
    #'
    #' @return `character` a card name.
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    get_name = function() {
      private$name
    },
    #' @description Set the name of the `ReportCard`.
    #'
    #' @param name (`character(1)`) a card name.
    #' @return `self`, invisibly.
    #' @examples
    #' ReportCard$new()$set_name("NAME")$get_name()
    set_name = function(name) {
      checkmate::assert_character(name)
      private$name <- name
      invisible(self)
    },
    #' @description Set content block names for compatibility with newer `doc`
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
    to_list = function(output_dir) {
      new_blocks <- list()
      for (block in self$get_content()) {
        block_class <- class(block)[1]
        formal_args <- formalArgs(block$to_list)
        cblock <- if ("output_dir" %in% formal_args) {
          block$to_list(output_dir)
        } else {
          block$to_list()
        }
        new_block <- list()
        new_block[[block_class]] <- cblock
        new_blocks <- c(new_blocks, new_block)
      }
      new_card <- list()
      new_card[["blocks"]] <- new_blocks
      new_card[["metadata"]] <- self$get_metadata()
      new_card[["name"]] <- self$get_name()
      new_card
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
    from_list = function(card, output_dir) {
      self$reset()
      blocks <- card$blocks
      metadata <- card$metadata
      name <- card$name
      if (length(name) == 0) name <- character(0)
      blocks_names <- names(blocks)
      blocks_names <- gsub("[.][0-9]*$", "", blocks_names)
      for (iter_b in seq_along(blocks)) {
        block_class <- blocks_names[iter_b]
        block <- blocks[[iter_b]]
        instance <- private$dispatch_block(block_class)
        formal_args <- formalArgs(instance$new()$from_list)
        cblock <- if (all(c("x", "output_dir") %in% formal_args)) {
          instance$new()$from_list(block, output_dir)
        } else if ("x" %in% formal_args) {
          instance$new()$from_list(block)
        } else {
          instance$new()$from_list()
        }
        self$append_content(cblock)
      }
      for (meta in names(metadata)) {
        self$append_metadata(meta, metadata[[meta]])
      }
      self$set_name(name)
      invisible(self)
    }
  ),
  private = list(
    content = list(),
    metadata = list(),
    name = character(0L),
    id = character(0L),
    dispatch_block = function(block_class) {
      eval(str2lang(block_class))
    },
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

#' @export
length.ReportCard <- function(x) {
  length(x$get_content())
}
