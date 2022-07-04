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
    #' @param dim `integer vector` width and height in pixels.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    append_plot = function(plot, dim = NULL) {
      pb <- PictureBlock$new()
      if (!is.null(dim) && length(dim) == 2) {
        pb$set_dim(dim)
      }
      pb$set_content(plot)
      private$content <- append(private$content, pb)
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
    #' @description Appends a `ContentBlock` to this `ReportCard`.
    #'
    #' @param content (`ContentBlock`)
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_content(teal.reporter:::NewpageBlock$new())
    #'
    append_content = function(content) {
      checkmate::assert_class(content, "ContentBlock")
      private$content <- append(private$content, content)
      invisible(self)
    },
    #' @description Returns the content of this `ReportCard`.
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
    #' @description Removes all objects added to this `ReportCard`.
    #'
    #' @return invisibly self
    #'
    reset = function() {
      private$content <- list()
      private$metadata <- list()
      invisible(self)
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
    #' @description Appends metadata to this `ReportCard`.
    #'
    #' @param key (`character(1)`) name of meta data.
    #' @param value value of meta data.
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
    },
    #' @description Convert the `ReportCard` to a list.
    #' @param output_dir `character` with a path to the directory where files will be copied.
    #' @return `named list` a `ReportCard` representation.
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
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
        cblock <- switch(block_class,
          TextBlock = block$to_list(),
          PictureBlock = block$to_list(output_dir),
          TableBlock = block$to_list(output_dir),
          NewpageBlock = list(),
          NULL
        )
        new_block <- list()
        new_block[[block_class]] <- cblock
        new_blocks <- c(new_blocks, new_block)
      }
      new_card <- list()
      new_card[["blocks"]] <- new_blocks
      new_card[["metadata"]] <- self$get_metadata()
      new_card
    },
    #' @description Create the `ReportCard` from a list.
    #' @param card `named list` a `ReportCard` representation.
    #' @param output_dir `character` with a path to the directory where a file will be copied.
    #' @return invisibly self
    #' @examples
    #' card <- ReportCard$new()$append_text("Some text")$append_plot(
    #'   ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
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
      blocks_names <- names(blocks)
      blocks_names <- gsub("[.][0-9]*$", "", blocks_names)
      for (iter_b in seq_along(blocks)) {
        block_class <- blocks_names[iter_b]
        block <- blocks[[iter_b]]
        cblock <- switch(block_class,
          TextBlock = TextBlock$new()$from_list(block),
          PictureBlock = PictureBlock$new()$from_list(block, output_dir),
          TableBlock = TableBlock$new()$from_list(block, output_dir),
          NewpageBlock = NewpageBlock$new(),
          NULL
        )
        self$append_content(cblock)
      }
      for (meta in names(metadata)) {
        self$append_metadata(meta, metadata[[meta]])
      }
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
