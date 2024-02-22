#' @title `Reporter`: An `R6` class for managing report cards
#' @docType class
#' @description `r lifecycle::badge("experimental")`
#'
#' This `R6` class is designed to store and manage report cards,
#' facilitating the creation, manipulation, and serialization of report-related data.
#'
#' @export
#'
Reporter <- R6::R6Class( # nolint: object_name_linter.
  classname = "Reporter",
  public = list(
    #' @description Initialize a `Reporter` object.
    #'
    #' @return Object of class `Reporter`, invisibly.
    #' @examples
    #' reporter <- Reporter$new()
    #'
    initialize = function() {
      private$cards <- list()
      private$reactive_add_card <- shiny::reactiveVal(0)
      invisible(self)
    },
    #' @description Append one or more `ReportCard` objects to the `Reporter`.
    #'
    #' @param cards (`ReportCard`) or a list of such objects
    #' @return `self`, invisibly.
    #' @examples
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    append_cards = function(cards) {
      checkmate::assert_list(cards, "ReportCard")
      private$cards <- append(private$cards, cards)
      private$reactive_add_card(length(private$cards))
      invisible(self)
    },
    #' @description Retrieves all `ReportCard` objects contained in the `Reporter`.
    #'
    #' @return A (`list`) of [`ReportCard`] objects.
    #' @examples
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_cards()
    get_cards = function() {
      private$cards
    },
    #' @description Compiles and returns all content blocks from the [`ReportCard`] in the `Reporter`.
    #'
    #' @param sep An optional separator to insert between each content block.
    #' Default is a `NewpageBlock$new()`object.
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock` and `NewpageBlock`.
    #' @examples
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_blocks()
    #'
    get_blocks = function(sep = NewpageBlock$new()) {
      blocks <- list()
      if (length(private$cards) > 0) {
        for (card_idx in head(seq_along(private$cards), -1)) {
          blocks <- append(blocks, append(private$cards[[card_idx]]$get_content(), sep))
        }
        blocks <- append(blocks, private$cards[[length(private$cards)]]$get_content())
      }
      blocks
    },
    #' @description Resets the `Reporter`, removing all [`ReportCard`] objects and metadata.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      private$cards <- list()
      private$metadata <- list()
      private$reactive_add_card(0)
      invisible(self)
    },
    #' @description Removes specific `ReportCard` objects from the `Reporter` by their indices.
    #'
    #' @param ids (`integer(id)`) the indexes of cards
    #' @return `self`, invisibly.
    remove_cards = function(ids = NULL) {
      checkmate::assert(
        checkmate::check_null(ids),
        checkmate::check_integer(ids, min.len = 1, max.len = length(private$cards))
      )
      if (!is.null(ids)) {
        private$cards <- private$cards[-ids]
      }
      private$reactive_add_card(length(private$cards))
      invisible(self)
    },
    #' @description Swaps the positions of two `ReportCard` objects within the `Reporter`.
    #'
    #' @param start (`integer`) the index of the first card
    #' @param end (`integer`) the index of the second card
    #' @return `self`, invisibly.
    swap_cards = function(start, end) {
      checkmate::assert(
        checkmate::check_integer(start,
          min.len = 1, max.len = 1, lower = 1, upper = length(private$cards)
        ),
        checkmate::check_integer(end,
          min.len = 1, max.len = 1, lower = 1, upper = length(private$cards)
        ),
        combine = "and"
      )
      start_val <- private$cards[[start]]$clone()
      end_val <- private$cards[[end]]$clone()
      private$cards[[start]] <- end_val
      private$cards[[end]] <- start_val
      invisible(self)
    },
    #' @description Gets the current value of the reactive variable for adding cards.
    #'
    #' @return `reactive_add_card` current `numeric` value of the reactive variable.
    #' @note The function has to be used in the shiny reactive context.
    #' @examples
    #' library(shiny)
    #'
    #' isolate(Reporter$new()$get_reactive_add_card())
    get_reactive_add_card = function() {
      private$reactive_add_card()
    },
    #' @description Get the metadata associated with this `Reporter`.
    #'
    #' @return `named list` of metadata to be appended.
    #' @examples
    #' reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    #' reporter$get_metadata()
    #'
    get_metadata = function() {
      private$metadata
    },
    #' @description Appends metadata to this `Reporter`.
    #'
    #' @param meta (`named list`) of metadata to be appended.
    #' @return `self`, invisibly.
    #' @examples
    #' reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    #' reporter$get_metadata()
    #'
    append_metadata = function(meta) {
      checkmate::assert_list(meta, names = "unique")
      checkmate::assert_true(length(meta) == 0 || all(!names(meta) %in% names(private$metadata)))
      private$metadata <- append(private$metadata, meta)
      invisible(self)
    },
    #' @description
    #' Reinitializes a `Reporter` instance by copying the report cards and metadata from another `Reporter`.
    #' @param reporter (`Reporter`) instance to copy from.
    #' @return `self`, invisibly.
    #' @examples
    #' reporter <- Reporter$new()
    #' reporter$from_reporter(reporter)
    from_reporter = function(reporter) {
      checkmate::assert_class(reporter, "Reporter")
      self$reset()
      self$append_cards(reporter$get_cards())
      self$append_metadata(reporter$get_metadata())
      invisible(self)
    },
    #' @description Convert a `Reporter` to a list and transfer any associated files to specified directory.
    #' @param output_dir (`character(1)`) a path to the directory where files will be copied.
    #' @return `named list` representing the `Reporter` instance, including version information,
    #'  metadata, and report cards.
    #'
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "testdir")
    #' dir.create(tmp_dir)
    #' reporter$to_list(tmp_dir)
    to_list = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      rlist <- list(version = "1", cards = list())
      rlist[["metadata"]] <- self$get_metadata()
      for (card in self$get_cards()) {
        # we want to have list names being a class names to indicate the class for $from_list
        card_class <- class(card)[1]
        u_card <- list()
        u_card[[card_class]] <- card$to_list(output_dir)
        rlist$cards <- c(rlist$cards, u_card)
      }
      rlist
    },
    #' @description Reinitializes a `Reporter` from a list representation and associated files in a specified directory.
    #' @param rlist (`named list`) representing a `Reporter` instance.
    #' @param output_dir (`character(1)`) a path to the directory from which files will be copied.
    #' @return `self`, invisibly.
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "testdir")
    #' unlink(tmp_dir, recursive = TRUE)
    #' dir.create(tmp_dir)
    #' reporter$from_list(reporter$to_list(tmp_dir), tmp_dir)
    from_list = function(rlist, output_dir) {
      checkmate::assert_list(rlist)
      checkmate::assert_directory_exists(output_dir)
      if (rlist$version == "1") {
        new_cards <- list()
        cards_names <- names(rlist$cards)
        cards_names <- gsub("[.][0-9]*$", "", cards_names)
        for (iter_c in seq_along(rlist$cards)) {
          card_class <- cards_names[iter_c]
          card <- rlist$cards[[iter_c]]
          new_card <- eval(str2lang(sprintf("%s$new()", card_class)))
          new_card$from_list(card, output_dir)
          new_cards <- c(new_cards, new_card)
        }
      } else {
        stop("The provided version is not supported")
      }
      self$reset()
      self$append_cards(new_cards)
      self$append_metadata(rlist$metadata)
      invisible(self)
    },
    #' @description Serializes the `Reporter` to a `JSON` file and copies any associated files to a specified directory.
    #' @param output_dir (`character(1)`) a path to the directory where files will be copied, `JSON` and statics.
    #' @return `output_dir` argument.
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "jsondir")
    #' dir.create(tmp_dir)
    #' reporter$to_jsondir(tmp_dir)
    to_jsondir = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      json <- self$to_list(output_dir)
      cat(jsonlite::toJSON(json, auto_unbox = TRUE, force = TRUE),
        file = file.path(output_dir, "Report.json")
      )
      output_dir
    },
    #' @description Reinitializes a `Reporter` from a `JSON ` file and files in a specified directory.
    #' @param output_dir (`character(1)`) a path to the directory with files, `JSON` and statics.
    #' @return `self`, invisibly.
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "jsondir")
    #' dir.create(tmp_dir)
    #' unlink(list.files(tmp_dir, recursive = TRUE))
    #' reporter$to_jsondir(tmp_dir)
    #' reporter$from_jsondir(tmp_dir)
    from_jsondir = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      checkmate::assert_true(length(list.files(output_dir)) > 0)
      dir_files <- list.files(output_dir)
      which_json <- grep("json$", dir_files)
      json <- jsonlite::read_json(file.path(output_dir, dir_files[which_json]))
      self$reset()
      self$from_list(json, output_dir)
      invisible(self)
    }
  ),
  private = list(
    cards = list(),
    metadata = list(),
    reactive_add_card = NULL,
    # @description The copy constructor.
    #
    # @param name the name of the field
    # @param value the value of the field
    # @return the new value of the field
    #
    deep_clone = function(name, value) {
      if (name == "cards") {
        lapply(value, function(card) card$clone(deep = TRUE))
      } else {
        value
      }
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
