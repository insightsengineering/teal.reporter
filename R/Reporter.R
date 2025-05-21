#' @title `Reporter`: An `R6` class for managing reports
#' @docType class
#' @description `r lifecycle::badge("experimental")`
#'
#' This `R6` class is designed to store and manage reports,
#' facilitating the creation, manipulation, and serialization of report-related data.
#' It supports both `ReportCard` (`r lifecycle::badge("deprecated")`) and `ReportDocument` objects, allowing flexibility
#' in the types of reports that can be stored and managed.
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
      private$reactive_add_card <- shiny::reactiveVal(NULL)
      invisible(self)
    },

    #' @description Append one or more `ReportCard` or `ReportDocument` objects to the `Reporter`.
    #'
    #' @param cards (`ReportCard` or `ReportDocument`) or a list of such objects
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' doc1 <- ReportCard$new()
    #' doc1$append_text("Document introduction")
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, doc1))
    append_cards = function(cards) {
      if (checkmate::test_multi_class(cards, classes = c("ReportDocument", "ReportCard"))) {
        cards <- list(cards)
      }

      checkmate::assert_list(cards, types = c("ReportCard", "ReportDocument"))
      lapply(cards, function(x) checkmate::assert(private$check_append(x)))
      new_cards <- cards

      rds <- vapply(new_cards, inherits, logical(1L), "ReportDocument")
      if (!is.null(self$get_template())) {
        new_cards[rds] <- lapply(new_cards[rds], self$get_template())
      }

      new_cards <- lapply(new_cards, private$update_attributes)

      # Set up unique id for each card
      names(new_cards) <- vapply(new_cards, metadata, character(1L), which = "id")
      private$cards <- append(private$cards, new_cards)
      shiny::isolate(private$trigger_add_card())
      invisible(self)
    },
    #' @description Reorders `ReportCard` or `ReportDocument` objects in `Reporter`.
    #' @param new_order `character` vector with names of `ReportCard` or `ReportDocument` objects to be set in this order.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #' card1$set_name('Card1')
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$set_name('Card2')
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    #' names(reporter$get_cards())
    #' reporter$reorder_cards(c("Card2", "Card1"))
    #' names(reporter$get_cards())
    reorder_cards = function(new_order) {
      private$cards <- stats::setNames(
        lapply(new_order, function(name) {
          if (inherits(private$cards[[name]], "ReportDocument")) {
            private$cards[[name]]
          } else {
            private$cards[[name]]$clone(deep = TRUE)
          }
        }),
        new_order
      )
      invisible(self)
    },
    #' @description Sets `ReportCard` or `ReportDocument` content.
    #' @param old_title Title of the `ReportCard` or `ReportDocument` to be replaced.
    #' @param card The new object (`ReportCard` or `ReportDocument`) to replace the existing one.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #' card1$set_name('Card1')
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$set_name('Card2')
    #'
    #' reporter$replace_card("Card1", card2)
    #' reporter$get_cards()[[1]]$get_name()
    replace_card = function(card) {
      checkmate::assert(private$check_append(card))
      private$cards[[metadata(card, "id")]] <- card
      private$trigger_add_card()
      invisible(self)
    },
    #' @description Retrieves all `ReportCard` and `ReportDocument` objects contained in `Reporter`.
    #' @return A (`list`) of [`ReportCard`] and [`ReportDocument`] objects.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_cards()
    get_cards = function(index = NULL) {
      if (is.null(index)) {
        private$cards
      } else {
        private$cards[index]
      }
    },
    #' @description Compiles and returns all content blocks from the `ReportCard` and `ReportDocument` objects in the `Reporter`.
    #' @param sep An optional separator to insert between each content block.
    #' Default is a `NewpageBlock$new()` object.
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock`, `NewpageBlock`, and raw `ReportDocument` content
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text")
    #' card1$append_plot(
    #'  ggplot(iris, aes(x = Petal.Length)) + geom_histogram()
    #' )
    #'
    #' card2 <- ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text")
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_blocks()
    #'
    get_blocks = function(sep = NewpageBlock$new()) {
      blocks <- list()
      if (length(private$cards) > 0) {
        for (card_idx in head(seq_along(private$cards), -1)) {
          if (inherits(private$cards[[card_idx]], "ReportCard")) {
            blocks <- append(blocks, append(private$cards[[card_idx]]$get_content(), sep))
          } else if (inherits(private$cards[[card_idx]], "ReportDocument")) {
            blocks <- append(blocks, append(private$cards[[card_idx]], "## NewPageSep ---")) # TODO - figure out if this is useful sep
          }
        }
        ncards <- length(private$cards)
        if (inherits(private$cards[[ncards]], "ReportCard")) {
          blocks <- append(blocks, private$cards[[ncards]]$get_content())
        } else if (inherits(private$cards[[ncards]], "ReportDocument")) {
          blocks <- append(blocks, private$cards[[ncards]])
        }
      }
      blocks
    },
    #' @description Resets the `Reporter`, removing all `ReportCard` and `ReportDocument` objects and metadata.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      private$cards <- list()
      private$metadata <- list()
      private$reactive_add_card(NULL)
      invisible(self)
    },
    #' @description Removes specific `ReportCard` or `ReportDocument` objects from the `Reporter` by their indices.
    #'
    #' @param ids (`integer`, `character`) the indexes of cards (either name)
    #' @return `self`, invisibly.
    remove_cards = function(ids = NULL) {
      checkmate::assert(
        checkmate::check_null(ids),
        checkmate::check_integer(ids, min.len = 1, max.len = length(private$cards)),
        checkmate::check_character(ids, min.len = 1, max.len = length(private$cards))
      )
      if (is.null(ids)) {
        return(invisible(self))
      }

      if (is.character(ids)) {
        ids <- which(names(private$cards) %in% ids)
      }
      private$cards <- private$cards[-ids]
      private$trigger_add_card()
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
    get_reactive_add_card = function() private$reactive_add_card(),
    #' @description Get the metadata associated with this `Reporter`.
    #'
    #' @return `named list` of metadata to be appended.
    #' @examples
    #' reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    #' reporter$get_metadata()
    #'
    get_metadata = function() private$metadata,
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
    #' @return invisibly self
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
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "testdir")
    #' dir.create(tmp_dir)
    #' reporter$to_list(tmp_dir)
    to_list = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      rlist <- list(name = "teal Reporter", version = "1", id = self$get_id(), cards = list())
      rlist[["metadata"]] <- self$get_metadata()
      cards <- self$get_cards()
      for (i in seq_along(cards)) {
        # we want to have list names being a class names to indicate the class for $from_list
        card_class <- class(cards[[i]])[1]
        u_card <- list()
        if (card_class == "ReportDocument") {
          tmp <- tempfile(fileext = ".rds")
          suppressWarnings(saveRDS(cards[[i]], file = tmp))
          tmp_base <- basename(tmp)
          file.copy(tmp, file.path(output_dir, tmp_base))
          u_card[[card_class]] <- list(name = names(cards)[i], path = tmp_base)
        } else {
          u_card[[card_class]] <- cards[[i]]$to_list(output_dir)
        }
        rlist$cards <- c(rlist$cards, u_card)
      }
      rlist
    },
    #' @description Reinitializes a `Reporter` from a list representation and associated files in a specified directory.
    #' @param rlist (`named list`) representing a `Reporter` instance.
    #' @param output_dir (`character(1)`) a path to the directory from which files will be copied.
    #' @return `self`, invisibly.
    #' @note if Report has an id when converting to JSON then It will be compared to the currently available one.
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "testdir")
    #' unlink(tmp_dir, recursive = TRUE)
    #' dir.create(tmp_dir)
    #' reporter$from_list(reporter$to_list(tmp_dir), tmp_dir)
    from_list = function(rlist, output_dir) {
      id <- self$get_id()
      checkmate::assert_list(rlist)
      checkmate::assert_directory_exists(output_dir)
      stopifnot("Report JSON has to have name slot equal to teal Reporter" = rlist$name == "teal Reporter")
      stopifnot("Loaded Report id has to match the current instance one" = rlist$id == id)
      if (rlist$version %in% c("1")) {
        new_cards <- list()
        cards_names <- names(rlist$cards)
        cards_names <- gsub("[.][0-9]*$", "", cards_names)
        for (iter_c in seq_along(rlist$cards)) {
          card_class <- cards_names[iter_c]
          card <- rlist$cards[[iter_c]]
          if (card_class == "ReportDocument") {
            new_card <- readRDS(file.path(output_dir, card$path))
            class(new_card) <- "ReportDocument"
            new_card <- list(new_card) # so that it doesn't loose class and can be used in self$append_cards
            names(new_card) <- card$name
          } else {
            new_card <- eval(str2lang(card_class))$new()
            new_card$from_list(card, output_dir)
          }
          new_cards <- c(new_cards, new_card)
        }
      } else {
        stop(
          sprintf(
            "The provided %s reporter version is not supported.",
            rlist$version
          )
        )
      }
      self$reset()
      self$set_id(rlist$id)
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
      cat(
        jsonlite::toJSON(json, auto_unbox = TRUE, force = TRUE),
        file = file.path(output_dir, "Report.json")
      )
      output_dir
    },
    #' @description Reinitializes a `Reporter` from a `JSON ` file and files in a specified directory.
    #' @param output_dir (`character(1)`) a path to the directory with files, `JSON` and statics.
    #' @return `self`, invisibly.
    #' @note if Report has an id when converting to JSON then It will be compared to the currently available one.
    #' @examples
    #' reporter <- Reporter$new()
    #' tmp_dir <- file.path(tempdir(), "jsondir")
    #' dir.create(tmp_dir)
    #' unlink(list.files(tmp_dir, recursive = TRUE))
    #' reporter$to_jsondir(tmp_dir)
    #' reporter$from_jsondir(tmp_dir)
    from_jsondir = function(output_dir) {
      checkmate::assert_directory_exists(output_dir)
      dir_files <- list.files(output_dir)
      stopifnot("There has to be at least one file in the loaded directory" = length(dir_files) > 0)
      stopifnot("Report.json file has to be in the loaded directory" = "Report.json" %in% basename(dir_files))
      json <- jsonlite::read_json(file.path(output_dir, "Report.json"))
      self$reset()
      self$from_list(json, output_dir)
      invisible(self)
    },
    #' @description Set the `Reporter` id
    #' Optionally add id to a `Reporter` which will be compared when it is rebuilt from a list.
    #' The id is added to the downloaded file name.
    #' @param id (`character(1)`) a Report id.
    #' @return `self`, invisibly.
    set_id = function(id) {
      private$id <- id
      invisible(self)
    },
    #' @description Get the `Reporter` id
    #' @return `character(1)` the `Reporter` id.
    get_id = function() private$id,
    #' @description Set template function for `ReportDocument`
    #' Set a function that is called on every report content (of class `ReportDocument`) added through `$append_cards`
    #' @param template (`function`) a template function.
    #' @return `self`, invisibly.
    #' @examples
    #'
    #' reporter <- teal.reporter::Reporter$new()
    #' template_fun <- function(document) {
    #'   disclaimer <- teal.reporter::report_document("Here comes disclaimer text")
    #'   c(disclaimer, document)
    #' }
    #' reporter$set_template(template_fun)
    #' doc1 <- teal.reporter::report_document("## Header 2 text", "Regular text")
    #' ndoc1 <- stats::setNames(list(doc1), "Welcome card")
    #' reporter$append_cards(ndoc1)
    #' reporter$get_cards()
    set_template = function(template) {
      private$template <- template
      invisible(self)
    },
    #' @description Get the `Reporter` template
    #' @return a template `function`.
    get_template = function() private$template
  ),
  private = list(
    id = "",
    cards = list(),
    metadata = list(),
    reactive_add_card = NULL,
    trigger_add_card = function() {
      new_value <- if (is.null(private$reactive_add_card)) {
        private$reactive_add_card <- reactiveVal(NULL)
        1
      } else if (is.null(private$reactive_add_card())) {
        1
      } else {
        private$reactive_add_card() + 1
      }
      private$reactive_add_card(new_value)
    },
    template = NULL,
    # @description Update the attributes of a card and generates unique hash
    # @param card the card to be updated
    # @param label the label to be set
    update_attributes = function(card) {
      metadata(card, "id") <- sprintf("card_%s", substr(rlang::hash(list(card, Sys.time())), 1, 8))
      card
    },
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
    },

    # @description Check if a card can be appended to the reporter.
    #
    # @param card (`ReportDocument`) title of new card.
    # @return `TRUE` if card can be safely added, otherwise, `FALSE`.
    check_append = function(card) {
      card_id <- metadata(card, "id")
      ix <- if (length(card_id) == 0L) rep(TRUE, length(private$cards)) else names(private$cards) != metadata(card, "id")
      (!metadata(card, "title") %in% vapply(private$cards[ix], metadata, character(1L), which = "title")) || return("Card with this name already exists")
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
