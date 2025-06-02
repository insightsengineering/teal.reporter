#' @title `Reporter`: An `R6` class for managing reports
#' @docType class
#' @description `r lifecycle::badge("experimental")`
#'
#' This `R6` class is designed to store and manage reports,
#' facilitating the creation, manipulation, and serialization of report-related data.
#' It supports both `ReportCard` (`r lifecycle::badge("deprecated")`) and `doc` objects, allowing flexibility
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
      private$cards <- shiny::reactiveValues()
      invisible(self)
    },

    #' @description Append one or more `ReportCard` or `doc` objects to the `Reporter`.
    #'
    #' @param cards (`ReportCard` or `doc`) or a list of such objects
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
      if (checkmate::test_multi_class(cards, classes = c("doc", "ReportCard"))) {
        cards <- list(cards)
      }

      checkmate::assert_list(cards, types = c("ReportCard", "doc"))
      new_cards <- cards

      rds <- vapply(new_cards, inherits, logical(1L), "doc")
      if (!is.null(self$get_template())) {
        new_cards[rds] <- lapply(new_cards[rds], self$get_template())
      }

      # Set up unique id for each card
      names(new_cards) <- vapply(new_cards, function(card) {
        sprintf("card_%s", substr(rlang::hash(list(deparse1(card), Sys.time())), 1, 8))
      }, character(1L))

      for (card_id in names(new_cards)) {
        private$cards[[card_id]] <- new_cards[[card_id]]
      }
      invisible(self)
    },
    #' @description Reorders `ReportCard` or `doc` objects in `Reporter`.
    #' @param new_order `character` vector with names of `ReportCard` or `doc`
    #' objects to be set in this order.
    #' @description Reorders `ReportCard` or `doc` objects in `Reporter`.
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
      private$override_order <- new_order
      invisible(self)
    },
    #' @description Sets `ReportCard` or `doc` content.
    #' @param card_id (`character(1)`) the unique id of the card to be replaced.
    #' @param card The new object (`ReportCard` or `doc`) to replace the existing one.
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
    #'   ggplot(iris, aes(x = Petal.Length)) + geom_histogram(binwidth = 0.2)
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
    #' table_res2 <- build_table(lyt, within(airquality, Day <- factor(Day)))
    #' card2$append_table(table_res2)
    #' card2$set_name('Card2')
    #'
    #' reporter$replace_card(card2, "Card1")
    #' reporter$get_cards()[[1]]$get_name()
    replace_card = function(card, card_id) {
      private$cards[[card_id]] <- card
      invisible(self)
    },
    #' @description Retrieves all `ReportCard` and `doc` objects contained in `Reporter`.
    #' @return A (`list`) of [`ReportCard`] and [`doc`] objects.
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
    get_cards = function() {
      result <- if (shiny::isRunning()) {
        shiny::reactiveValuesToList(private$cards)
      } else {
        shiny::isolate(shiny::reactiveValuesToList(private$cards))
      }
      result <- Filter(Negate(is.null), result) # Exclude all cards that were removed
      # Ensure that cards added after reorder are returned (as well as reordered ones that were removed are excluded)
      result[union(intersect(private$override_order, names(result)), names(result))]
    },
    #' @description Compiles and returns all content blocks from the `ReportCard`
    #' and `doc` objects in the `Reporter`.
    #' @param sep An optional separator to insert between each content block.
    #' Default is a `\n\\newpage\n` markdown.
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock`,
    #' `NewpageBlock`, and raw `doc` content
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
    get_blocks = function(sep = "\\newpage") {
      cards <- self$get_cards()
      blocks <- list()
      for (idx in seq_along(cards)) {
        card <- cards[[idx]]
        if (inherits(card, "ReportCard")) {
          blocks <- append(blocks, card$get_content())
          if (idx != length(cards)) blocks <- append(blocks, sep)
          next # Easier to remove when ReportCard is fully deprecated
        }
        card_with_title <- if (length(metadata(card, "title")) > 0) {
          c(doc(sprintf("# %s", metadata(card, "title"))), card)
        } else {
          card
        }

        blocks <- append(blocks, unclass(card_with_title))
        if (idx != length(cards)) blocks <- append(blocks, trimws(sep))
      }
      blocks
    },
    #' @description Resets the `Reporter`, removing all `ReportCard` and `doc` objects and metadata.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      if (shiny::isRunning()) {
        for (card_id in shiny::names(private$cards)) private$cards[[card_id]] <- NULL
      } else {
        private$cards <- shiny::reactiveValues()
      }
      private$override_order <- character(0L)
      private$metadata <- list()
      invisible(self)
    },
    #' @description Removes specific `ReportCard` or `doc` objects from the `Reporter` by their indices.
    #'
    #' @param ids (`integer`, `character`) the indexes of cards (either name)
    #' @return `self`, invisibly.
    remove_cards = function(ids = NULL) {
      checkmate::assert(
        checkmate::check_null(ids),
        checkmate::check_integer(ids, min.len = 1, max.len = length(private$cards)),
        checkmate::check_character(ids, min.len = 1, max.len = length(private$cards))
      )
      for (card_id in ids) {
        private$cards[[card_id]] <- NULL
      }
      invisible(self)
    },
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
        if (card_class == "doc") {
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
          if (card_class == "doc") {
            new_card <- readRDS(file.path(output_dir, card$path))
            class(new_card) <- "doc"
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
    #' @description Set template function for `doc`
    #' Set a function that is called on every report content (of class `doc`) added through `$append_cards`
    #' @param template (`function`) a template function.
    #' @return `self`, invisibly.
    #' @examples
    #'
    #' reporter <- teal.reporter::Reporter$new()
    #' template_fun <- function(document) {
    #'   disclaimer <- teal.reporter::doc("Here comes disclaimer text")
    #'   c(disclaimer, document)
    #' }
    #' reporter$set_template(template_fun)
    #' doc1 <- teal.reporter::doc("## Header 2 text", "Regular text")
    #' metadata(doc1, "title") <- "Welcome card"
    #' reporter$append_cards(doc1)
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
    cards = NULL, # reactiveValues
    override_order = character(0L), # to sort cards (reactiveValues are not sortable)
    metadata = list(),
    template = NULL,
    # @description The copy constructor.
    #
    # @param name the name of the field
    # @param value the value of the field
    # @return the new value of the field
    #
    deep_clone = function(name, value) {
      shiny::isolate({
        if (name == "cards") {
          new_cards <- lapply(shiny::reactiveValuesToList(value), function(card) {
            if (R6::is.R6(card)) card$clone(deep = TRUE) else card
          })
          do.call(shiny::reactiveValues, new_cards)
        } else {
          value
        }
      })
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
