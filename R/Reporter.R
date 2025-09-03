#' @title `Reporter`: An `R6` class for managing reports
#' @docType class
#' @description
#'
#' This `R6` class is designed to store and manage reports,
#' facilitating the creation, manipulation, and serialization of report-related data.
#' It supports both `ReportCard` and `teal_card` objects, allowing flexibility
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
      private$cached_html <- shiny::reactiveValues()
      private$trigger_reactive <- shiny::reactiveVal(NULL)
      invisible(self)
    },

    #' @description Append one or more `ReportCard` or `teal_card` objects to the `Reporter`.
    #'
    #' @param cards (`ReportCard` or `teal_card`) or a list of such objects
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #'
    #' card1 <- teal_card("## Header 2 text", "A paragraph of default text")
    #' card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
    #' metadata(card1, "title") <- "Card1"
    #'
    #' card2 <- teal_card("Document introduction")
    #' metadata(card2, "title") <- "Card2"
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    append_cards = function(cards) {
      if (checkmate::test_multi_class(cards, classes = c("teal_card", "ReportCard"))) {
        cards <- list(cards)
      }

      checkmate::assert_list(cards, types = c("ReportCard", "teal_card"))
      new_cards <- lapply(cards, function(x) if (inherits(x, "teal_card")) x else x$get_content())

      if (!is.null(self$get_template())) {
        new_cards <- lapply(new_cards, self$get_template())
      }

      # Set up unique id for each card
      names(new_cards) <- vapply(new_cards, function(card) {
        sprintf("card_%s", substr(rlang::hash(list(deparse1(card), Sys.time())), 1, 8))
      }, character(1L))

      for (card_id in names(new_cards)) {
        private$cards[[card_id]] <- new_cards[[card_id]]
        private$cached_html[[card_id]] <- lapply(new_cards[[card_id]], tools::toHTML)
      }
      invisible(self)
    },
    #' @description Reorders `teal_card` objects in `Reporter`.
    #' @param new_order `character` vector with names of `teal_card` objects to
    #' be set in this order.
    #' @description Reorders `teal_card` objects in `Reporter`.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- teal_card("## Header 2 text", "A paragraph of default text")
    #' card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
    #' metadata(card1, "title") <- "Card1"
    #'
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2 <- teal_card(
    #'   "## Header 2 text",
    #'   "A paragraph of default text",
    #'   table_res2
    #' )
    #' metadata(card2, "title") <- "Card2"
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
    #' @description Sets `ReportCard` or `teal_card` content.
    #' @param card_id (`character(1)`) the unique id of the card to be replaced.
    #' @param card The new object (`ReportCard` or `teal_card`) to replace the existing one.
    #' @return `self`, invisibly.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- teal_card("## Header 2 text", "A paragraph of default text")
    #' card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
    #' metadata(card1, "title") <- "Card1"
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1))
    #'
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2 <- teal_card(
    #'   "## Header 2 text",
    #'   "A paragraph of default text",
    #'   table_res2
    #' )
    #' metadata(card2, "title") <- "Card2"
    #'
    #' reporter$replace_card(card2, "Card1")
    #' reporter$get_cards()[[1]]$get_name()
    replace_card = function(card, card_id) {
      if (inherits(card, "ReportCard")) {
        card <- card$get_content()
      }
      private$cards[[card_id]] <- card
      private$cached_html[[card_id]] <- lapply(card, tools::toHTML)
      invisible(self)
    },
    #' @description Retrieves all `teal_card` objects contained in `Reporter`.
    #' @return A (`list`) of [`teal_card`] objects.
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- teal_card("## Header 2 text", "A paragraph of default text")
    #' card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
    #'
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2 <- teal_card(
    #'   "## Header 2 text",
    #'   "A paragraph of default text",
    #'   table_res2
    #' )
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
    #' @description Compiles and returns all content blocks from the `teal_card`
    #'  objects in the `Reporter`.
    #' @param sep An optional separator to insert between each content block.
    #' Default is a `\n\\newpage\n` markdown.
    #' @return `list()` of `teal_card`
    #' @examplesIf require("ggplot2")
    #' library(ggplot2)
    #' library(rtables)
    #'
    #' card1 <- teal_card("## Header 2 text", "A paragraph of default text")
    #' card1 <- c(card1, ggplot(iris, aes(x = Petal.Length)) + geom_histogram())
    #'
    #' lyt <- analyze(split_rows_by(basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- build_table(lyt, airquality)
    #' card2 <- teal_card(
    #'   "## Header 2 text",
    #'   "A paragraph of default text",
    #'   table_res2
    #' )
    #'
    #' reporter <- Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_blocks()
    get_blocks = function(sep = "\\newpage") {
      cards <- self$get_cards()
      blocks <- teal_card()
      for (idx in seq_along(cards)) {
        card <- cards[[idx]]
        title <- trimws(metadata(card, "title"))
        metadata(card)$title <- NULL
        card_title <- if (length(title) > 0 && nzchar(title)) {
          sprintf("# %s", title)
        } else {
          sprintf("# _Unnamed Card (%d)_", idx)
        }
        blocks <- c(blocks, as.teal_card(card_title), card)
        if (idx != length(cards) && length(sep)) blocks <- c(blocks, trimws(sep))
      }
      blocks
    },
    #' @description Resets the `Reporter`, removing all `teal_card` objects and metadata.
    #'
    #' @return `self`, invisibly.
    #'
    reset = function() {
      if (shiny::isRunning()) {
        for (card_id in names(private$cards)) private$cards[[card_id]] <- NULL
      } else {
        private$cards <- shiny::reactiveValues()
      }
      private$override_order <- character(0L)
      private$metadata <- list()
      invisible(self)
    },
    #' @description Removes specific `teal_card` objects from the `Reporter` by their indices.
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
      lifecycle::deprecate_warn("0.5.0.9000", "Reporter$from_reporter()")
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
        card <- cards[[i]]
        card_class <- class(card)[1]
        u_card <- list()
        tmp <- tempfile(fileext = ".rds")
        suppressWarnings(saveRDS(card, file = tmp))
        tmp_base <- basename(tmp)
        file.copy(tmp, file.path(output_dir, tmp_base))
        u_card[[card_class]] <- list(name = names(cards)[i], path = tmp_base)
        rlist$cards <- c(rlist$cards, u_card)
      }
      rlist
    },
    #' @description Extracts and saves all figure elements from the `teal_card` objects in the `Reporter` to a specified directory.
    #' @param output_dir (`character(1)`) a path to the directory where figures will be saved.
    #' @param sub_directory (`character(1)`) a sub-directory within `output_dir` to save figures.
    write_figures = function(output_dir, sub_directory = "figures") {
      figures_dir <- file.path(output_dir, sub_directory)
      dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
      cards <- self$get_cards()
      for (card_id in names(cards)) {
        card <- cards[[card_id]]
        cached_html <- self$get_cached_html(card_id)
        for (element_ix in seq_along(card)) {
          card_element <- card[[element_ix]]
          if (
            inherits(card_element, "chunk_output") &&
              checkmate::test_multi_class(card_element[[1]], classes = c("recordedplot", "ggplot", "grob", "trellis", "gg", "Heatmap"))
          ) {
            base64_image <- cached_html[[names(card)[[element_ix]]]]
            if ( # Ensure we only save valid base64 images
              !is.null(base64_image) && inherits(base64_image, "shiny.tag") && identical(base64_image$name, "img") &&
                !is.null(base64_image$attribs) && grepl("^data:image/[^;]+;base64,", base64_image$attribs$src)
            ) {
              b64 <- sub("^data:image/[^;]+;base64,", "", base64_image$attribs$src)
              writeBin(jsonlite::base64_dec(b64), file.path(figures_dir, sprintf("card_%s_%d.png", card_id, element_ix)))
            }
          }
        }
      }
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
          if (card_class == "teal_card") {
            new_card <- readRDS(file.path(output_dir, card$path))
            class(new_card) <- "teal_card"
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
    ## TODO: averissimo consider alternatives to trigger the re-render of modal
    #' @description Trigger report rendering of preview modal in shiny context.
    #' @param  val value to the passed to the reactive trigger.
    #' @return `reactiveVal` value
    reactive_trigger = function(val) {
      if (missing(val)) {
        private$trigger_reactive()
      } else {
        private$trigger_reactive(val)
      }
    },
    #' @description Get cached HTML for a specific `teal_card` by its id.
    #' @param card_id (`character(1)`) the unique id of the card.
    get_cached_html = function(card_id) {
      if (shiny::isRunning()) {
        private$cached_html[[card_id]]
      } else {
        shiny::isolate(private$cached_html[[card_id]])
      }
    },
    #' @description Get the `Reporter` id
    #' @return `character(1)` the `Reporter` id.
    get_id = function() private$id,
    #' @description Set template function for `teal_card`
    #' Set a function that is called on every report content (of class `teal_card`) added through `$append_cards`
    #' @param template (`function`) a template function.
    #' @return `self`, invisibly.
    #' @examples
    #'
    #' reporter <- teal.reporter::Reporter$new()
    #' template_fun <- function(document) {
    #'   disclaimer <- teal.reporter::teal_card("Here comes disclaimer text")
    #'   c(disclaimer, document)
    #' }
    #' reporter$set_template(template_fun)
    #' doc1 <- teal.reporter::teal_card("## Header 2 text", "Regular text")
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
    cached_html = NULL, # reactiveValues
    trigger_reactive = NULL, # reactiveVal to trigger reactive contexts
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

#' @export
length.Reporter <- function(x) length(x$get_cards())
