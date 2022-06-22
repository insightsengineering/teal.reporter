#' @title `Reporter`
#' @description `r lifecycle::badge("experimental")`
#' R6 class that stores and manages report cards.
#' @export
#'
Reporter <- R6::R6Class( # nolint: object_name_linter.
  classname = "Reporter",
  public = list(
    #' @description Returns a `Reporter` object.
    #'
    #' @return a `Reporter` object
    #' @examples
    #' reporter <- teal.reporter:::Reporter$new()
    #'
    initialize = function() {
      private$cards <- list()
      private$reactive_add_card <- shiny::reactiveVal(0)
      invisible(self)
    },
    #' @description Appends a table to this `Reporter`.
    #'
    #' @param cards [`ReportCard`] or a list of such objects
    #' @return invisibly self
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #'
    append_cards = function(cards) {
      checkmate::assert_list(cards, "ReportCard")
      private$cards <- append(private$cards, cards)
      private$reactive_add_card(length(private$cards))
      invisible(self)
    },
    #' @description Returns cards of this `Reporter`.
    #'
    #' @return `list()` list of [`ReportCard`]
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
    #' reporter$append_cards(list(card1, card2))
    #' reporter$get_cards()
    get_cards = function() {
      private$cards
    },
    #' @description Returns blocks of all [`ReportCard`] of this `Reporter`.
    #'
    #' @param sep the element inserted between each content element in this `Reporter`.
    #' Pass `NULL` to return content without any additional elements. Default: `NewpageBlock$new()`
    #' @return `list()` list of `TableBlock`, `TextBlock`, `PictureBlock` and `NewpageBlock`
    #' @examples
    #' card1 <- teal.reporter:::ReportCard$new()
    #'
    #' card1$append_text("Header 2 text", "header2")
    #' card1$append_text("A paragraph of default text", "header2")
    #' card1$append_plot(
    #'  ggplot2::ggplot(iris, ggplot2::aes(x = Petal.Length)) + ggplot2::geom_histogram()
    #' )
    #'
    #' card2 <- teal.reporter:::ReportCard$new()
    #'
    #' card2$append_text("Header 2 text", "header2")
    #' card2$append_text("A paragraph of default text", "header2")
    #' lyt <- rtables::analyze(rtables::split_rows_by(rtables::basic_table(), "Day"), "Ozone", afun = mean)
    #' table_res2 <- rtables::build_table(lyt, airquality)
    #' card2$append_table(table_res2)
    #' card2$append_table(iris)
    #'
    #' reporter <- teal.reporter:::Reporter$new()
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
    #' @description Removes all [`ReportCard`] objects added to this `Reporter`.
    #' Additionally all metadata are removed.
    #'
    #' @return invisibly self
    #'
    reset = function() {
      private$cards <- list()
      private$metadata <- list()
      private$reactive_add_card(0)
      invisible(self)
    },
    #' @description remove a specific Card in the Reporter
    #'
    #' @param ids `integer` the indexes of cards
    #' @return invisibly self
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
    #' @description swap two cards in the Reporter
    #'
    #' @param start `integer` the index of the first card
    #' @param end `integer` the index of the second card
    #' @return invisibly self
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
    #' @description get a value for the reactive value for the add card
    #'
    #' @return `reactive_add_card` field value
    #' @note The function has to be used in the shiny reactive context.
    #' @examples
    #' shiny::isolate(Reporter$new()$get_reactive_add_card())
    get_reactive_add_card = function() {
      private$reactive_add_card()
    },
    #' @description get metadata of this `Reporter`.
    #'
    #' @return metadata
    #' @examples
    #' reporter <- Reporter$new()$append_metadata(list(sth = "sth"))
    #' reporter$get_metadata()
    #'
    get_metadata = function() {
      private$metadata
    },
    #' @description Appends metadata to this `Reporter`.
    #'
    #' @param meta (`list`) of metadata.
    #' @return invisibly self
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
    #' @description Create/Recreate a Reporter from another Reporter
    #' @param reporter `Reporter` instance.
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
