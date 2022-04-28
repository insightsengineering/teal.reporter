#' @title `Reporter`
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
      invisible(self)
    },
    #' @description Appends a table to this `Reporter`.
    #'
    #' @param cards `ReportCard` or a list of such objects
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
      invisible(self)
    },
    #' @description Returns cards of this `Reporter`.
    #'
    #' @return `list()` list of `ReportCard`
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
    #' @description Returns blocks of all `ReportCard` of this `Reporter`.
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
    }
  ),
  private = list(
    cards = list(),
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
