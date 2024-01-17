#' @title `TableBlock`
#' @keywords internal
TableBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TableBlock",
  inherit = FileBlock,
  public = list(
    #' @description Returns a new `TableBlock` object
    #'
    #' @param table (`data.frame`, `rtables`, `TableTree`, `ElementaryTable`, `listing_df`) a table assigned to
    #'   this `TableBlock`
    #'
    #' @return a `TableBlock` object
    initialize = function(table) {
      if (!missing(table)) {
        self$set_content(table)
      }
      invisible(self)
    },
    #' @description Sets content of this `TableBlock`.
    #'
    #' @details throws if argument is not a table-like object.
    #'
    #' @param content (`data.frame`, `rtables`, `TableTree`, `ElementaryTable`, `listing_df`) a table assigned to
    #'   this `TableBlock`
    #'
    #' @return invisibly self
    #' @examples
    #' table_block <- getFromNamespace("TableBlock", "teal.reporter")
    #' block <- table_block$new()
    #' block$set_content(iris)
    #'
    set_content = function(content) {
      checkmate::assert_multi_class(content, private$supported_tables)
      content <- to_flextable(content)
      path <- tempfile(fileext = ".rds")
      saveRDS(content, file = path)
      super$set_content(path)
      invisible(self)
    }
  ),
  private = list(
    supported_tables = c("data.frame", "rtables", "TableTree", "ElementaryTable", "listing_df")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
