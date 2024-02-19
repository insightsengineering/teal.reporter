#' @title `TableBlock`
#' @docType class
#' @description
#' Specialized `FileBlock` for managing table content in reports.
#' It's designed to handl various table formats, converting them into a consistent,
#' document-ready format (e.g., FlexTable) for inclusion in reports.
#'
#' @keywords internal
TableBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TableBlock",
  inherit = FileBlock,
  public = list(
    #' @description Initialize a `TableBlock` object.
    #'
    #' @param table (`data.frame` or `rtables` or `TableTree` or `ElementaryTable` or `listing_df`) a table assigned to
    #'   this `TableBlock`
    #'
    #' @return Object of class `TableBlock`, invisibly.
    initialize = function(table) {
      if (!missing(table)) {
        self$set_content(table)
      }
      invisible(self)
    },
    #' @description Sets content of this `TableBlock`.
    #'
    #' @details Throws if argument is not a table-like object.
    #'
    #' @param content (`data.frame` or `rtables` or `TableTree` or `ElementaryTable` or `listing_df`)
    #' a table assigned to this `TableBlock`
    #'
    #' @return `self`, invisibly.
    #' @examples
    #' TableBlock <- getFromNamespace("TableBlock", "teal.reporter")
    #' block <- TableBlock$new()
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
