#' @title `TableBlock`
#' @docType class
#' @description
#' Specialized `FileBlock` for managing table content in reports.
#' It's designed to handle various table formats, converting them into a consistent,
#' document-ready format (e.g., `flextable`) for inclusion in reports.
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
    #' @details Raises error if argument is not a table-like object.
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
      # Store whether this table is too wide for portrait orientation
      private$landscape_mode <- is_table_too_wide(content)
      path <- tempfile(fileext = ".rds")
      saveRDS(content, file = path)
      super$set_content(path)
      invisible(self)
    },
    #' @description Get landscape mode flag
    #'
    #' @return `logical` indicating if table should be rendered in landscape mode
    #'
    get_landscape_mode = function() {
      if (is.null(private$landscape_mode)) {
        return(FALSE)
      }
      private$landscape_mode
    },
    #' @description Convert the `TableBlock` to a list.
    #'
    #' @param output_dir (`character`) with a path to the directory where a file will be copied.
    #'
    #' @return `named list` with a `basename` of the file and landscape mode flag.
    #'
    to_list = function(output_dir) {
      result <- super$to_list(output_dir)
      result$landscape_mode <- self$get_landscape_mode()
      result
    },
    #' @description Create the `TableBlock` from a list.
    #'
    #' @param x (`named list`) with fields `"basename"` and optionally `"landscape_mode"`.
    #' @param output_dir (`character`) with a path to the directory where a file will be copied.
    #'
    #' @return `self`, invisibly.
    #'
    from_list = function(x, output_dir) {
      super$from_list(x, output_dir)
      # Restore landscape mode flag if present
      if (!is.null(x$landscape_mode)) {
        private$landscape_mode <- x$landscape_mode
      } else {
        private$landscape_mode <- FALSE
      }
      invisible(self)
    }
  ),
  private = list(
    supported_tables = c("data.frame", "rtables", "TableTree", "ElementaryTable", "listing_df"),
    landscape_mode = NULL
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
