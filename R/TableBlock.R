#' @title `TableBlock`
#' @keywords internal
#'
TableBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "TableBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Returns a new `TableBlock` object
    #'
    #' @param table (`data.frame`, `DT`, `rtables`, `TableTree`) a table assigned to this `TableBlock`
    #' @return a `TableBlock` object
    #'
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
    #' @param content (`data.frame`, `DT`, `rtables`) a table assigned to this `TableBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$set_content(iris)
    #'
    set_content = function(content) {
      checkmate::assert_multi_class(content, private$supported_tables)
      path <- tempfile(fileext = ".rds")
      saveRDS(content, file = path)
      super$set_content(path)
      invisible(self)
    },
    #' @description finalize of this `TableBlock`.
    #'
    #' @details Removes the temporary file created in the constructor.
    #'
    finalize = function() {
      try(unlink(super$get_content()))
    }
  ),
  private = list(
    supported_tables = c("data.frame", "rtables", "TableTree")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
