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
    #' @param content (`data.frame`, `rtables`) a table assigned to this `TableBlock`
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$set_content(iris)
    #'
    set_content = function(content) {
      is.character(content)
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
    },
    #' @description Create the `TableBlock` from a list.
    #' The list should contain one named field, `"path"`.
    #' @param x `named list` with one field `"path"`, a path to the `RDS` file.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only `x` is used. Default `NULL`.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$from_list(list(path = "file.RDS"))
    from_list = function(x, base_path = NULL) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = "path")
      path <- if (!is.null(base_path)) {
        file.path(base_path, basename(x$path))
      } else {
        basename(x$path)
      }
      super$set_content(path)
      invisible(self)
    },
    #' @description Convert the `TableBlock` to a list.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only basename is returned. Default `.`.
    #' @return `named list` with a path to the file.
    #' @examples
    #' block <- teal.reporter:::TableBlock$new()
    #' block$set_content(iris)
    #' block$to_list()
    #' block$to_list("/path/sth")
    to_list = function(base_path = ".") {
      path <- if (!is.null(base_path)) {
        file.path(base_path, basename(super$get_content()))
      } else {
        basename(super$get_content())
      }
      list(path = path)
    }
  ),
  private = list(
    supported_tables = c("data.frame", "rtables", "TableTree")
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
