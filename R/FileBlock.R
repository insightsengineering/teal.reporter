#' @title `FileBlock`
#' @keywords internal
#'
FileBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "FileBlock",
  inherit = ContentBlock,
  public = list(
    #' @description Create the `TableBlock` from a list.
    #' The list should contain one named field, `"path"`.
    #' @param x `named list` with one field `"path"`, a path to the `RDS` file.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only `x` is used. Default `NULL`.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::FileBlock$new()
    #' file_path <- tempfile(fileext = ".png")
    #' saveRDS(iris, file_path)
    #' block$from_list(list(path = file_path))
    from_list = function(x, base_path = NULL) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = "path")
      path <- if (!is.null(base_path)) {
        file.path(base_path, basename(x$path))
      } else {
        x$path
      }
      checkmate::assert_file_exists(path)
      super$set_content(path)
      invisible(self)
    },
    #' @description Convert the `FileBlock` to a list.
    #' @param base_path `character` with a path to the dir with the file.
    #' If the argument is `NULL` then only basename is returned. Default `.`.
    #' @return `named list` with a path to the file.
    #' @examples
    #' block <- teal.reporter:::FileBlock$new()
    #' block$to_list()
    to_list = function(base_path = ".") {
      list(path = file.path(base_path, basename(super$get_content())))
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
