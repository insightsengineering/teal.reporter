#' @title `FileBlock`
#' @keywords internal
#'
FileBlock <- R6::R6Class( # nolint: object_name_linter.
  classname = "FileBlock",
  inherit = ContentBlock,
  public = list(
    #' @description finalize of this `FileBlock`.
    #'
    #' @details Removes the temporary file created in the constructor.
    #'
    finalize = function() {
      try(unlink(super$get_content()))
    },
    #' @description Create the `FileBlock` from a list.
    #' The list should contain one named field, `"basename"`.
    #' @param x `named list` with one field `"basename"`, a name of the file.
    #' @param output_dir `character` with a path to the directory where a file will be copied.
    #' @return invisibly self
    #' @examples
    #' block <- teal.reporter:::FileBlock$new()
    #' file_path <- tempfile(fileext = ".png")
    #' saveRDS(iris, file_path)
    #' block$from_list(list(basename = basename(file_path)), dirname(file_path))
    from_list = function(x, output_dir) {
      checkmate::assert_list(x)
      checkmate::assert_names(names(x), must.include = "basename")
      path <- file.path(output_dir, x$basename)
      file_type <- paste0(".", tools::file_ext(path))
      checkmate::assert_file_exists(path, extension = file_type)
      new_file_path <- tempfile(fileext = file_type)
      file.copy(path, new_file_path)
      super$set_content(new_file_path)
      invisible(self)
    },
    #' @description Convert the `FileBlock` to a list.
    #' @param output_dir `character` with a path to the directory where a file will be copied.
    #' @return `named list` with a `basename` of the file.
    #' @examples
    #' block <- teal.reporter:::FileBlock$new()
    #' block$to_list(tempdir())
    to_list = function(output_dir) {
      base_name <- basename(super$get_content())
      file.copy(super$get_content(), file.path(output_dir, base_name))
      list(basename = base_name)
    }
  ),
  lock_objects = TRUE,
  lock_class = TRUE
)
